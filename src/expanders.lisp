
(in-package #:expanders)

(defun first-duplicate (list &key (test #'eql))
  "Get the first duplicate of a list or NIL"
  (let ((seen (make-hash-table :test test)))
    (loop for item in list
          if (gethash item seen)
            return item
          do (setf (gethash item seen) t))))

(defun check-lambda-list-variables (lambda-obj lambda-list)
  "Check lambda list variables correctness"
  (when-let ((duplicate (first-duplicate (ecclesia:lambda-list-variables lambda-obj))))
    (error "The variable ~s occurs more than once in the lambda list:~%~s" duplicate lambda-list)))

(defun destructure-macro-lambda-list (lambda-list var &optional name env)
  "Return the code to destructure a macro lambda list"
  (let* ((lambda-obj (ecclesia:parse-macro-lambda-list lambda-list))
         (whole-var (ecclesia:whole lambda-obj))
         (env-var (ecclesia:environment lambda-obj)))
    (check-lambda-list-variables lambda-obj lambda-list)
    (setf (ecclesia:whole lambda-obj) :none)
    (multiple-value-bind (bindings ignoring) (ecclesia:destructure-lambda-list lambda-obj var)
      (unless (eq env-var :none)
        (push (list env-var env) bindings))
      (unless (eq whole-var :none)
        (push (list whole-var (if name `(cons ',name ,var) var)) bindings))
      (values bindings ignoring))))

(defclass expander ()
  ((expansions :initform (make-vault) :reader expander-expansions)))

(defun make-expander ()
  "Make an expander"
  (make-instance 'expander))

(defun expanderp (obj)
  "Check if an object is an expander"
  (typep obj 'expander))

(defun expansionp (expander name)
  "Retrieve the expansion function named NAME from EXPANDER. Return NIL if that function does not exist."
  (check-type expander expander)
  (check-type name symbol)
  (and (vault-function (expander-expansions expander) name) t))

(defmacro defexpansion (expander name macro-lambda-list &body body)
  "Define an expansion named NAME for EXPANDER. Arguments are specified in a macro-lambda-list where
&whole and &environment can be used."
  (with-gensyms (form-sym env-sym)
    (multiple-value-bind (bindings ignoring)
        (destructure-macro-lambda-list macro-lambda-list form-sym name env-sym)
      (multiple-value-bind (actual-body declarations docstring)
          (parse-body body :documentation t)
        `(vault-defun (expander-expansions ,expander) ,name (,form-sym &optional ,env-sym)
           (declare (ignorable ,env-sym))
           ,@(when docstring `(,docstring))
           (let* ,bindings
             (declare (ignore ,@ignoring))
             ,@(when declarations `(,@declarations))
             ,@actual-body))))))


(defun expand (expander expr &optional env)
  "Expand an EXPANSION from EXPANDER. An environment object can be supplied."
  (check-type expander expander)
  (check-type expr list)
  (let ((expansion (car expr))
        (args (cdr expr)))
    (assert (expansionp expander expansion) (expansion)
            "~s is not a valid expansion for the given expander" expansion)
    (vault-funcall (expander-expansions expander) expansion args env)))

(defmethod documentation ((object symbol) (doc-type expander))
  (documentation object (expander-expansions doc-type)))

(defmethod (setf documentation) (new-value (object symbol) (doc-type expander))
  (setf (documentation object (expander-expansions doc-type)) new-value))
