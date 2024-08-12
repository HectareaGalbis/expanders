
(in-package #:expanders)

(defconstant +expander-prop+ 'expander)

(defstruct expander-info
  docstring
  func)

(defun expanderp (sym)
  "Check if a symbol denotes an expander."
  (check-type sym symbol)
  (and (get sym +expander-prop+) t))

(defun expander-has-symbol-p (expander sym)
  (check-type expander symbol)
  (assert (expanderp expander) (expander) "~s is not a valid expander." expander)
  (check-type sym symbol)
  (let* ((default (gensym "DEFAULT"))
         (value (get sym (get expander +expander-prop+) default)))
    (not (eq value default))))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun extract-docstring (body)
    "Returns the docstring and the body without that docstring."
    (loop for (expr . rest-body) on body
          if (and (listp expr) (eq (car expr) 'declare))
            collect expr into declarations
          else if (stringp expr)
                 do (return-from extract-docstring (values expr (append declarations rest-body)))
          else
            do (return-from extract-docstring (values nil (append declarations (list expr) rest-body))))))

(defmacro defexpander (sym definer predicate)
  "Defines an expander represented by the symbol SYM. Defines also a definer and a predicate.
The DEFINER will be like DEFMACRO. It receives a name and a destructuring-lambda-list. The definition
must return the expansion of the provided name.
The PREDICATE will receive a symbol adn will return true if the symbol was used to define an expansion with
the DEFINER."
  (check-type sym symbol)
  (check-type definer symbol)
  (check-type predicate symbol)
  (with-gensyms (def-name def-args def-body docstring sym-obj doc-type actual-body pred-sym func expander-info
                  pre-args)
    `(progn
       (setf (get ',sym +expander-prop+) (gensym ,(symbol-name sym)))

       (defmethod (setf documentation) (,docstring ,sym-obj (,doc-type (eql ',sym)))
         (declare (ignore ,doc-type))
         (print ,sym-obj)
         (print (get ',sym +expander-prop+))
         (print (get ,sym-obj (get ',sym +expander-prop+)))
         (setf (expander-info-docstring (get ,sym-obj (get ',sym +expander-prop+))) ,docstring))

       (defmethod documentation (,sym-obj (,doc-type (eql ',sym)))
         (declare (ignore ,doc-type))
         (expander-info-docstring (get ,sym-obj (get ',sym +expander-prop+))))

       (defmacro ,definer (,def-name (&rest ,def-args) &body ,def-body)
         (multiple-value-bind (,docstring ,actual-body) (extract-docstring ,def-body)
           (with-gensyms (,func ,expander-info ,pre-args)
             `(eval-when (:compile-toplevel :load-toplevel :execute)
                (flet ((,,func (&rest ,,pre-args)
                         (destructuring-bind (,@,def-args) ,,pre-args
                           ,@,actual-body)))
                  (let ((,,expander-info (make-expander-info :func #',,func)))
                    (setf (get ',,def-name (get ',',sym +expander-prop+)) ,,expander-info)
                    ,@(when ,docstring
                        `((setf (documentation ',,def-name ',',sym) ,,docstring)))
                    ',,def-name))))))
       
       (defun ,predicate (,pred-sym)
         (expander-has-symbol-p ',sym ,pred-sym))

       ',sym)))



(defun expand (expander form)
  "Expands a form using an expander."
  (assert (expanderp expander) (expander) "~s is not a valid expander." expander)
  (check-type form cons)
  (let ((sym (car form)))
    (check-type sym symbol)
    (assert (expander-has-symbol-p expander sym) (sym)
            "The form ~s does not belong to the expander ~s." sym expander)
    (let* ((args (cdr form))
           (expander-info (get sym (get expander +expander-prop+))))
      (apply (expander-info-func expander-info) args))))
