
(in-package #:expanders)

(defconstant +expander-prop+ 'expander)

(defstruct expander-info
  docstring
  func)

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

(defmacro defexpander (sym)
  "Defines an expander represented by the symbol SYM."
  (check-type sym symbol)
  (with-gensyms (docstring sym-obj doc-type)
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (setf (get ',sym +expander-prop+) (gensym ,(symbol-name sym)))

       (defmethod (setf documentation) (,docstring ,sym-obj (,doc-type (eql ',sym)))
         (declare (ignore ,doc-type))
         (setf (expander-info-docstring (get ,sym-obj (get ',sym +expander-prop+))) ,docstring))

       (defmethod documentation (,sym-obj (,doc-type (eql ',sym)))
         (declare (ignore ,doc-type))
         (expander-info-docstring (get ,sym-obj (get ',sym +expander-prop+))))

       ',sym)))

(defun expanderp (sym)
  "Check if a symbol denotes an expander."
  (check-type sym symbol)
  (and (get sym +expander-prop+) t))

(defmacro defexpansion (expander name (&rest args) &body body)
  "Defines an expansion for the expander EXPANDER. NAME must be a symbol denoting
the new expansion. ARGS is a destructuring lambda list. This must return the desired
expansion for NAME and EXPANDER."
  (assert (expanderp expander))
  (check-type name symbol)
  (multiple-value-bind (docstring actual-body) (extract-docstring body)
    (with-gensyms (func-sym expander-info-sym pre-args-sym)
      `(eval-when (:compile-toplevel :load-toplevel :execute)
         (flet ((,func-sym (&rest ,pre-args-sym)
                  (destructuring-bind (,@args) ,pre-args-sym
                    ,@actual-body)))
           (let ((,expander-info-sym (make-expander-info :func #',func-sym)))
             (setf (get ',name (get ',expander +expander-prop+)) ,expander-info-sym)
             ,@(when docstring
                 `((setf (documentation ',name ',expander) ,docstring)))
             ',name))))))

(defun expansionp (expander expansion)
  "Checks if EXPANSION is a valid expansion for the expander EXPANDER.
EXPANDER must be a valid expander."
  (check-type expander symbol)
  (assert (expanderp expander) (expander) "~s is not a valid expander." expander)
  (check-type expansion symbol)
  (let* ((default (gensym "DEFAULT"))
         (value (get expansion (get expander +expander-prop+) default)))
    (not (eq value default))))


(defun expand (expander expansion &rest args)
  "Expands an expansion."
  (assert (expanderp expander) (expander) "~s is not a valid expander." expander)
  (assert (expansionp expander expansion) (expansion) "~s is not a valid expansion for the expader ~s" expansion expander)
  (let* ((expander-info (get expansion (get expander +expander-prop+))))
    (apply (expander-info-func expander-info) args)))

(defun expand* (expander &rest args)
  "Expands an expansion. The last argument can be a symbol denoting the expansion (no arguments),
or a list with the last arguments to use in the expansion.
  Examples:
    (expand* 'my-expander 'my-expansion)   ; No arguments
    (expand* 'my-expander (list 'my-expansion arg1 arg2 ...))
    (expand* 'my-expander 'my-expansion arg1 arg2 (list arg3 arg4 ...))"
  (when (null (cdr args))
    (setf args (cons (ensure-list (car args)) (cdr args))))
  (apply #'expand expander (apply #'list* args)))
