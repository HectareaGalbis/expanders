
(in-package #:expanders)

(defconstant +expander-prop+ 'expander)

(defmacro defexpander (sym)
  "Define an expander represented by the symbol SYM.
If used at top level the expander will be defined at compile time."
  (check-type sym symbol)
  (with-gensyms (docstring sym-obj doc-type)
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (setf (get ',sym +expander-prop+) (gensym ,(symbol-name sym)))

       (defmethod (setf documentation) (,docstring ,sym-obj (,doc-type (eql ',sym)))
         (declare (ignore ,doc-type))
         (setf (documentation (get ,sym-obj (get ',sym +expander-prop+)) 'function) ,docstring))

       (defmethod documentation (,sym-obj (,doc-type (eql ',sym)))
         (declare (ignore ,doc-type))
         (documentation (get ,sym-obj (get ',sym +expander-prop+)) 'function))

       ',sym)))

(defun expanderp (sym)
  "Check if a symbol denotes an expander."
  (check-type sym symbol)
  (and (get sym +expander-prop+) t))

(defmacro defexpansion (expander name (&rest args) &body body)
  "Define an expansion for the expander EXPANDER. If used at top level the expansion will be defined at
compile time. NAME must be a symbol denoting the new expansion. ARGS is a destructuring lambda list.
The &whole argument can be supplied to bind a list with all the arguments.
DEFEXPANSION must return the desired expansion for NAME and EXPANDER."
  (assert (expanderp expander))
  (check-type name symbol)
  (multiple-value-bind (actual-body declarations docstring) (parse-body body :documentation t)
    (with-gensyms (pre-args-sym)
      `(eval-when (:compile-toplevel :load-toplevel :execute)
         (setf (get ',name (get ',expander +expander-prop+))
               (lambda (&rest ,pre-args-sym)
                 ,@(when docstring `(,docstring))
                 (destructuring-bind (,@args) ,pre-args-sym
                   ,@declarations
                   ,@actual-body)))
         ',name))))

(defun expansionp (expander expansion)
  "Check if EXPANSION is a valid expansion for EXPANDER."
  (check-type expander symbol)
  (assert (expanderp expander) (expander) "~s is not a valid expander." expander)
  (check-type expansion symbol)
  (let* ((default (gensym "DEFAULT"))
         (value (get expansion (get expander +expander-prop+) default)))
    (not (eq value default))))


(defun expand (expander expansion &rest args)
  "Expand an EXPANSION from EXPANDER."
  (assert (expanderp expander) (expander) "~s is not a valid expander." expander)
  (assert (expansionp expander expansion) (expansion) "~s is not a valid expansion for the expader ~s" expansion expander)
  (apply (get expansion (get expander +expander-prop+)) args))

(defun expand* (expander &rest args)
  "Expand an expansion from EXPANDER. The first argument from ARGS must be a valid expansion.
The last argument can be a symbol denoting the expansion (no arguments),
or a list with the last arguments to use in the expansion.
  Examples:
    (expand* 'my-expander 'my-expansion)   ; No arguments
    (expand* 'my-expander (list 'my-expansion arg1 arg2 ...))
    (expand* 'my-expander 'my-expansion arg1 arg2 (list arg3 arg4 ...))"
  (when (null (cdr args))
    (setf args (cons (ensure-list (car args)) (cdr args))))
  (apply #'expand expander (apply #'list* args)))
