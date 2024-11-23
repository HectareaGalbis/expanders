
(in-package #:expanders-docs)

@select-output-file["/README.md"]

@header{Expanders}

Welcome to Expanders!! :D

@table-of-contents{}

@subheader{What is this?}

The project lets you define @code{expanders}. An @code{expander} is like a namespace for forms that should transform in a specific way.

It easier to understand with an example. Suppose we have the following form @code{(op (+ 3 4) b)}. This form will be used in two different macros named @code{plus-macro} and @code{minus-macro}. Each macro receives a form. That form could start with @code{op}. In that case, @code{op} is substituted by @code{+} or @code{-} respectively.

Let's define 2 different expanders:

@example{
;; We assume (use-package #:expanders)
(defexpander plus-expander)
(defexpander minus-expander)
}

Now we have two different expanders. The function @fref[expanderp] can tell us if a symbol denotes an expander:

@example{
(expanderp 'plus-expander)
}

@example{
(expanderp 'hey)
}

@example{
(expanderp 'minus-expander)
}

Now it is time to define the expansion @code{op} for each expander using @fref[defexpansion]:

@example{
(defexpansion plus-expander op (a b)
  "OP to + expansion"
  `(+ ,a ,b))

(defexpansion minus-expander op (a b)
  "OP to - expansion"
  `(- ,a ,b))
}

We can check if a symbol is an expansion for a given expander using @fref[expansionp]:

@example{
(expansionp 'plus-expander 'op)
}

@example{
(expansionp 'plus-expander 'hey)
}

@example{
(expansionp 'minus-expander 'op)
}

Also, we can retrieve or set the docstring using @code{documentation}:

@example{
(documentation 'op 'plus-expander)
}

@example{
(let ((old-docstring (documentation 'op 'minus-expander)))
  (setf (documentation 'op 'minus-expander) "Another docstring")
  (let ((new-docstring (documentation 'op 'minus-expander)))
    (format t "Old: ~s~%New: ~s" old-docstring new-docstring)))
}

We can expand a form using @fref[expand]. Note that the form must be a list starting with the name of an expansion:

@example{
(expand 'plus-expander '(op 3 (+ 5 6)))
}

@example{
(expand 'minus-expander '(op 3 (+ 5 6)))
}

Finally, let's define the macros @code{plus-macro} and @code{minus-macro}:

@example{
(defmacro plus-macro (form)
  (if (and (consp form)
           (expansionp 'plus-expander (car form)))
      (expand 'plus-expander form)
      form))
}

@example{
(defmacro minus-macro (form)
  (if (and (consp form)
           (expansionp 'minus-expander (car form)))
      (expand 'minus-expander form)
      form))
}

If we use the form @code{(op 5 4)} we will see that each macro will expand to @code{(+ 5 4)} or @code{(- 5 4)} respectively.

@example{
(plus-macro (op 5 4))
}

@example{
(minus-macro (op 5 4))
}

@subheader{Why?}

@itemize[

@item{@bold{It is common}: I have noticed that having expanders is a relatively common pattern in macros. The best example is @code{setf} and its @code{setf-expanders}. Another project using expanders is @link[:address "https://github.com/cffi/cffi"]{CFFI} and its type parsers. In my own projects I ended up using the same techniques (@link[:address "https://github.com/Hectarea1996/clith"]{Clith}).}

@item{@bold{Duality of syntax}: We can increase the duality of syntax using expanders. The best example is @code{setf}. Thanks to @code{setf} we don't need names for setters because they come for free with the getter.}

]

@subheader{Reference}

@function-glossary[#:expanders]
