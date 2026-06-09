

<a id="TITLE:EXPANDERS-DOCS:TAG1"></a>
# Expanders

Welcome to Expanders\!\! \:D

* [What is this\?](/README.md#TITLE:EXPANDERS-DOCS:TAG2)
* [Why\?](/README.md#TITLE:EXPANDERS-DOCS:TAG3)
* [Reference](/README.md#TITLE:EXPANDERS-DOCS:TAG4)


<a id="TITLE:EXPANDERS-DOCS:TAG2"></a>
## What is this\?

This project lets you define ```expanders```\. An ```expander``` is like a namespace for forms that can be expanded by a macro\.

It easier to understand with an example\. Suppose we have the following form ```(op (+ 3 4) b)```\. This form will be used in two different macros named ```plus-macro``` and ```minus-macro```\. Each macro receives a form\. That form could start with ```op```\. In that case\, ```op``` is substituted by ```+``` or ```-``` respectively\.

Let\'s define 2 different expanders\:

`````common-lisp
;; We assume (use-package #:expanders)
(defexpander plus-expander)
(defexpander minus-expander)
`````
`````common-lisp
;; Returns
MINUS-EXPANDER
`````

Now we have two different expanders\. The function [exp\:expanderp](/README.md#FUNCTION:EXPANDERS:EXPANDERP) can tell us if a symbol denotes an expander\:

`````common-lisp
(expanderp 'plus-expander)
`````
`````common-lisp
;; Returns
T
`````

`````common-lisp
(expanderp 'hey)
`````
`````common-lisp
;; Returns
NIL
`````

`````common-lisp
(expanderp 'minus-expander)
`````
`````common-lisp
;; Returns
T
`````

Now it is time to define the expansion ```op``` for each expander using [exp\:defexpansion](/README.md#FUNCTION:EXPANDERS:DEFEXPANSION)\:

`````common-lisp
(defexpansion plus-expander op (a b)
  "OP to + expansion"
  `(+ ,a ,b))

(defexpansion minus-expander op (a b)
  "OP to - expansion"
  `(- ,a ,b))
`````
`````common-lisp
;; Returns
OP
`````

We can check if a symbol is an expansion for a given expander using [exp\:expansionp](/README.md#FUNCTION:EXPANDERS:EXPANSIONP)\:

`````common-lisp
(expansionp 'plus-expander 'op)
`````
`````common-lisp
;; Returns
T
`````

`````common-lisp
(expansionp 'plus-expander 'hey)
`````
`````common-lisp
;; Returns
NIL
`````

`````common-lisp
(expansionp 'minus-expander 'op)
`````
`````common-lisp
;; Returns
T
`````

Also\, we can retrieve or set the docstring using ```documentation```\:

`````common-lisp
(documentation 'op 'plus-expander)
`````
`````common-lisp
;; Returns
"OP to + expansion"
`````

`````common-lisp
(let ((old-docstring (documentation 'op 'minus-expander)))
  (setf (documentation 'op 'minus-expander) "Another docstring")
  (let ((new-docstring (documentation 'op 'minus-expander)))
    (format t "Old: ~s~%New: ~s" old-docstring new-docstring)))
`````
`````text
;; Output
Old: "OP to - expansion"
New: "Another docstring"
`````
`````common-lisp
;; Returns
NIL
`````

We can expand an expansion using [exp\:expand](/README.md#FUNCTION:EXPANDERS:EXPAND)\:

`````common-lisp
(expand 'plus-expander 'op 3 '(+ 5 6))
`````
`````common-lisp
;; Returns
(+ 3 (+ 5 6))
`````

`````common-lisp
(expand 'minus-expander 'op 3 '(+ 5 6))
`````
`````common-lisp
;; Returns
(- 3 (+ 5 6))
`````

But usually is more convenient to use the [exp\:expand\*](/README.md#FUNCTION:EXPANDERS:EXPAND*) function\:

`````common-lisp
(expand* 'plus-expander 'op '(3 (+ 5 6)))
`````
`````common-lisp
;; Returns
(+ 3 (+ 5 6))
`````

or\:

`````common-lisp
(expand* 'plus-expander '(op 3 (+ 5 6)))
`````
`````common-lisp
;; Returns
(+ 3 (+ 5 6))
`````

Finally\, let\'s define the macros ```plus-macro``` and ```minus-macro```\:

`````common-lisp
(defmacro plus-macro (form)
  (if (and (consp form)
           (expansionp 'plus-expander (car form)))
      (expand* 'plus-expander form)
      form))
`````
`````common-lisp
;; Returns
PLUS-MACRO
`````

`````common-lisp
(defmacro minus-macro (form)
  (if (and (consp form)
           (expansionp 'minus-expander (car form)))
      (expand* 'minus-expander form)
      form))
`````
`````common-lisp
;; Returns
MINUS-MACRO
`````

If we use the form ```(op 5 4)``` we will see that each macro will expand to ```(+ 5 4)``` or ```(- 5 4)``` respectively\.

`````common-lisp
(plus-macro (op 5 4))
`````
`````common-lisp
;; Returns
9
`````

`````common-lisp
(minus-macro (op 5 4))
`````
`````common-lisp
;; Returns
1
`````

<a id="TITLE:EXPANDERS-DOCS:TAG3"></a>
## Why\?

* **It is common**\: I have noticed that having expanders is a relatively common pattern in macros\. The best example is ```setf``` and its ```setf-expanders```\. Another project using expanders is [CFFI](https://github.com/cffi/cffi) and its type parsers\. In my own projects I ended up using the same techniques \([Clith](https://github.com/Hectarea1996/clith)\)\.
* **Duality of syntax**\: We can increase the duality of syntax using expanders\. The best example is ```setf```\. Thanks to ```setf``` we don\'t need names for setters because they come for free from getters\.


<a id="TITLE:EXPANDERS-DOCS:TAG4"></a>
## Reference

<a id="FUNCTION:EXPANDERS:DEFEXPANDER"></a>
<a id="FUNCTION:EXPANDERS-DOCS:TAG7"></a>
#### Macro: exp\:defexpander \(sym\)

`````text
Define an expander represented by the symbol SYM.
If used at top level the expander will be defined at compile time.
`````

<a id="FUNCTION:EXPANDERS:DEFEXPANSION"></a>
<a id="FUNCTION:EXPANDERS-DOCS:TAG5"></a>
#### Macro: exp\:defexpansion \(expander name \(\&rest args\) \&body body\)

`````text
Define an expansion for the expander EXPANDER. If used at top level the expansion will be defined at
compile time. NAME must be a symbol denoting the new expansion. ARGS is a destructuring lambda list.
The &whole argument can be supplied to bind a list with all the arguments.
DEFEXPANSION must return the desired expansion for NAME and EXPANDER.
`````

<a id="FUNCTION:EXPANDERS:EXPAND"></a>
<a id="FUNCTION:EXPANDERS-DOCS:TAG8"></a>
#### Function: exp\:expand \(expander expansion \&rest args\)

`````text
Expand an EXPANSION from EXPANDER.
`````

<a id="FUNCTION:EXPANDERS:EXPAND*"></a>
<a id="FUNCTION:EXPANDERS-DOCS:TAG9"></a>
#### Function: exp\:expand\* \(expander \&rest args\)

`````text
Expand an expansion from EXPANDER. The first argument from ARGS must be a valid expansion.
The last argument can be a symbol denoting the expansion (no arguments),
or a list with the last arguments to use in the expansion.
  Examples:
    (expand* 'my-expander 'my-expansion)   ; No arguments
    (expand* 'my-expander (list 'my-expansion arg1 arg2 ...))
    (expand* 'my-expander 'my-expansion arg1 arg2 (list arg3 arg4 ...))
`````

<a id="FUNCTION:EXPANDERS:EXPANDERP"></a>
<a id="FUNCTION:EXPANDERS-DOCS:TAG10"></a>
#### Function: exp\:expanderp \(sym\)

`````text
Check if a symbol denotes an expander.
`````

<a id="FUNCTION:EXPANDERS:EXPANSIONP"></a>
<a id="FUNCTION:EXPANDERS-DOCS:TAG6"></a>
#### Function: exp\:expansionp \(expander expansion\)

`````text
Check if EXPANSION is a valid expansion for EXPANDER.
`````