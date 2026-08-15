

<a id="TITLE:EXPANDERS-DOCS:TAG1"></a>
# Expanders

Welcome to Expanders\!\! \:D

* [What is this\?](/README.md#TITLE:EXPANDERS-DOCS:TAG2)
* [Why\?](/README.md#TITLE:EXPANDERS-DOCS:TAG3)
* [Reference](/README.md#TITLE:EXPANDERS-DOCS:TAG4)


<a id="TITLE:EXPANDERS-DOCS:TAG2"></a>
## What is this\?

This project lets you define ```expanders```\. An ```expander``` is like a namespace for forms that can be expanded by a macro\.

It is easier to understand with an example\. Suppose we have the following form ```(op (+ 3 4) b)```\. This form will be used in two different macros named ```plus-macro``` and ```minus-macro```\. Each macro receives a form\. That form could start with ```op```\. In that case\, ```op``` is substituted by ```+``` or ```-``` respectively\.

Let\'s define 2 different expanders\:

`````common-lisp
;; We assume (use-package #:expanders)
(defvar plus-expander (make-expander))
(defvar minus-expander (make-expander))
`````
`````common-lisp
;; Returns
MINUS-EXPANDER
`````

Now we have two different expanders\. The function [exp\:expanderp](/README.md#FUNCTION:EXPANDERS:EXPANDERP) can tell us if a symbol denotes an expander\:

`````common-lisp
(expanderp plus-expander)
`````
`````common-lisp
;; Returns
T
`````

`````common-lisp
(expanderp 'another-thing)
`````
`````common-lisp
;; Returns
NIL
`````

`````common-lisp
(expanderp minus-expander)
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
(expansionp plus-expander 'op)
`````
`````common-lisp
;; Returns
T
`````

`````common-lisp
(expansionp plus-expander 'hey)
`````
`````common-lisp
;; Returns
NIL
`````

`````common-lisp
(expansionp minus-expander 'op)
`````
`````common-lisp
;; Returns
T
`````

Also\, we can retrieve or set the docstring using ```documentation```\:

`````common-lisp
(documentation 'op plus-expander)
`````
`````common-lisp
;; Returns
"OP to + expansion"
`````

`````common-lisp
(let ((old-docstring (documentation 'op minus-expander)))
  (setf (documentation 'op minus-expander) "Another docstring")
  (let ((new-docstring (documentation 'op minus-expander)))
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
(expand plus-expander '(op 3 (+ 5 6)))
`````
`````common-lisp
;; Returns
(+ 3 (+ 5 6))
`````

`````common-lisp
(expand minus-expander '(op 3 (+ 5 6)))
`````
`````common-lisp
;; Returns
(- 3 (+ 5 6))
`````

Finally\, let\'s define the macros ```plus-macro``` and ```minus-macro```\:

`````common-lisp
(defmacro plus-macro (form)
  (expand plus-expander form))
`````
`````common-lisp
;; Returns
PLUS-MACRO
`````

`````common-lisp
(defmacro minus-macro (form)
  (expand minus-expander form))
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

<a id="FUNCTION:EXPANDERS:DEFEXPANSION"></a>
<a id="FUNCTION:EXPANDERS-DOCS:TAG9"></a>
#### Macro: exp\:defexpansion \(expander name macro\-lambda\-list \&body body\)

`````text
Define an expansion named NAME for EXPANDER. Arguments are specified in a macro-lambda-list where
&whole and &environment can be used.
`````

<a id="FUNCTION:EXPANDERS:EXPAND"></a>
<a id="FUNCTION:EXPANDERS-DOCS:TAG7"></a>
#### Function: exp\:expand \(expander expr \&optional env\)

`````text
Expand an EXPANSION from EXPANDER. An environment object can be supplied.
`````

<a id="FUNCTION:EXPANDERS:EXPANDERP"></a>
<a id="FUNCTION:EXPANDERS-DOCS:TAG8"></a>
#### Function: exp\:expanderp \(obj\)

`````text
Check if an object is an expander
`````

<a id="FUNCTION:EXPANDERS:EXPANSIONP"></a>
<a id="FUNCTION:EXPANDERS-DOCS:TAG6"></a>
#### Function: exp\:expansionp \(expander name\)

`````text
Retrieve the expansion function named NAME from EXPANDER. Return NIL if that function does not exist.
`````

<a id="FUNCTION:EXPANDERS:MAKE-EXPANDER"></a>
<a id="FUNCTION:EXPANDERS-DOCS:TAG5"></a>
#### Function: exp\:make\-expander ()

`````text
Make an expander
`````