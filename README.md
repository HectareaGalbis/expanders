

<a id="header-adp-github-headertag273"></a>
# Expanders

Welcome to Expanders\!\! \:D

* [Expanders](/README.md#header-adp-github-headertag273)
  * [What is this\?](/README.md#header-adp-github-headertag274)
  * [Why\?](/README.md#header-adp-github-headertag301)
  * [Reference](/README.md#header-adp-github-headertag302)


<a id="header-adp-github-headertag274"></a>
## What is this\?

The project lets you define ``` expanders ```\. An ``` expander ``` is like a namespace for forms that should transform in a specific way\.

It easier to understand with an example\. Suppose we have the following form ``` (op (+ 3 4) b) ```\. This form will be used in two different macros named ``` plus-macro ``` and ``` minus-macro ```\. Each macro receives a form\. That form could start with ``` op ```\. In that case\, ``` op ``` is substituted by ``` + ``` or ``` - ``` respectively\.

Let\'s define 2 different expanders\:

`````common-lisp
;; We assume (use-package #:expanders)
(defexpander plus-expander define-plus-expansion plus-expansion-p)
(defexpander minus-expander define-minus-expansion minus-expansion-p)
`````
`````common-lisp
minus-expander
`````

Now we have two different expanders\. The function [exp\:expanderp](/README.md#function-expanders-expanderp) can tell us if a symbol denotes an expander\:

`````common-lisp
(expanderp 'plus-expander)
`````
`````common-lisp
t
`````

`````common-lisp
(expanderp 'hey)
`````
`````common-lisp
nil
`````

`````common-lisp
(expanderp 'minus-expander)
`````
`````common-lisp
t
`````

Now it is time to define the expansion of ``` op ```\.

`````common-lisp
(define-plus-expansion op (a b)
  `(+ ,a ,b))

(define-minus-expansion op (a b)
  `(- ,a ,b))
`````
`````common-lisp
op
`````

We can expand a form using [exp\:expand](/README.md#function-expanders-expand)\:

`````common-lisp
(expand 'plus-expander '(op 3 (+ 5 6)))
`````
`````common-lisp
(+ 3 (+ 5 6))
`````

`````common-lisp
(expand 'minus-expander '(op 3 (+ 5 6)))
`````
`````common-lisp
(- 3 (+ 5 6))
`````

Finally\, let\'s define the macros ``` plus-macro ``` and ``` minus-macro ```\:

`````common-lisp
(defmacro plus-macro (form)
  (if (and (consp form)
           (plus-expansion-p (car form)))
      (expand 'plus-expander form)
      form))
`````
`````common-lisp
plus-macro
`````

`````common-lisp
(defmacro minus-macro (form)
  (if (and (consp form)
           (minus-expansion-p (car form)))
      (expand 'minus-expander form)
      form))
`````
`````common-lisp
minus-macro
`````

If we use the form ``` (op 5 4) ``` we will see that each macro will expand to ``` (+ 5 4) ``` or ``` (- 5 4) ``` respectively\.

`````common-lisp
(plus-macro (op 5 4))
`````
`````common-lisp
9
`````

`````common-lisp
(minus-macro (op 5 4))
`````
`````common-lisp
1
`````

<a id="header-adp-github-headertag301"></a>
## Why\?

* **It is common**\: I have noticed that having expanders is a relatively common pattern in macros\. The best example is ``` setf ``` and its ``` setf-expanders ```\. Another project using expanders is [CFFI](https://github.com/cffi/cffi) and its type parsers\. In my own projects I ended up using the same techniques \([Clith](https://github.com/Hectarea1996/clith)\)\.
* **Duality of syntax**\: We can increase the duality of syntax using expanders\. The best example is ``` setf ```\. Thanks to ``` setf ``` we don\'t need names for setters because they come for free with the getter\.


<a id="header-adp-github-headertag302"></a>
## Reference

<a id="function-expanders-defexpander"></a>
#### Macro: exp:defexpander (sym definer predicate)

`````text
Defines an expander represented by the symbol SYM. Defines also a definer and a predicate.
The DEFINER will be like DEFMACRO. It receives a name and a destructuring-lambda-list. The definition
must return the expansion of the provided name.
The PREDICATE will receive a symbol adn will return true if the symbol was used to define an expansion with
the DEFINER.
`````

<a id="function-expanders-expand"></a>
#### Function: exp:expand (expander form)

`````text
Expands a form using an expander.
`````

<a id="function-expanders-expanderp"></a>
#### Function: exp:expanderp (sym)

`````text
Check if a symbol denotes an expander.
`````