elva language specification
====

Elva is a tiny LISP on Java.
Everyone can write elva by learning only how to define functions and macros and how to call the Java APIs.

## Why Elva?

The elva specification is very small and is independent of the complex traditions of Common Lisp and Scheme.
Unlike Clojure, you don't have to keep up with the latest language evolution to maintain the programs.
This is a great advantage for domain-specific issues.

## Learn Elva

Everything you need to learn is written on this page.
If you find a function or macro that you are not familiar with, you can always find the definition by following the source code dependencies.

## Evaluation

A program is written as an atom or a list form.
Atoms are all values that are not lists, e.g., a symbol, type, number, boolean, string, function, macro, and other Java objects.
Unlike lists, they cannot be disassembled, so they are called atoms.

```lisp
1919810 ;atom
"Hello" ;atom
(3 6 4) ;list
```

Elva evaluates these programs according to the following rules.

- The value of an atom is the atom itself.
- The value of a symbol is the variable value referenced by the symbol.
- For lists, apply the operator at the first element to the subsequent list elements.

There are no exceptions.

## Atom

### Symbol

The following shows an example of defining and referencing the variable `JR1ZTT`.

```lisp
(setq JR1ZTT 1420)
JR1ZTT ;1420
```

If there is no variable with the specified name, the symbol will be resolved as the name of the Java class.

```lisp
(import java.lang.String)
String ;java.lang.String
```

### Number

A number is an instance of `Number`, e.g., an `int`, `long`, `float`, `double` and `BigDecimal`.
The entity of integer literal is an `int`, and all other numeric literals with a decimal point are `BigDecimal`s.

```lisp
1919810 ;int
364.364 ;BigDecimal
```

### Boolean

A boolean literal is an instance of `Boolean`.

```lisp
#t ;true
#f ;false
```

### String

A string literal is an instance of `String`.

```lisp
"Ramen Yatai"
```

### Object

Any other atoms are instances of `Object`.

## List

A list is a data structure in which multiple elements are arranged in order.
A list is an instance of `List`.

```lisp
'(1 2 3) ;a list whose contents are 1, 2, and 3
```

To create a list, use the `list` function.

```lisp
(list 1 2 3)
```

To insert an element at the beginning of the list, use the `cons` function.

```lisp
(cons 1 (list 2 3)) ;(1 2 3)
```

Use the `car` function to retrieve the first element of the list.
Use the `cdr` function to get a partial list with the second and subsequent elements of the list.

```lisp
(car (list 1 2 3)) ;1
(cdr (list 1 2 3)) ;(2 3)
```

The `nth` function is an efficient way to specify an index by number and retrieve the element at that position.

```lisp
(nth 0 (list 1 14 5 14)) ;1
(nth 2 (list 1 14 5 14)) ;5
```

Note that Java arrays are handled as lists in elva.

## Function

A function works as the C and Java programmers know.
You can define some functions `add` and `mul` as follows.

```lisp
(defun add (x y z) (+ x y z))
(defun mul (x y z) (* x y z))
```

These functions take three values as arguments and calculate the sum or product.
To call these functions, write as follows.

```lisp
(add 1 2 3) ;  6
(mul 3 6 9) ;162
```

Note that the function arguments are evaluated before calling the function.
Therefore, you can specify the value of a function as an argument to another function as follows.

```lisp
(add (mul 5 9 9) (mul 1 0 0) (mul 1 1 0)) ;405
(mul (add 5 9 9) (add 1 0 0) (add 1 1 0)) ; 46
```

The arguments are evaluated as follows.
Keep this behavior in mind and compare it to the macro behavior described below.

```lisp
(add 405 0 1) ;405
(mul  23 1 2) ; 46
```

To create an anonymous function, use the `lambda` form.

```lisp
(lambda () (+ 1420 16005)) ;accepts no arguments and returns 17425
(lambda (x y z) (+ x y z)) ;accepts three arguments
```

These anonymous functions can be called in the same way as named functions.

```lisp
((lambda (x y) (* x y)) 100 110) ;11000
```

In fact, the `defun` macro is syntactic sugar equivalent to the following expression.

```lisp
(defmacro defun (name params body) (list 'setq name (list 'lambda params body)))
```

## Macro

A macro is a function that transforms a macro call into another expression and evaluates the converted expression.
The functions `add` and `mul` introduced above can be redefined as macros as follows.

```lisp
(defmacro add (x y z) (list + x y z)) ;will expand x,y,z into the template (+ x y z)
(defmacro mul (x y z) (list * x y z)) ;will expand x,y,z into the template (* x y z)
```

And you can use them like functions.

```lisp
(add (mul 5 9 9) (mul 1 0 0) (mul 1 1 0)) ;405
(mul (add 5 9 9) (add 1 0 0) (add 1 1 0)) ; 46
````

However, macros are not functions.
First, the macro call is evaluated as follows:

```lisp
(+ (mul 5 9 9) (mul 1 0 0) (mul 1 1 0)) ;405
(* (add 5 9 9) (add 1 0 0) (add 1 1 0)) ; 46
```

Great.
By using the `list` function, both macros embed their arguments into the template without evaluating the arguments.
The converted expression is then evaluated as follows.

```lisp
(+ 405 0 0) ;405
(*  23 1 2) ; 26
```

Unlike functions, macros give you the freedom to decide whether to evaluate an argument or treat it as data without evaluating it.
Macros are a good way to implement some syntax or domain-specific languages in LISP.
To create an anonymous macro, use the `syntax` form.

```lisp
(syntax () (+ 1420 16005)) ;accepts no arguments and returns 17425
(syntax (x y z) (+ x y z)) ;accepts three arguments
```

These anonymous macros can be called in the same way as named macros.

```lisp
((syntax (x y) (* x y)) 100 110) ;11000
```

In fact, the `defmacro` macro is syntactic sugar equivalent to the following expression.

```lisp
(defmacro defmacro (name params body) (list 'setq name (list 'syntax params body)))
```

## Java APIs

To import a Java class, use the `import` function.

```
(import java.lang.String)
```

To reference a Java method, use the `method` function.

```lisp
(method 'length String) ;String.length()
```

Give arguments to call the method.
The first argument must be the object that has the method.
This argument is not needed when calling static methods.

```lisp
((method 'replaceAll String String String) "foobar" "r" "z") ;"foobaz"
```

The second and subsequent arguments are passed to the method as its arguments.
You can refer to the Java constructor by setting the method name to `new`.

```lisp
((method `new Integer int) 123)
```
Passing an elva list to a Java method will convert the list to a Java `List`.
If the method requires an array, the list will be converted to an array.

## Quotation

Quoting is an important feature that realizes LISP's powerful metaprogramming capabilities.
The `quote` form controls when the expression is evaluated.

```lisp
(quote (+ 1 2 3)) ;a list (+ 1 2 3)
```

The `quote` form accepts a single expression as an argument and returns it as is without evaluation.
You can abbreviate the `quote` form as follows.

```lisp
'(+ 1 2 3) ;is equivalent to (quote (+ 1 2 3))
```

There is another version of quoting, namely `quasiquote`.

```lisp
`(+ 1 2 3)
(quasiquote (+ 1 2 3))
```

You can unquote some sub expressions in the quasi-quoted expression.
These sub expressions are evaluated exceptionally.
You can use the `unquote` function or a special symbol `` ` `` for this purpose.

```lisp
`(+ 1 2 ,(+ 1 2)) ;(+ 1 2 3)
```

There is another version of unquoting, called `unquote-splicing`.
This evaluates the unquoted sub expression as a list and then embeds the elements to the expression as a list.

```lisp
`(+ 1 2 ,@(list 3 4 5)) ;(+ 1 2 3 4 5)
```

## System Functions

### +

performs addition and returns a real value.

```lisp
(+ real1 reals...)
```

### and

performs and operation and returns a bool value.

```lisp
(and bool1 bools...)
```

### array

returns an array type.

```lisp
(array element-type)
```

### assert

throws an error if the expression returns false.

```lisp
(assert condition message)
```

### block

evaluates the expressions inside a nested scope.

```lisp
(block expressions...)
```

### cadr

returns the second element of the specified list.

```lisp
(cadr list)
```

### car

returns the first element of the specified list.

```lisp
(car list)
```

### catch

evaluates the expression and returns an error.

```lisp
(catch expression)
```

### cddr

returns the third and subsequent elements of the specified list.

```lisp
(cddr list)
```

### cdr

returns the second and subsequent elements of the specified list.

```lisp
(cdr list)
```

### cons

concatenates the specified value and list into a list.

```lisp
(cons first second)
```

### contest

creates and returns a [`contest`](https://nextzlog.github.io/qxsl/doc/qxsl/ruler/Contest.html) object, which references `start-day`, `final-day`, and `dead-line` functions.

```lisp
(contest name host mail link)
```

### /

performs division and returns a real value.

```lisp
(/ real1 reals...)
```

### eq

performs identify test and returns a bool value.

```lisp
(equal value1 value2)
```

### equal

performs equality test and returns a bool value.

```lisp
(equal value1 value2)
```

### eval

evaluates the specified value.

```lisp
(eval value)
```

### failure

creates and returns a [`failure`](https://nextzlog.github.io/qxsl/doc/qxsl/ruler/Failure.html) object.

```lisp
(failure item message)
```

### format

formats a string using the specified arguments.

```lisp
(format string arguments...)
```

### >=

performs greater-equal operation and returns a bool value.

```lisp
(>= real1 reals...)
```

### getf

extracts the specified field value of the item.

```lisp
(getf item qname)
```

### >

performs greater operation and returns a bool value.

```lisp
(> real1 reals...)
```

### if

evaluates the first (or second) expression if true (or false).

```lisp
(if condition first second)
```

### import

imports the specified class.

```lisp
(import class)
```

### item

creates and returns an [`item`](https://nextzlog.github.io/qxsl/doc/qxsl/model/Item.html) object.

```lisp
(item)
```

### lambda

creates and returns an anonymous closure.

```lisp
(lambda (parameters) body)
```

### <=

performs less-equal operation and returns a bool value.

```lisp
(<= real1 reals...)
```

### length

returns the length of the list.

```lisp
(length list)
```

### let

creates a local variable and evaluates the expression.

```lisp
(let variable value expression)
```

### list

creates a list of elements as specified.

```lisp
(list elements...)
```

### load

loads the specified LISP file.

```lisp
(load string)
```

### <

performs less operation and returns a bool value.

```lisp
(< real1 reals...)
```

### member

tests if the list contains the specified value.

```lisp
(member value list)
```

### method

returns a method of the specified class.

```lisp
(method 'name class parameter-types...)
```

### mod

performs modulo and returns a real value.

```lisp
(mod real1 reals...)
```

### *

performs multiplication and returns a real value.

```lisp
(* real1 reals...)
```

### name

returns a qualified name in the specified namespace.

```lisp
(name namespace local-name)
```

### nil?

tests if the argument is nil or not.

```lisp
(nil? argument)
```

### not

performs not operation and returns a bool value.

```lisp
(not bool)
```

### nth

returns the n-th element of the list.

```lisp
(nth index list)
```

### null?

tests if the argument is null or not.

```lisp
(null? argument)
```

### or

performs or operation and returns a bool value.

```lisp
(or bool1 bools...)
```

### pattern

creates and returns a [`pattern`](https://nextzlog.github.io/qxsl/doc/qxsl/ruler/Pattern.html) object.

```lisp
(pattern normalize transform)
```

### quasiquote

quotes the expression except for some unquoted sub-expressions.

```lisp
(quasiquote expression)
```

### quote

returns the expression without evaluation.

```lisp
(quote expression)
```

### rcvd

extracts the [`rcvd`](https://nextzlog.github.io/qxsl/doc/qxsl/model/Rcvd.html) object from the item.

```lisp
(rcvd item)
```

### section

takes four functions and returns a [`section`](https://nextzlog.github.io/qxsl/doc/qxsl/ruler/Section.html) object.
 
```lisp
(section name code verify unique entity result)
```

### sent

extracts the [`sent`](https://nextzlog.github.io/qxsl/doc/qxsl/model/Sent.html) object from the item.

```lisp
(sent item)
```

### set

creates a variable with the specified name and value.

```lisp
(set name value)
```

### setf

assigns the specified field value into the item.

```lisp
(setf item qname value)
```

### -

performs subtraction and returns a real value.

```lisp
(- real1 reals...)
```

### subseq

returns a subsequence of the specified list.

```lisp
(subseq list start end)
```

### success

creates and returns a [`success`](https://nextzlog.github.io/qxsl/doc/qxsl/ruler/Success.html) object.

```lisp
(success item score)
```

### symbol

returns a symbol of the specified name.

```lisp
(symbol name)
```

### syntax

creates and returns an anonymous macro.

```lisp
(syntax (parameters) body)
```

### throw

throws an error with the specified message.

```lisp
(throw string)
```

### type

returns the type of the specified value.

```lisp
(type value)
```

### unquote-splicing

embeds the specified list elements into the outer expression.

```lisp
(unquote-splicing list)
```

### unquote

embeds the specified value into the outer expression.

```lisp
(unquote value)
```

### xor

performs exclusive-or operation and returns a bool value.

```lisp
(xor bool1 bool2)
```
