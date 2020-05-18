# STRUCTURE-EXT.LEFT-ARROW-ACCESSORS 0.0.6

## introduction
Any language has direction.
In this context, "direction" means order of explain.
For example, English has "from center to around" direction, and Japanese has "from around to center" direction.

E.g. person name.
In English notation, my name is written as 'Shinichi Sato'.
Shinichi is first name (of course center), and Sato is family name (of course around).
But in Japanese notation, my name is written as "佐藤 真一".
"佐藤" equals "Sato", and "真一" equals "Shinichi".

Also day unit.
In English notation, its written "Day Month Year".
It aims from small unit to bigger unit.
But in Japanese notation, its written "年月日".
"年" equals "year", "月" equals "month", and "日" equals "Day".
It aims from big unit to smaller unit.

Also address notations.
In English notation, its written from small place like town to huge place like prefecture, state, etc...
But in Japanese notation, its written from huge place like prefecture to small place like town, street etc...

Of course each language has inversion though.

Also programming language is one kind of language.
In such context, we can refer center as "return value", and around as "algorithm".
Roughly, we can say imperative language has Japanese style "from around to center" direction, and functional language has English style "from center to around" direction.

## Current lisp world
In lisp comunity, functional programming style is recommended.
Language is carefully designed to be able to write functional style easily.
For example, LET's binding is designed "from center to around"(var is center(It is what we want.), and init form is around.), METHOD's specializer is designed so too, WITH-SLOTS's alias is same, etc...

## Issues
But there is few "from around to center" rule in CL.
It is slot accessor or slot accessor style function name like CHAR-CODE.

## Proposal
Using left arrow instead of hyphen is proposed.
Do you think which code is easy to understand?

```lisp
(code-char n)
(char<=code n)
```
I bet it is almost same.
There is just small differences only.
But in many cases, we don't write such low level code as toplevel.
Usually we nests it.
So how below?

```lisp
(defun example1 (symbol)
  (let ((name (package-name (symbol-pakcage symbol))))
    ...))
(defun example2 (symbol)
  (let ((name (name<=package (package<=symbol symbol))))
    ...))
```
In former case, relational part appears alternately.
While reading code from left to right, the stack in your brain acting violently like seesaw but later case.

There is no probrem with class, because we can specify arbitraly name as readers/writers/accessors, but structure.

## Usage

```lisp
(defstruct foo bar)
(DEFINE-LEFT-ARROW-ACCESSORS foo bar)
(bar<=foo (make-foo :bar 0))
=> 0
```

## From developer

* Product's goal - already?
* License - MIT
### Tested
* SBCL/2.0.2
* CCL/1.12
* CLISP/2.49
* ECL/20.4.24
