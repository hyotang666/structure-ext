(defpackage :structure-ext.as-class.spec
  (:use :cl :jingoh :structure-ext.as-class))
(in-package :structure-ext.as-class.spec)
(setup :structure-ext.as-class.spec)

(requirements-about DEFSTRUCT*)

;;;; Description:
; Define class by defstruct syntax.

#+syntax
(DEFSTRUCT* &body body) ; => result
#?(defstruct* foo bar)
=> FOO
,:lazy nil
,:ignore-signals warning
#?(make-foo) :be-the foo
#?(foo-bar(make-foo :bar 0)) => 0
#?(foo-p(make-foo)) => T
#?(let*((foo(make-foo :bar 0))
	(copied(copy-foo foo)))
    (values (eq foo copied)
	    (foo-bar copied)))
:multiple-value-satisfies #`(& (null $samep)
			       (= 0 $copied-bar-value))

;;;; Arguments and Values:

; see CL:DEFSTRUCT.

;;;; Affected By:
; none

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:
; :initial-offset, :type and :named option is invalid.
#?(defstruct*(bar (:type list))
    hoge fuga)
:signals error
#?(defstruct*(bar (:initial-offset 4))
    hoge fuga) 
:signals error

; specify both :print-function and :print-object is invalid.
#?(defstruct*(bar (:print-function bar-printer)
		  (:print-object (lambda(&rest args)(print args))))
    hoge fuga)
:signals error
