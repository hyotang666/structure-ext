(defpackage :structure-ext.left-arrow-accessors.spec
  (:use :cl :jingoh :structure-ext.left-arrow-accessors))
(in-package :structure-ext.left-arrow-accessors.spec)
(setup :structure-ext.left-arrow-accessors)

(requirements-about DEFINE-LEFT-ARROW-ACCESSORS)

;;;; Description:
; Define accessor aliases.
#?(defstruct foo bar bazz)
=> FOO
,:lazy nil
,:ignore-signals warning
,:stream nil
#?(define-left-arrow-accessors foo bar)
=> (BAR<=FOO)
,:test equal
,:ignore-signals warning
,:stream nil
#?(bar<=foo(make-foo :bar 0))
=> 0
#?(bazz<=foo(make-foo :bazz 0))
:signals (or undefined-function
	     warning ; for ccl
	     )

#+syntax
(DEFINE-LEFT-ARROW-ACCESSORS type &rest slot*) ; => result

;;;; Arguments and Values:

; type := symbol which is structure name, otherwise error.
#?(define-left-arrow-accessors "FOO" bazz) :signals error
#?(define-left-arrow-accessors bar bazz) :signals error

; slot* := symbol which is slot-name, otherwise error.
#?(define-left-arrow-accessors foo "BAR") :signals error
#?(define-left-arrow-accessors foo hoge) :signals error

; result := list which includes defined accessor names.

;;;; Affected By:
; lisp image status.

;;;; Side-Effects:
; define new functions in current package.

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about MISSING-SYMBOL)

;;;; Description:
;;;; Class Precedence List: (case in SBCL)
; missing-symbol simple-error simple-condition program-error error serious-condition condition slot-object t

;;;; Effective Slots:

; FORMAT-CONTROL [Type] T
; [READER] simple-condition-format-control

; FORMAT-ARGUMENTS [Type] T
; [READER] simple-condition-format-arguments

;;;; Notes:

