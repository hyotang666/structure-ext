(defpackage :structure-ext.make-instance.spec
  (:use :cl :jingoh))
(in-package :structure-ext.make-instance.spec)
(setup :structure-ext.make-instance)

(requirements-about make-instance :doc-type nil)

#?(defstruct foo bar)
=> FOO
,:lazy nil
,:ignore-signals warning
,:stream nil

#?(make-instance 'foo) :be-the foo

#?(let ((a (make-instance 'foo :bar 0))
        (b (make-instance 'foo :bar 1)))
    (setf (foo-bar a) :reset)
    (values (foo-bar a) (foo-bar b)))
:values (:RESET 1)
