; vim: ft=lisp et
(in-package :asdf)
(defsystem :structure-ext.make-instance.test
  :version "0.0.1"
  :depends-on
  (:jingoh "structure-ext.make-instance")
  :components
  ((:file "structure-ext.make-instance"))
  :perform
  (test-op (o c) (symbol-call :jingoh :examine :structure-ext.make-instance)))
