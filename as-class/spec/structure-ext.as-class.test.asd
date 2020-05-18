; vim: ft=lisp et
(in-package :asdf)
(defsystem :structure-ext.as-class.test
  :version "0.0.2"
  :depends-on
  (:jingoh "structure-ext.as-class")
  :components
  ((:file "structure-ext.as-class"))
  :perform
  (test-op (o c) (symbol-call :jingoh :examine :structure-ext.as-class)))
