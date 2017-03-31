; vim: ft=lisp et
(in-package :asdf)
(defsystem :structure-ext.left-arrow-accessors.test
  :depends-on
  (:jingoh "structure-ext.left-arrow-accessors")
  :components
  ((:file "structure-ext.left-arrow-accessors"))
  :perform
  (test-op (o c) (symbol-call :jingoh :examine)))