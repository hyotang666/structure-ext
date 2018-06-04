; vim: ft=lisp et
(in-package :asdf)
(defsystem :structure-ext.make-instance
  :author "Shinichi Sato"
  :version "0.0.0"
  :license "MIT"
  :description "Method make-instance for construct structure."
  :depends-on(:closer-mop)
  :components((:file "make-instance")))

(defmethod component-depends-on
           ((o test-op) (c (eql (find-system "structure-ext.make-instance"))))
  (append (call-next-method)'((test-op "structure-ext.make-instance.test"))))
