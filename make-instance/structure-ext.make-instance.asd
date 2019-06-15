; vim: ft=lisp et
(in-package :asdf)
(defsystem :structure-ext.make-instance
  :author "Shinichi Sato"
  :version "0.0.1"
  :license "MIT"
  :description "Method make-instance for construct structure."
  :depends-on
  (
   "closer-mop" ; wrapper for meta object protocols.
   )
  :components((:file "make-instance")))

(defmethod component-depends-on
           ((o test-op) (c (eql (find-system "structure-ext.make-instance"))))
  (append (call-next-method)'((test-op "structure-ext.make-instance.test"))))
(defmethod operate :around(o (c (eql (find-system "structure-ext.make-instance")))
                             &key ((:compile-print *compile-print*))
                             ((:compile-verbose *compile-verbose*))
                             &allow-other-keys)
  (call-next-method))
