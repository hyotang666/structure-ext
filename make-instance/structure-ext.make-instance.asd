; vim: ft=lisp et
(in-package :asdf)
(defsystem :structure-ext.make-instance
  :depends-on(:closer-mop)
  :components((:file "make-instance")))
;; Perform method below is added by JINGOH.GENERATOR.
(defmethod perform
           ((o test-op) (c (eql (find-system "structure-ext.make-instance"))))
  (test-system :structure-ext.make-instance.test))