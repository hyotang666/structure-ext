; vim: ft=lisp et
(in-package :asdf)
(defsystem :structure-ext.as-class
  :depends-on(:resignal-bind :lambda-list :closer-mop :with-package :uiop)
  :components((:file "as-class")))
;; Perform method below is added by JINGOH.GENERATOR.
(defmethod perform
           ((o test-op) (c (eql (find-system "structure-ext.as-class"))))
  (test-system :structure-ext.as-class.test))