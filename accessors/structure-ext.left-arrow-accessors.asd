; vim: ft=lisp et
(in-package :asdf)
(defsystem :structure-ext.left-arrow-accessors
  :author "Shinichi Sato"
  :version "0.0.0"
  :license "MIT"
  :description "Slot accessor alias maker."
  :long-description #.(uiop:read-file-string(uiop:subpathname *load-pathname*
                                                              "README.md"))
  :depends-on(:resignal-bind :lambda-list)
  :components((:file "left-arrow-accessors")))
;; Perform method below is added by JINGOH.GENERATOR.
(defmethod perform
           ((o test-op)
            (c (eql (find-system "structure-ext.left-arrow-accessors"))))
  (test-system :structure-ext.left-arrow-accessors.test))
