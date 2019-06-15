; vim: ft=lisp et
(in-package :asdf)
(defsystem :structure-ext.left-arrow-accessors
  :author "Shinichi Sato"
  :version "0.0.1"
  :license "MIT"
  :description "Slot accessor alias maker."
  :long-description #.(uiop:read-file-string(uiop:subpathname *load-pathname*
                                                              "README.md"))
  :depends-on
  (
   "resignal-bind"      ; condition handlings.
   )
  :components((:file "left-arrow-accessors")))

(defmethod component-depends-on
           ((o test-op)
            (c (eql (find-system "structure-ext.left-arrow-accessors"))))
  (append (call-next-method)'((test-op "structure-ext.left-arrow-accessors.test"))))
(defmethod operate :around(o (c (eql (find-system "structure-ext.left-arrow-accessors")))
                             &key((:compile-print *compile-print*))
                             ((:compile-verbose *compile-verbose*))
                             &allow-other-keys)
  (call-next-method))
