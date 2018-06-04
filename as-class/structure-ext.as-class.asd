; vim: ft=lisp et
(in-package :asdf)
(defsystem :structure-ext.as-class
  :version "0.0.0"
  :license "MIT"
  :author "Shinichi Sato"
  :description "Defstruct as defclass"
  :long-description #.(uiop:read-file-string (uiop:subpathname *load-pathname*
                                                               "README.md"))
  :depends-on
  (
   "resignal-bind"      ; condition handlings.
   "lambda-list"        ; utilities for lambda list.
   "closer-mop"         ; wrapper for meta object protocols.
   "with-package"       ; using package temporarily.
   "uiop"               ; utilities.
   )
  :components((:file "as-class")))

(defmethod component-depends-on
           ((o test-op) (c (eql (find-system "structure-ext.as-class"))))
  (append (call-next-method)'((test-op "structure-ext.as-class.test"))))
