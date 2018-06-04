; vim: ft=lisp et
(in-package :asdf)

(progn
  (defsystem :structure-ext
             :version "0.0.0"
             :description "Tiny structure extensions"
             :author "Shinichi Sato"
             :licence "MIT"
             :depends-on #0=("structure-ext.left-arrow-accessors"
                             "structure-ext.make-instance"
                             "structure-ext.as-class")
             :components((:file "package")))

  (defmethod component-depends-on((o test-op) (c (eql (find-system "structure-ext"))))
    (append (call-next-method) '((test-op . #0#))))
  )
