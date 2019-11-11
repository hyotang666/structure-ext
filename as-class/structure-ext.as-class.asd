; vim: ft=lisp et
(in-package :asdf)
(defsystem :structure-ext.as-class
  :version "0.0.4"
  :license "MIT"
  :author "Shinichi Sato"
  :description "Defstruct as defclass"
  :long-description #.(uiop:read-file-string (uiop:subpathname *load-pathname*
                                                               "README.md"))
  :depends-on
  (
   "resignal-bind"      ; condition handlings.
   "lambda-fiddle"      ; utilities for lambda list.
   "closer-mop"         ; wrapper for meta object protocols.
   "uiop"               ; utilities.
   )
  :components((:file "as-class")))

;;; These forms below are added by JINGOH.GENERATOR.
;; Ensure in ASDF for pretty printings.
(in-package :asdf)
;; Enable testing via (asdf:test-system "structure-ext.as-class").
(defmethod component-depends-on
           ((o test-op) (c (eql (find-system "structure-ext.as-class"))))
  (append (call-next-method) '((test-op "structure-ext.as-class.test"))))
;; Enable passing parameter for JINGOH:EXAMINER via ASDF:TEST-SYSTEM.
(defmethod operate :around
           ((o test-op) (c (eql (find-system "structure-ext.as-class")))
            &rest keys
            &key ((:compile-print *compile-print*))
            ((:compile-verbose *compile-verbose*)) &allow-other-keys)
  (flet ((jingoh.args (keys)
           (loop :for (key value) :on keys :by #'cddr
                 :when (find key '(:on-fails :subject :vivid) :test #'eq)
                 :collect key
                 :and
                 :collect value :else
                 :when (eq :jingoh.verbose key)
                 :collect :verbose
                 :and
                 :collect value)))
    (let ((args (jingoh.args keys)))
      (declare (special args))
      (call-next-method))))
;; Enable importing spec documentations.
(let ((system (find-system "jingoh.documentizer" nil)))
  (when (and system (not (featurep :clisp)))
    (load-system system)
    (defmethod perform :after
               ((o load-op) (c (eql (find-system "resignal-bind"))))
      (symbol-call :jingoh.documentizer :import c))))
