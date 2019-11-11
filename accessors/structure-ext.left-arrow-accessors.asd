; vim: ft=lisp et
(in-package :asdf)
(defsystem :structure-ext.left-arrow-accessors
  :author "Shinichi Sato"
  :version "0.0.4"
  :license "MIT"
  :description "Slot accessor alias maker."
  :long-description #.(uiop:read-file-string(uiop:subpathname *load-pathname*
                                                              "README.md"))
  :depends-on
  (
   "resignal-bind"      ; condition handlings.
   )
  :components((:file "left-arrow-accessors")))

;;; These forms below are added by JINGOH.GENERATOR.
;; Ensure in ASDF for pretty printings.
(in-package :asdf)
;; Enable testing via (asdf:test-system "structure-ext.left-arrow-accessors").
(defmethod component-depends-on
           ((o test-op)
            (c (eql (find-system "structure-ext.left-arrow-accessors"))))
  (append (call-next-method)
          '((test-op "structure-ext.left-arrow-accessors.test"))))
;; Enable passing parameter for JINGOH:EXAMINER via ASDF:TEST-SYSTEM.
(defmethod operate :around
           ((o test-op)
            (c (eql (find-system "structure-ext.left-arrow-accessors")))
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
