; vim: ft=lisp et
(in-package :asdf)
(defsystem :structure-ext.left-arrow-accessors
  :author "Shinichi Sato"
  :version "0.0.2"
  :license "MIT"
  :description "Slot accessor alias maker."
  :long-description #.(uiop:read-file-string(uiop:subpathname *load-pathname*
                                                              "README.md"))
  :depends-on
  (
   "resignal-bind"      ; condition handlings.
   )
  :components((:file "left-arrow-accessors")))

;; These forms below are added by JINGOH.GENERATOR.
(in-package :asdf)
(defmethod component-depends-on
           ((o test-op)
            (c (eql (find-system "structure-ext.left-arrow-accessors"))))
  (append (call-next-method)
          '((test-op "structure-ext.left-arrow-accessors.test"))))
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
(let ((system (find-system "jingoh.documentizer" nil)))
  (when (and system (not (featurep :clisp)))
    (load-system system)
    (defmethod operate :around
               ((o load-op)
                (c (eql (find-system "structure-ext.left-arrow-accessors")))
                &key)
      (let* ((seen nil)
             (*default-pathname-defaults*
              (merge-pathnames "spec/" (system-source-directory c)))
             (*macroexpand-hook*
              (let ((outer-hook *macroexpand-hook*))
                (lambda (expander form env)
                  (if (not (typep form '(cons (eql defpackage) *)))
                      (funcall outer-hook expander form env)
                      (if (find (cadr form) seen :test #'string=)
                          (funcall outer-hook expander form env)
                          (progn
                           (push (cadr form) seen)
                           `(progn
                             ,form
                             ,@(symbol-call :jingoh.documentizer :importer
                                            form)))))))))
        (call-next-method)))))
