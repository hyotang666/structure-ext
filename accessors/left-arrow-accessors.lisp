(defpackage :structure-ext.left-arrow-accessors(:use :cl :resignal-bind)
  (:nicknames "LARAC")
  (:export
    ;; main api
    #:define-left-arrow-accessors
    ;; condition
    #:missing-symbol
    ))
(in-package :structure-ext.left-arrow-accessors)

(eval-when(:compile-toplevel :load-toplevel :execute)
  (defun left-arrow-names(type slots)
    (flet((LEFT-ARROW-NAME(type slot)
            (intern(format nil "~A<=~A" slot type)))
          )
      (loop :for slot :in slots
            :collect (LEFT-ARROW-NAME type slot))))

  (defun accessors(type slots)
    (flet((ACCESSOR(type slot)
            (let((symbol-name(format nil "~A-~A" type slot)))
              (or (find-symbol symbol-name)
                  (error 'missing-symbol
                         :format-arguments(list 'accessor symbol-name *package*)))))
          )
      (loop :for slot :in slots
            :collect (ACCESSOR type slot))))
  ) ; eval-when

(defmacro define-left-arrow-accessors(type &rest slot*)
  (check-type type symbol)
  (assert(every #'symbolp slot*))
  (let((left-arrow-names(left-arrow-names type slot*))
       (accessors(accessors type slot*)))
    `(PROGN
       (DECLAIM (INLINE ,@left-arrow-names))
       ,@(the-defun-forms left-arrow-names accessors)
       ,@(the-setf-forms left-arrow-names accessors)
       ',left-arrow-names)))

(defun the-defun-forms(left-arrow-names accessors)
  (loop :for left-arrow-name :in left-arrow-names
        :for accessor :in accessors
        :collect `(DEFUN,left-arrow-name(#0=#:arg)
                    (,accessor #0#))))

(defun the-setf-forms(left-arrow-names accessors)
  (loop :for left-arrow-name :in left-arrow-names
        :for accessor :in accessors
        :when (fboundp `(SETF ,accessor))
        :collect `(DEFUN(SETF,left-arrow-name)(#0=#:new #1=#:arg)
                    (SETF(,accessor #1#)#0#))))

(define-condition missing-symbol(simple-error program-error) ()
  (:report (lambda(c *standard-output*)
             (apply #'format t "~S: ~S is not found in ~S."
                    (simple-condition-format-arguments c)))))

