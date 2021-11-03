(defpackage :structure-ext.left-arrow-accessors
  (:use :cl)
  (:nicknames "LARAC")
  (:export ;; main api
           #:define-left-arrow-accessors
           ;; condition
           #:missing-symbol))

(in-package :structure-ext.left-arrow-accessors)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun left-arrow-names (type slots)
    (flet ((left-arrow-name (type slot)
             (intern (format nil "~A<=~A" slot type))))
      (loop :for slot :in slots
            :collect (left-arrow-name type slot))))
  (defun accessors (type slots)
    (flet ((accessor (type slot)
             (let ((symbol-name (format nil "~A-~A" type slot)))
               (or (find-symbol symbol-name)
                   (error 'missing-symbol
                          :format-arguments (list 'accessor symbol-name
                                                  *package*))))))
      (loop :for slot :in slots
            :collect (accessor type slot))))) ; eval-when

(defmacro define-left-arrow-accessors (type &rest slot*)
  (check-type type symbol)
  (assert (every #'symbolp slot*))
  (let ((left-arrow-names (left-arrow-names type slot*))
        (accessors (accessors type slot*)))
    `(progn
      (declaim (inline ,@left-arrow-names))
      ,@(the-defun-forms left-arrow-names accessors)
      ,@(the-setf-forms left-arrow-names accessors)
      ',left-arrow-names)))

(defun the-defun-forms (left-arrow-names accessors)
  (loop :for left-arrow-name :in left-arrow-names
        :for accessor :in accessors
        :collect `(defun ,left-arrow-name (#0=#:arg) (,accessor #0#))))

(defun the-setf-forms (left-arrow-names accessors)
  (loop :for left-arrow-name :in left-arrow-names
        :for accessor :in accessors
        :when (fboundp `(setf ,accessor))
          :collect `(defun (setf ,left-arrow-name) (#0=#:new #1=#:arg)
                      (setf (,accessor #1#) #0#))))

(define-condition missing-symbol (simple-error program-error) ()
  (:report
   (lambda (c *standard-output*)
     (apply #'format t "~S: ~S is not found in ~S."
            (simple-condition-format-arguments c)))))