(defpackage :structure-ext.make-instance
  (:use :cl)
  (:nicknames "MAKINS"))

(in-package :structure-ext.make-instance)

#+(or sbcl allegro clasp)
(defmethod make-instance :around ((type structure-class) &rest args)
  (loop :with instance = (copy-structure (closer-mop:class-prototype type))
        :for slot :in (closer-mop:class-slots type)
        :for name = (closer-mop:slot-definition-name slot)
        :do (setf (slot-value instance name)
                    (or (getf args (intern (symbol-name name) :keyword))
                        (funcall
                          (handler-case
                              (closer-mop:slot-definition-initfunction slot)
                            (error ()
                              (constantly nil))
                            (:no-error (fun?)
                              (or fun? (constantly nil)))))))
        :finally (return instance)))

#+abcl
(defmethod make-instance :around ((type structure-class) &rest args)
  (loop :with instance = (copy-structure (closer-mop:class-prototype type))
        :for slot :in (closer-mop:class-slots type)
        :for name = (aref slot 1)
        :do (setf (slot-value instance name)
                    (or (getf args (intern (symbol-name name) :keyword))
                        (aref slot 4)))
        :finally (return instance)))

#+abcl
(defmethod make-instance ((type structure-class) &rest args)
  (declare (ignore args))
  :dummy)