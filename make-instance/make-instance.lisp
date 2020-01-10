(defpackage :structure-ext.make-instance(:use :cl)
  (:nicknames "MAKINS"))
(in-package :structure-ext.make-instance)

#+(or sbcl)
(defmethod make-instance :around ((type structure-class) &rest args)
  (loop :with instance = (copy-structure(closer-mop:class-prototype type))
        :for slot :in (closer-mop:class-slots type)
        :for name = (closer-mop:slot-definition-name slot)
        :do(setf (slot-value instance name)
                 (or (getf args (intern(symbol-name name):keyword))
                     (funcall(closer-mop:slot-definition-initfunction slot))))
        :finally (return instance)))

