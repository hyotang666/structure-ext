(defpackage :structure-ext.as-class
  (:use :cl)
  (:nicknames "AS-CLASS")
  (:export #:defstruct*))

(in-package :structure-ext.as-class)

(defmacro defstruct* (&body body)
  (multiple-value-bind (name options documentation slots)
      (parse body)
    (check options)
    `(progn
      (defclass ,name ,(super-classes options) ,(slots slots options name)
        ,@(may-initargs options)
        ,@(may-documentation documentation))
      ,@(constructors name options)
      ,@(print-function name options)
      ,@(copier name options)
      ,@(predicate name options)
      ,@(accessors name options)
      ',name)))

(defun parse (body)
  (labels ((name&options (name&options)
             (if (symbolp name&options)
                 (values name&options nil)
                 (values (car name&options)
                         (canonicalize (cdr name&options)))))
           (slots (slots)
             (if (stringp (car slots))
                 (values (car slots) (canonicalize (cdr slots)))
                 (values nil (canonicalize slots))))
           (canonicalize (slots)
             (mapcar #'uiop:ensure-list slots)))
    (multiple-value-call #'values
      (name&options (car body))
      (slots (cdr body)))))

(defun check (options)
  (flet ((check (type)
           (when (assoc type options)
             (error "Options ~S is invalid in DEFSTRUCT* form.~%~S" type
                    options))))
    (mapc #'check `(:type :named :initial-offset))))

(defun super-classes (options)
  (mapcar #'second (collect-option :include options)))

(defun collect-option (option options)
  (remove-if (complement (lambda (x) (eq option x))) options :key #'car))

(defun slots (slots options class-name)
  (let ((conc-name (conc-name options class-name)))
    (loop :for slot :in slots
          :collect (enslot slot conc-name))))

(defun conc-name (options class-name)
  (let ((conc-name (assoc :conc-name options)))
    (if conc-name
        (cadr conc-name)
        (format nil "~A-" class-name))))

(defun enslot (slot conc-name) ; separated cause of huge.
  (labels ((may-type-spec (slot-options)
             (let ((spec (getf slot-options :type)))
               (when spec
                 `(:type ,spec))))
           (initform (init-form)
             `(:initform ,init-form))
           (initarg (slot-name)
             (intern (symbol-name slot-name) :keyword))
           (may-accessors (conc-name slot-name slot-options)
             `(,(if (getf slot-options :read-only)
                    :reader
                    :accessor)
               ,(method-name conc-name slot-name))))
    (destructuring-bind
        (slot-name &optional init-form . slot-options)
        slot
      `(,slot-name ,@(may-type-spec slot-options) ,@(initform init-form)
        :initarg ,(initarg slot-name)
        ,@(may-accessors conc-name slot-name slot-options)))))

(defun method-name (conc-name slot-name)
  (intern (format nil "~@[~A~]~A" conc-name slot-name)))

(defun may-documentation (doc)
  (when doc
    `((:documentation ,doc))))

(defun may-initargs (options)
  (labels ((default-initargs (super-classes)
             (loop :for option :in super-classes
                   :when (third option)
                     :nconc (option-plist (cddr option))))
           (option-plist (slot-descriptions)
             (loop :for slot-description :in slot-descriptions
                   :when (symbolp slot-description)
                     :collect (intern (symbol-name slot-description) :keyword)
                     :and :collect nil
                   :else
                     :collect (intern (symbol-name (car slot-description))
                                      :keyword)
                     :and :collect (cadr slot-description))))
    (let ((superclasses (collect-option :include options)))
      (when superclasses
        (let ((initargs (default-initargs superclasses)))
          (when initargs
            `((:default-initargs ,@initargs))))))))

(defun constructors (name options)
  (labels ((default-constructor (constructor)
             `(defun ,constructor (&rest args)
                (apply #'make-instance ',name args)))
           (default-name (name)
             (intern (format nil "MAKE-~A" name)))
           (rec (constructors &optional acc)
             (if (endp constructors)
                 (do-return acc)
                 (body (car constructors) (cdr constructors) acc)))
           (body (constructor rest acc)
             (etypecase constructor
               ((cons (eql :constructor) null)
                (rec rest
                     (push (default-constructor (default-name name)) acc)))
               ((cons (eql :constructor) (cons symbol null))
                (if (second constructor)
                    (rec rest
                         (push (default-constructor (second constructor)) acc))
                    (do-return acc)))
               ((cons (eql :constructor) (cons symbol (cons t null)))
                (rec rest
                     (push
                      `(defun ,(second constructor) ,(third constructor)
                         (make-instance ',name
                                        ,@(canonicalize (third constructor))))
                      acc)))))
           (canonicalize (lambda-list)
             (loop :for var
                        :in (lambda-fiddle:extract-all-lambda-vars lambda-list)
                   :collect (intern (symbol-name var) :keyword)
                   :collect var))
           (do-return (acc)
             (nreverse acc)))
    (let ((constructors (collect-option :constructor options)))
      (if (null constructors)
          `(,(default-constructor (default-name name)))
          (rec constructors)))))

(defun print-function (name options)
  (let ((print-function (cadr (assoc :print-function options)))
        (print-object (cadr (assoc :print-object options))))
    (when (and print-function print-object)
      (error
        "Invalid syntax:~%Only one of :print-function or :print-object is allowed but~S"
        options))
    (or (and print-function
             `((defmethod print-object ((obj ,name) stream)
                 (,print-function obj stream *print-level*))))
        (and print-object
             `((defmethod print-object ((obj ,name) stream)
                 (,print-object obj stream)))))))

(defun copier (name options)
  (flet ((def-form (method)
           `(,@(may-generic method)
             (defmethod ,method ((arg ,name))
               (let ((obj (make-instance (class-of arg))))
                 (dolist (slot (slots<=obj arg))
                   (if (slot-boundp arg slot)
                       (setf (slot-value obj slot) (slot-value arg slot))
                       (slot-makunbound obj slot)))
                 obj)))))
    (let ((option (assoc :copier options)))
      (if option
          (when (cadr option)
            (def-form (cadr option)))
          (def-form (method-name "COPY-" name))))))

(defun slots<=obj (obj)
  (mapcar #'c2mop:slot-definition-name (c2mop:class-slots (class-of obj))))

(defun may-generic (method &optional (lambda-list '(arg)))
  (when (or (null (fboundp method))
            (not (typep (fdefinition method) 'generic-function)))
    `((defgeneric ,method ,lambda-list))))

(defun predicate (name options)
  (flet ((def-form (method)
           #.(or #+(or clisp sbcl)
                 '`((defun ,method (arg) (typep arg ',name)))
                 '`(,@(may-generic method)
                    (defmethod ,method ((arg ,name)) arg)
                    (defmethod ,method (arg) (declare (ignore arg)) nil)))))
    (let ((option (assoc :predicate options)))
      (if option
          (when (cadr option)
            (def-form (cadr option)))
          (def-form (method-name (conc-name nil name) "P"))))))

(defun accessors (name options)
  (let ((includes (collect-option :include options)))
    (when includes
      (uiop:while-collecting (acc)
        (labels ((ensure-class-slots (class)
                   (ensure-finalize class)
                   (c2mop:class-slots class))
                 (ensure-finalize (class)
                   (unless (c2mop:class-finalized-p class)
                     (c2mop:finalize-inheritance class)))
                 (generic? (generic)
                   (when generic
                     (acc (car generic))))
                 (collect (include slot-name)
                   (let ((method-name
                          (method-name (conc-name options name) slot-name))
                         (super-accessor
                          (method-name (conc-name options (cadr include))
                                       slot-name)))
                     (generic? (may-generic method-name))
                     (acc
                       `(defmethod ,method-name ((#0=#:arg ,name))
                          (,super-accessor #0#)))
                     (when (and (has-setter? super-accessor)
                                (null (cadr (member :read-only include))))
                       (generic? (may-generic `(setf ,method-name) '(new arg)))
                       (acc
                         `(defmethod (setf ,method-name) (#1=#:new (#0# ,name))
                            (setf (,super-accessor #0#) #1#))))))
                 (has-setter? (super-accessor)
                   (values (ignore-errors
                            (fdefinition `(setf ,super-accessor))))))
          (dolist (include includes)
            (dolist (slot (ensure-class-slots (find-class (cadr include))))
              (collect include (c2mop:slot-definition-name slot)))))))))