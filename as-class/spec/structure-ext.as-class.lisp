(defpackage :structure-ext.as-class.spec
  (:shadowing-import-from :structure-ext.as-class
			  #:args #:arg #:slots<=obj #:obj #:slot #:new)
  (:use :cl :jingoh :structure-ext.as-class))
(in-package :structure-ext.as-class.spec)
(setup :structure-ext.as-class)

(requirements-about DEFSTRUCT* :doc-type function)

;;;; Description:
; Define class by defstruct syntax.

#+syntax
(DEFSTRUCT* &body body) ; => result

#?(defstruct* foo bar)
=> FOO
,:lazy nil
,:ignore-signals warning
,:stream nil

#?(make-foo) :be-the foo
#?(foo-bar(make-foo :bar 0)) => 0
#?(foo-p(make-foo)) :satisfies identity
#?(let*((foo(make-foo :bar 0))
	(copied(copy-foo foo)))
    (values (eq foo copied)
	    (foo-bar copied)))
:multiple-value-satisfies (lambda($samep $copied-bar-value)
			    (& (null $samep)
			       (= 0 $copied-bar-value)))

;;;; Arguments and Values:

; see CL:DEFSTRUCT.

;;;; Affected By:
; none

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:
; :initial-offset, :type and :named option is invalid.
#?(defstruct*(bar (:type list))
    hoge fuga)
:signals error
#?(defstruct*(bar (:initial-offset 4))
    hoge fuga) 
:signals error

; specify both :print-function and :print-object is invalid.
#?(defstruct*(bar (:print-function bar-printer)
		  (:print-object (lambda(&rest args)(print args))))
    hoge fuga)
:signals error

;;;; Examples
; simplest form
#?(defstruct* hoge bar)
:expanded-to
(progn (defclass hoge ()
	 ((bar :initform nil :initarg :bar :accessor hoge-bar)))
       (defun make-hoge (&rest args)
	 (apply #'make-instance 'hoge args))
       (defgeneric copy-hoge (arg))
       (defmethod copy-hoge ((arg hoge))
	 (let((obj(make-instance (class-of arg))))
	   (dolist(slot (slots<=obj arg))
	     (if(slot-boundp arg slot)
	       (setf (slot-value obj slot)(slot-value arg slot))
	       (slot-makunbound obj slot)))
	   obj))
       #+(or clisp sbcl)
       (defun hoge-p(arg)(typep arg 'hoge))
       #-(or clisp sbcl)
       (defgeneric hoge-p(arg))
       #-(or clisp sbcl)
       (defmethod hoge-p((arg hoge))arg)
       #-(or clisp sbcl)
       (defmethod hoge-p(arg)
	 (declare(ignore arg))
	 nil)
       'hoge)

; with initform.
#?(defstruct* hoge
    (bar 0)) ; <--- specified.
:expanded-to
(progn (defclass hoge ()
	 ((bar :initform 0 ; <--- This!
	       :initarg :bar :accessor hoge-bar)))
       (defun make-hoge (&rest args)
	 (apply #'make-instance 'hoge args))
       (defgeneric copy-hoge (arg))
       (defmethod copy-hoge ((arg hoge))
	 (let((obj(make-instance (class-of arg))))
	   (dolist(slot (slots<=obj arg))
	     (if(slot-boundp arg slot)
	       (setf (slot-value obj slot)(slot-value arg slot))
	       (slot-makunbound obj slot)))
	   obj))
       #+(or clisp sbcl)
       (defun hoge-p(arg)(typep arg 'hoge))
       #-(or clisp sbcl)
       (defgeneric hoge-p(arg))
       #-(or clisp sbcl)
       (defmethod hoge-p((arg hoge))arg)
       #-(or clisp sbcl)
       (defmethod hoge-p(arg)
	 (declare(ignore arg))
	 nil)
       'hoge)

; Specify type.
#?(defstruct* hoge
    (bar 0 :type integer)) ; <--- specified.
:expanded-to
(progn (defclass hoge ()
	 ((bar :type integer ; <--- This!
	       :initform 0 :initarg :bar :accessor hoge-bar)))
       (defun make-hoge (&rest args)
	 (apply #'make-instance 'hoge args))
       (defgeneric copy-hoge (arg))
       (defmethod copy-hoge ((arg hoge))
	 (let((obj(make-instance (class-of arg))))
	   (dolist(slot (slots<=obj arg))
	     (if(slot-boundp arg slot)
	       (setf (slot-value obj slot)(slot-value arg slot))
	       (slot-makunbound obj slot)))
	   obj))
       #+(or clisp sbcl)
       (defun hoge-p(arg)(typep arg 'hoge))
       #-(or clisp sbcl)
       (defgeneric hoge-p(arg))
       #-(or clisp sbcl)
       (defmethod hoge-p((arg hoge))arg)
       #-(or clisp sbcl)
       (defmethod hoge-p(arg)
	 (declare(ignore arg))
	 nil)
       'hoge)

; Specify read-only T.
#?(defstruct* hoge
    (bar 0 :read-only T)) ; <--- specified.
:expanded-to
(progn (defclass hoge ()
	 ((bar :initform 0 :initarg :bar :reader ; <--- This!
	       hoge-bar)))
       (defun make-hoge (&rest args)
	 (apply #'make-instance 'hoge args))
       (defgeneric copy-hoge (arg))
       (defmethod copy-hoge ((arg hoge))
	 (let((obj(make-instance (class-of arg))))
	   (dolist(slot (slots<=obj arg))
	     (if(slot-boundp arg slot)
	       (setf (slot-value obj slot)(slot-value arg slot))
	       (slot-makunbound obj slot)))
	   obj))
       #+(or clisp sbcl)
       (defun hoge-p(arg)(typep arg 'hoge))
       #-(or clisp sbcl)
       (defgeneric hoge-p(arg))
       #-(or clisp sbcl)
       (defmethod hoge-p((arg hoge))arg)
       #-(or clisp sbcl)
       (defmethod hoge-p(arg)
	 (declare(ignore arg))
	 nil)
       'hoge)

; Conc-name option example.
; Specify no prefix.
#?(defstruct*(hoge(:conc-name nil)) ; <--- Specify.
    bar)
:expanded-to
(progn (defclass hoge()
	 ((bar :initform nil :initarg :bar :accessor bar))) ; <--- This!
       (defun make-hoge (&rest args)
	 (apply #'make-instance 'hoge args))
       (defgeneric copy-hoge (arg))
       (defmethod copy-hoge ((arg hoge))
	 (let((obj(make-instance (class-of arg))))
	   (dolist(slot (slots<=obj arg))
	     (if(slot-boundp arg slot)
	       (setf (slot-value obj slot)(slot-value arg slot))
	       (slot-makunbound obj slot)))
	   obj))
       #+(or clisp sbcl)
       (defun hoge-p(arg)(typep arg 'hoge))
       #-(or clisp sbcl)
       (defgeneric hoge-p(arg))
       #-(or clisp sbcl)
       (defmethod hoge-p((arg hoge))arg)
       #-(or clisp sbcl)
       (defmethod hoge-p(arg)
	 (declare(ignore arg))
	 nil)
       'hoge)

; Same above.
#?(defstruct*(hoge(:conc-name))bar)
:expanded-to
(progn (defclass hoge()
	 ((bar :initform nil :initarg :bar :accessor bar))) ; <--- This!
       (defun make-hoge (&rest args)
	 (apply #'make-instance 'hoge args))
       (defgeneric copy-hoge (arg))
       (defmethod copy-hoge ((arg hoge))
	 (let((obj(make-instance (class-of arg))))
	   (dolist(slot (slots<=obj arg))
	     (if(slot-boundp arg slot)
	       (setf (slot-value obj slot)(slot-value arg slot))
	       (slot-makunbound obj slot)))
	   obj))
       #+(or clisp sbcl)
       (defun hoge-p(arg)(typep arg 'hoge))
       #-(or clisp sbcl)
       (defgeneric hoge-p(arg))
       #-(or clisp sbcl)
       (defmethod hoge-p((arg hoge))arg)
       #-(or clisp sbcl)
       (defmethod hoge-p(arg)
	 (declare(ignore arg))
	 nil)
       'hoge)

; Specify alternative.
#?(defstruct*(hoge(:conc-name hoge-)) ; <--- Specify
    bar)
:expanded-to
(progn (defclass hoge()
	 ((bar :initform nil :initarg :bar :accessor hoge-bar))) ; <--- This!
       (defun make-hoge (&rest args)
	 (apply #'make-instance 'hoge args))
       (defgeneric copy-hoge (arg))
       (defmethod copy-hoge ((arg hoge))
	 (let((obj(make-instance (class-of arg))))
	   (dolist(slot (slots<=obj arg))
	     (if(slot-boundp arg slot)
	       (setf (slot-value obj slot)(slot-value arg slot))
	       (slot-makunbound obj slot)))
	   obj))
       #+(or clisp sbcl)
       (defun hoge-p(arg)(typep arg 'hoge))
       #-(or clisp sbcl)
       (defgeneric hoge-p(arg))
       #-(or clisp sbcl)
       (defmethod hoge-p((arg hoge))arg)
       #-(or clisp sbcl)
       (defmethod hoge-p(arg)
	 (declare(ignore arg))
	 nil)
       'hoge)

; Can use string-designator.
#?(defstruct*(hoge(:conc-name "hoge-")) ; <--- use string.
    bar)
:expanded-to
(progn (defclass hoge()
	 ((bar :initform nil :initarg :bar :accessor |hoge-BAR|))) ; <--- This!
       (defun make-hoge (&rest args)
	 (apply #'make-instance 'hoge args))
       (defgeneric copy-hoge (arg))
       (defmethod copy-hoge ((arg hoge))
	 (let((obj(make-instance (class-of arg))))
	   (dolist(slot (slots<=obj arg))
	     (if(slot-boundp arg slot)
	       (setf (slot-value obj slot)(slot-value arg slot))
	       (slot-makunbound obj slot)))
	   obj))
       #+(or clisp sbcl)
       (defun hoge-p(arg)(typep arg 'hoge))
       #-(or clisp sbcl)
       (defgeneric hoge-p(arg))
       #-(or clisp sbcl)
       (defmethod hoge-p((arg hoge))arg)
       #-(or clisp sbcl)
       (defmethod hoge-p(arg)
	 (declare(ignore arg))
	 nil)
       'hoge)

; Examples of :constructor option.
#?(defstruct*(hoge(:constructor make)) ; <--- Specify.
    bar)
:expanded-to
(progn (defclass hoge()
	 ((bar :initform nil :initarg :bar :accessor hoge-bar)))
       (defun make ; <--- This!
	 (&rest args)
	 (apply #'make-instance 'hoge args))
       (defgeneric copy-hoge (arg))
       (defmethod copy-hoge ((arg hoge))
	 (let((obj(make-instance (class-of arg))))
	   (dolist(slot (slots<=obj arg))
	     (if(slot-boundp arg slot)
	       (setf (slot-value obj slot)(slot-value arg slot))
	       (slot-makunbound obj slot)))
	   obj))
       #+(or clisp sbcl)
       (defun hoge-p(arg)(typep arg 'hoge))
       #-(or clisp sbcl)
       (defgeneric hoge-p(arg))
       #-(or clisp sbcl)
       (defmethod hoge-p((arg hoge))arg)
       #-(or clisp sbcl)
       (defmethod hoge-p(arg)
	 (declare(ignore arg))
	 nil)
       'hoge)

#?(defstruct*(hoge(:constructor)) ; <--- No arg means use default.
    bar)
:expanded-to
(progn (defclass hoge()
	 ((bar :initform nil :initarg :bar :accessor hoge-bar)))
       (defun make-hoge ; <--- This!
	 (&rest args)
	 (apply #'make-instance 'hoge args))
       (defgeneric copy-hoge (arg))
       (defmethod copy-hoge ((arg hoge))
	 (let((obj(make-instance (class-of arg))))
	   (dolist(slot (slots<=obj arg))
	     (if(slot-boundp arg slot)
	       (setf (slot-value obj slot)(slot-value arg slot))
	       (slot-makunbound obj slot)))
	   obj))
       #+(or clisp sbcl)
       (defun hoge-p(arg)(typep arg 'hoge))
       #-(or clisp sbcl)
       (defgeneric hoge-p(arg))
       #-(or clisp sbcl)
       (defmethod hoge-p((arg hoge))arg)
       #-(or clisp sbcl)
       (defmethod hoge-p(arg)
	 (declare(ignore arg))
	 nil)
       'hoge)

#?(defstruct*(hoge(:constructor create-hoge
	            (a &optional b (c 'sea)&rest d &aux e (f 'eff))))
    a b c d e f)
:expanded-to
(progn (defclass hoge()
	 ((a :initform nil :initarg :a :accessor hoge-a)
	  (b :initform nil :initarg :b :accessor hoge-b)
	  (c :initform nil :initarg :c :accessor hoge-c)
	  (d :initform nil :initarg :d :accessor hoge-d)
	  (e :initform nil :initarg :e :accessor hoge-e)
	  (f :initform nil :initarg :f :accessor hoge-f)))
       ;; This!
       (defun create-hoge(a &optional b (c 'sea)&rest d &aux e (f 'eff))
	 (make-instance 'hoge :a a :b b :c c :d d :e e :f f))
       (defgeneric copy-hoge (arg))
       (defmethod copy-hoge ((arg hoge))
	 (let((obj(make-instance (class-of arg))))
	   (dolist(slot (slots<=obj arg))
	     (if(slot-boundp arg slot)
	       (setf (slot-value obj slot)(slot-value arg slot))
	       (slot-makunbound obj slot)))
	   obj))
       #+(or clisp sbcl)
       (defun hoge-p(arg)(typep arg 'hoge))
       #-(or clisp sbcl)
       (defgeneric hoge-p(arg))
       #-(or clisp sbcl)
       (defmethod hoge-p((arg hoge))arg)
       #-(or clisp sbcl)
       (defmethod hoge-p(arg)
	 (declare(ignore arg))
	 nil)
       'hoge)

; Examples of :copier option.
#?(defstruct*(hoge(:copier nil)) ; specify nil.
    bar)
:expanded-to
(progn (defclass hoge ()
	 ((bar :initform nil :initarg :bar :accessor hoge-bar)))
       (defun make-hoge (&rest args)
	 (apply #'make-instance 'hoge args))
       ;; No cpier!
       #+(or clisp sbcl)
       (defun hoge-p(arg)(typep arg 'hoge))
       #-(or clisp sbcl)
       (defgeneric hoge-p(arg))
       #-(or clisp sbcl)
       (defmethod hoge-p((arg hoge))arg)
       #-(or clisp sbcl)
       (defmethod hoge-p(arg)
	 (declare(ignore arg))
	 nil)
       'hoge)

#?(defstruct*(hoge (:copier hoge)) ; specify name.
    bar)
:expanded-to
(progn (defclass hoge ()
	 ((bar :initform nil :initarg :bar :accessor hoge-bar)))
       (defun make-hoge (&rest args)
	 (apply #'make-instance 'hoge args))
       ;; This!
       (defgeneric hoge (arg))
       ;; Also!
       (defmethod hoge ((arg hoge))
	 (let((obj(make-instance (class-of arg))))
	   (dolist(slot (slots<=obj arg))
	     (if(slot-boundp arg slot)
	       (setf (slot-value obj slot)(slot-value arg slot))
	       (slot-makunbound obj slot)))
	   obj))
       #+(or clisp sbcl)
       (defun hoge-p(arg)(typep arg 'hoge))
       #-(or clisp sbcl)
       (defgeneric hoge-p(arg))
       #-(or clisp sbcl)
       (defmethod hoge-p((arg hoge))arg)
       #-(or clisp sbcl)
       (defmethod hoge-p(arg)
	 (declare(ignore arg))
	 nil)
       'hoge)

; Examples of :include option.
#?(defstruct*(hoge (:include foo))fuga)
:expanded-to
(progn (defclass hoge (foo) ; <--- This!
	 ((fuga :initform nil :initarg :fuga :accessor hoge-fuga)))
       (defun make-hoge (&rest args)
	 (apply #'make-instance 'hoge args))
       (defgeneric copy-hoge (arg))
       (defmethod copy-hoge ((arg hoge))
	 (let((obj(make-instance (class-of arg))))
	   (dolist(slot (slots<=obj arg))
	     (if(slot-boundp arg slot)
	       (setf (slot-value obj slot)(slot-value arg slot))
	       (slot-makunbound obj slot)))
	   obj))
       #+(or clisp sbcl)
       (defun hoge-p(arg)(typep arg 'hoge))
       #-(or clisp sbcl)
       (defgeneric hoge-p(arg))
       #-(or clisp sbcl)
       (defmethod hoge-p((arg hoge))arg)
       #-(or clisp sbcl)
       (defmethod hoge-p(arg)
	 (declare(ignore arg))
	 nil)
       ;; These!
       (defgeneric hoge-bar(arg))
       (defmethod hoge-bar((arg hoge))
	 (foo-bar arg))
       (defgeneric (setf hoge-bar)(new arg))
       (defmethod (setf hoge-bar)(new(arg hoge))
	 (setf(foo-bar arg)new))
       'hoge)

; Some :include options are valid.
; Lets say another object bazz.
#?(defstruct* bazz
    (piyo 0 :read-only t))
=> BAZZ
,:lazy nil
,:ignore-signals warning
,:stream nil
#?(defstruct*(hoge (:include foo)
		   (:include bazz)) ; <--- This!
    fuga)
:expanded-to
(progn (defclass hoge (foo bazz) ; <--- This!
	 ((fuga :initform nil :initarg :fuga :accessor hoge-fuga)))
       (defun make-hoge (&rest args)
	 (apply #'make-instance 'hoge args))
       (defgeneric copy-hoge (arg))
       (defmethod copy-hoge ((arg hoge))
	 (let((obj(make-instance (class-of arg))))
	   (dolist(slot (slots<=obj arg))
	     (if(slot-boundp arg slot)
	       (setf (slot-value obj slot)(slot-value arg slot))
	       (slot-makunbound obj slot)))
	   obj))
       #+(or clisp sbcl)
       (defun hoge-p(arg)(typep arg 'hoge))
       #-(or clisp sbcl)
       (defgeneric hoge-p(arg))
       #-(or clisp sbcl)
       (defmethod hoge-p((arg hoge))arg)
       #-(or clisp sbcl)
       (defmethod hoge-p(arg)
	 (declare(ignore arg))
	 nil)
       ;; These!
       (defgeneric hoge-bar(arg))
       (defmethod hoge-bar((arg hoge))
	 (foo-bar arg))
       (defgeneric (setf hoge-bar)(new arg))
       (defmethod (setf hoge-bar)(new(arg hoge))
	 (setf(foo-bar arg)new))
       (defgeneric hoge-piyo(arg))
       (defmethod hoge-piyo((arg hoge))
	 (bazz-piyo arg))
       'hoge)

; Slot description example.
#?(defstruct*(hoge (:include foo (bar 0)))
    fuga)
:expanded-to
(progn (defclass hoge (foo)
	 ((fuga :initform nil :initarg :fuga :accessor hoge-fuga))
	 (:default-initargs :bar 0)) ; <--- This!
       (defun make-hoge (&rest args)
	 (apply #'make-instance 'hoge args))
       (defgeneric copy-hoge (arg))
       (defmethod copy-hoge ((arg hoge))
	 (let((obj(make-instance (class-of arg))))
	   (dolist(slot (slots<=obj arg))
	     (if(slot-boundp arg slot)
	       (setf (slot-value obj slot)(slot-value arg slot))
	       (slot-makunbound obj slot)))
	   obj))
       #+(or clisp sbcl)
       (defun hoge-p(arg)(typep arg 'hoge))
       #-(or clisp sbcl)
       (defgeneric hoge-p(arg))
       #-(or clisp sbcl)
       (defmethod hoge-p((arg hoge))arg)
       #-(or clisp sbcl)
       (defmethod hoge-p(arg)
	 (declare(ignore arg))
	 nil)
       (defgeneric hoge-bar(arg))
       (defmethod hoge-bar((arg hoge))
	 (foo-bar arg))
       (defgeneric (setf hoge-bar)(new arg))
       (defmethod (setf hoge-bar)(new(arg hoge))
	 (setf(foo-bar arg)new))
       'hoge)

; Examples of :predicate option.
#?(defstruct*(hoge(:predicate nil)) ; specify nil.
    bar)
:expanded-to
(progn (defclass hoge ()
	 ((bar :initform nil :initarg :bar :accessor hoge-bar)))
       (defun make-hoge (&rest args)
	 (apply #'make-instance 'hoge args))
       (defgeneric copy-hoge (arg))
       (defmethod copy-hoge ((arg hoge))
	 (let((obj(make-instance (class-of arg))))
	   (dolist(slot (slots<=obj arg))
	     (if(slot-boundp arg slot)
	       (setf (slot-value obj slot)(slot-value arg slot))
	       (slot-makunbound obj slot)))
	   obj))
       ;; No predicate!
       'hoge)

#?(defstruct*(hoge (:predicate hoge)) ; specify name.
    bar)
:expanded-to
(progn (defclass hoge ()
	 ((bar :initform nil :initarg :bar :accessor hoge-bar)))
       (defun make-hoge (&rest args)
	 (apply #'make-instance 'hoge args))
       (defgeneric copy-hoge (arg))
       (defmethod copy-hoge ((arg hoge))
	 (let((obj(make-instance (class-of arg))))
	   (dolist(slot (slots<=obj arg))
	     (if(slot-boundp arg slot)
	       (setf (slot-value obj slot)(slot-value arg slot))
	       (slot-makunbound obj slot)))
	   obj))
       ;; This!
       #+(or clisp sbcl)
       (defun hoge(arg)(typep arg 'hoge))
       #-(or clisp sbcl)
       (defgeneric hoge(arg))
       #-(or clisp sbcl)
       (defmethod hoge((arg hoge))arg)
       #-(or clisp sbcl)
       (defmethod hoge(arg)
	 (declare(ignore arg))
	 nil)
       'hoge)

; :print-function
#?(defstruct*(hoge(:print-function printer)) ; <--- specify
    bar)
:expanded-to
(progn (defclass hoge ()
	 ((bar :initform nil :initarg :bar :accessor hoge-bar)))
       (defun make-hoge (&rest args)
	 (apply #'make-instance 'hoge args))
       ;; This!
       (defmethod print-object ((obj hoge)stream)
	 (printer obj stream *print-level*))
       (defgeneric copy-hoge (arg))
       (defmethod copy-hoge ((arg hoge))
	 (let((obj(make-instance (class-of arg))))
	   (dolist(slot (slots<=obj arg))
	     (if(slot-boundp arg slot)
	       (setf (slot-value obj slot)(slot-value arg slot))
	       (slot-makunbound obj slot)))
	   obj))
       #+(or clisp sbcl)
       (defun hoge-p(arg)(typep arg 'hoge))
       #-(or clisp sbcl)
       (defgeneric hoge-p(arg))
       #-(or clisp sbcl)
       (defmethod hoge-p((arg hoge))arg)
       #-(or clisp sbcl)
       (defmethod hoge-p(arg)
	 (declare(ignore arg))
	 nil)
       'hoge)

; :print-object
#?(defstruct*(hoge(:print-object printer)) ; <--- specify
    bar)
:expanded-to
(progn (defclass hoge ()
	 ((bar :initform nil :initarg :bar :accessor hoge-bar)))
       (defun make-hoge (&rest args)
	 (apply #'make-instance 'hoge args))
       ;; This!
       (defmethod print-object ((obj hoge)stream)
	 (printer obj stream))
       (defgeneric copy-hoge (arg))
       (defmethod copy-hoge ((arg hoge))
	 (let((obj(make-instance (class-of arg))))
	   (dolist(slot (slots<=obj arg))
	     (if(slot-boundp arg slot)
	       (setf (slot-value obj slot)(slot-value arg slot))
	       (slot-makunbound obj slot)))
	   obj))
       #+(or clisp sbcl)
       (defun hoge-p(arg)(typep arg 'hoge))
       #-(or clisp sbcl)
       (defgeneric hoge-p(arg))
       #-(or clisp sbcl)
       (defmethod hoge-p((arg hoge))arg)
       #-(or clisp sbcl)
       (defmethod hoge-p(arg)
	 (declare(ignore arg))
	 nil)
       'hoge)

