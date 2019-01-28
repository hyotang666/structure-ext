# STRUCTURE-EXT.AS-CLASS 0.0.0 - Defclass by defstruct syntax.

## Current lisp world
Common Lisp has strong structure and class.

## Issues
In Common Lisp comunity, subject 'structure vs class' is issued sometimes.

Only structure can do is nothing.
Only class can do is Multiple inheritance. 

NOTE - Here, we ignores the thing which can not do but there is a way.
E.g. class is unreadable, but we can write reader macro.
E.g. structure does not have shared slot, but we can emulate it with symbol-plist.

So roughly, we can say 'if you need multiple inheritance, use class. otherwise structure. Because structure is fast.'

```Lisp
(defstruct foo slot)
(defclass bar()((slot :initarg :slot :accessor bar-slot)))

;; constructor
(time(dotimes(x 1000)(make-foo :slot 0)))
=> 299,098 processor cycles
   16,376 bytes consed
(time(dotimes(x 1000)(make-instance 'bar :slot 0)))
=> 19,777,048 processor cycles
   187,400 bytes consed
;; 66 times faster, 11.5 times less consed.

;; accessor
(let((s(make-foo :slot 0)))
  (time(dotimes(x 1000)(foo-slot s))))
=> 26,771 processor cycles
(let((c(make-instance 'bar :slot 0)))
  (time(dotimes(x 1000)(bar-slot c))))
=> 143,488 processor cycles
;; 5.3 times faster.
```

## Proposal
DEFSTRUCT\* is almost same syntax with CL:DEFSTRUCT, but expanded into DEFCLASS and DEFMETHOD forms.
At first, start to write your application with CL:DEFSTRUCT.
If you need multiple inheritance, change CL:DEFSTRUCT to DEFSTRUCT\*, and add super class as :include option.
DEFSTRUCT\* can accept multiple :include options.

## Constraints
### Invalid options
Option :type, :named, and :initial-offset are invalid in DEFSTRUCT\*

### Uncompleteness of copier
The copy function which is made by DEFSTRUCT\* has some constraints.

* Any slots has CL:ERROR initform.
* All slots are not specified :READ-ONLY T.

Because, copier evaluates `CL:MAKE-INSTANCE` without any initargs,
then `CL:SETF` each slots.

## Usage

## From developer

* Product's goal - already?
* License - MIT
### Tested
SBCL/1.4.15
CCL/1.11.5

