;;;; -*- Mode:Common-Lisp; Package:user; Base:10 -*-
;;; ---------------------------------------------------------------------------
;;;
;;; The Simple Blackboard (SBB) system and a trivial application. Although
;;; this application demonstrates basic aspects of a real blackboard
;;; application, the SBB system is *far* too simple and inefficient to be used
;;; for any serious work.
;;;
;;; Placed in the Public Domain by Daniel D. Corkill, 1991.
;;;
;;; ---------------------------------------------------------------------------
(defvar *blackboard* nil
"*BLACKBOARD*
Contains the blackboard database, which is simply a list of blackboard objects
stored by name. (A particularly poor choice!)")
;;; ---------------------------------------------------------------------------
(defvar *events* nil
"*EVENTS*
Contains the events generated during each KSA execution. The events are
buffered until execution of the KSA is completed. Then the events are
processed by the control components.")
;;; ---------------------------------------------------------------------------
(defvar *agenda* nil
"*AGENDA*
Contains the list of activated KSs awaiting execution.")
;;; ---------------------------------------------------------------------------
(defvar *creation-event-kss* nil
"*CREATION-EVENT-KSS*
Contains KS specifications for all KSs that are interested in blackboard-object
creation events (which is the only type of events supported). The
define-creation-ks macro manages this list.")
;;; ---------------------------------------------------------------------------
(defvar *trace-level* 3
"*TRACE-LEVEL*
Contains the current trace level. Trace levels from 0 (none) to 3 (highest)
are supported.")
;;; ---------------------------------------------------------------------------
(defstruct (ks-spec (:conc-name "KS-SPEC."))
"KS-SPEC (Structure)
Contains the information about a KS needed by the control machinery."
object-types ; a list of object types of interest
ks-function) ; the name of the function implementing the KS
;;; ---------------------------------------------------------------------------
(defstruct (bb-object (:type list)
(:conc-name "BB-OBJECT.")
;; We define our own constructor below
(:constructor nil))
"BB-OBJECT (List)
Contains the name and data of a blackboard object."
name ; the name of the object
data) ; the object data
;;; ---------------------------------------------------------------------------
(defun reset-bb-system ()
"RESET-BB-SYSTEM
Resets the system to the initial state."
(setf *blackboard* nil)
(setf *events* nil)
(setf *agenda* nil))
;;; ---------------------------------------------------------------------------
(defun undefine-all-kss ()
"UNDEFINE-ALL-KSS
Removes all KS definitions."
(setf *creation-event-kss* nil))
;;; ---------------------------------------------------------------------------
(defun signal-creation-event (bb-object)
"SIGNAL-CREATION-EVENT bb-object
Signals that ’bb-object’ has been created."
(push ‘(creation-event ,(bb-object.data bb-object)) *events*))
;;; ---------------------------------------------------------------------------
(defun make-bb-object (name data)
"MAKE-BB-OBJECT name data
Makes ’object’ a blackboard object with name ’name’ and signals a creation
event. ’Name’ must be a symbol (a very poor choice)."
(let ((bb-obj (list name data)))
(when (> *trace-level* 2)
(format t "~&~5tCreating ~a object: ~a~%" (type-of data) bb-obj))
(push bb-obj *blackboard*)
(signal-creation-event bb-obj)
bb-obj))
;;; ---------------------------------------------------------------------------
(defun get-bb-object (name)
"GET-BB-OBJECT name
A trivial means of retrieving a blackboard object by name. ’Name’ must be a
symbol. This function is not used in the application below."
(bb-object.data (find name *blackboard* :key #’bb-object.name :test #’eq)))
;;; ---------------------------------------------------------------------------
(defun creation-event (bb-object)
"CREATION-EVENT bb-object
Control component code for processing a creation event. Determines which KSs
are interested in the event and adds them to the agenda. Does not evaluate
the relative importance of activated KSs."
(let ((bb-object-type (type-of bb-object)))
(dolist (ks-spec *creation-event-kss*)
(when (find bb-object-type (ks-spec.object-types ks-spec))
(let* ((ks (ks-spec.ks-function ks-spec))
(ksa ‘(,ks ,bb-object)))
(when (> *trace-level* 1)
(format t "~&~5tActivating ~a~%" ksa))
(push ksa *agenda*))))))
;;; ---------------------------------------------------------------------------
(defun control-loop ()
"CONTROL-LOOP
A trivial control loop. No opportunistic control is performed -- simply
last-in, first-out scheduling.
The loop terminates when the agenda is empty."
(loop
;; process events:
(dolist (event *events*)
(eval event))
(setf *events* nil)
;; check for stopping condition:
(unless *agenda*
(format t "~2&Agenda is empty. Stopping.~%")
(return-from control-loop (values)))
;; run the top KSA:
(let ((ksa (pop *agenda*)))
(when (> *trace-level* 0)
(format t "~&~5tRunning: ~a~%" ksa))
;; Note that use of eval is a very poor choice here:
(eval ksa))))
;;; ---------------------------------------------------------------------------
(defmacro define-creation-ks (ks obj-types arglist &body body)
"DEFINE-CREATION-KS ks obj-types arglist &body body
Defines KSs interested in creation events. ’KS’ must be a symbol and is the
name to be given to the created KS function. ’Obj-types’ is a list of the
types of objects for which creation events are of interest to the KS. ’Arglist’
and ’body’ are as per normal Common Lisp functions."
‘(progn
;; remove any existing definitions:
(setf *creation-event-kss*
(delete ’,ks *creation-event-kss* :key #’ks-spec.ks-function))
;; add the new definition:
(push (make-ks-spec :object-types ’,obj-types
:ks-function ’,ks)
*creation-event-kss*)
;; define the function:
(defun ,ks ,arglist
,@body)))
;;; ---------------------------------------------------------------------------
;;;
;;; A simple "blackboard" application that generates integer values,
;;; computes their squares, and prints the squares.
;;;
;;; ---------------------------------------------------------------------------
(defparameter *stop-value* 25
"*STOP-VALUE*
Specifies the last integer generated by the generate-integers KS.")
;;; ---------------------------------------------------------------------------
(defstruct (integer-object
(:conc-name "INTEGER-OBJECT.")
(:print-function
(lambda (object stream depth)
(declare (ignore depth))
(let ((*print-structure* nil))
(format stream "#<integer-object ~D>"
(integer-object.value object))))))
"INTEGER-OBJECT (Structure)
A blackboard object containing a generated integer."
value
square)
;;; ---------------------------------------------------------------------------
(defstruct (square-object
(:conc-name "SQUARE-OBJECT.")
(:print-function
(lambda (object stream depth)
(declare (ignore depth))
(let ((*print-structure* nil))
(format stream "#<square-object ~D>"
(square-object.value object))))))
"SQUARE-OBJECT (Structure)
A blackboard object containing a squared integer."
value
integer)
;;; ---------------------------------------------------------------------------
;;;
;;; The KS Definitions:
;;;
;;; Note: Because the SBB control scheme implements a simple LIFO ordering and
;;; the KSs interested in a single type of event are activated in the
;;; order in which they appear in the *creation-event-kss* list,
;;; changing the order of definitions below will change the behavior of
;;; the application.
;;;
;;; ---------------------------------------------------------------------------
(define-creation-ks compute-squares (integer-object) (bb-obj)
"COMPUTE-SQUARES bb-obj
Defines a KS that computes the square of its ’bb-obj’
argument.
This KS is interested only in integer-object creation
events."
(let* ((value (integer-object.value bb-obj))
(square-obj (make-square-object :value (* value value))))
(make-bb-object (gensym) square-obj)
(setf (square-object.integer square-obj) bb-obj)
(setf (integer-object.square bb-obj) square-obj)))
;;; ---------------------------------------------------------------------------
(define-creation-ks generate-integers (integer-object) (bb-obj)
"COMPUTE-SQUARES bb-obj
Defines a KS that creates a new integer-object with a value that is 1 larger
than its ’bb-obj’ argument. Creation stops when the value exceeds *stop-value*.
This KS is interested only in integer-object creation events."
(when (< (integer-object.value bb-obj) *stop-value*)
(make-bb-object
(gensym)
(make-integer-object :value (1+ (integer-object.value bb-obj))))))
;;; ---------------------------------------------------------------------------
(define-creation-ks print-squares (square-object) (bb-obj)
"PRINT-SQUARES bb-obj
Defines a KS that prints the value of the created square-object (contained in
the ’bb-obj’ argument).
This KS is interested only in square-object creation events."
(format t "~&** Square: ~d~%" (square-object.value bb-obj)))
;;; ---------------------------------------------------------------------------
(defun run-application ()
"RUN-APPLICATION
The top-level application function that runs (and reruns) the simple
application."
(reset-bb-system)
(make-bb-object (gensym) (make-integer-object :value 1))
(control-loop))
;;; ---------------------------------------------------------------------------
;;; End of File
;;; ---------------------------------------------------------------------------
