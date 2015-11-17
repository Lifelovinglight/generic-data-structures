(defpackage :queue
  (:use :common-lisp)
  (:export :queue
	   :queue-data
	   :queue-rear
	   :enqueue
	   :dequeue)
  (:documentation "A queue implementation."))

(in-package :queue)

(defclass queue ()
  ((queue-data
    :initform nil
    :initarg :data
    :accessor queue-data
    :type list
    :documentation "The contents of the queue.")
   (queue-rear
    :initform nil
    :initarg :rear
    :accessor queue-rear
    :documentation "The rear of the queue."))
  (:documentation "A queue implementation."))

(defgeneric enqueue (queue arg))

(defgeneric dequeue (queue))

(defmethod initialize-instance :after ((this-queue queue) &key)
	   (setf (queue-rear this-queue) (last (queue-data this-queue))))

(defmethod enqueue ((queue queue) arg)
  "Add an item to the rear of a queue."
  (setf (cdr (queue-rear queue)) (cons arg nil))
  (setf (queue-rear queue) (cdr (queue-rear queue))))

(defmethod dequeue ((queue queue))
  "Remove an item from the front of a queue."
  (pop (queue-data queue)))
