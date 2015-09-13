(defpackage :queue
  (:use :common-lisp)
  (:export :queue
	   :queue-data
	   :queue-rear
	   :make-queue
	   :enqueue
	   :dequeue)
  (:documentation "A queue implementation."))

(in-package :queue)

(declaim
 (optimize
  (compilation-speed 0)
  (debug 3)
  (safety 3)
  (space 2)
  (speed 2)))

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
    :type cons
    :documentation "The rear of the queue."))
  (:documentation "A queue implementation."))

(defgeneric enqueue (queue arg))

(defgeneric dequeue (queue))

(declaim (ftype (function (list) queue) make-queue))

(defun make-queue (initial-data)
  "Constructor for a queue."
  (make-instance 'queue :data initial-data :rear (last initial-data)))

(assert (let ((test-queue (make-queue (list 1 2 3))))
	  (and (equal (list 1 2 3) (queue-data test-queue))
	       (= 3 (car (queue-rear test-queue))))))

(defmethod enqueue ((queue queue) arg)
  "Add an item to the rear of a queue."
  (setf (cdr (queue-rear queue)) (cons arg nil))
  (setf (queue-rear queue) (cdr (queue-rear queue))))

(assert (let ((test-queue (make-queue (list 1 2))))
	  (enqueue test-queue 3)
	  (equal (list 1 2 3) (queue-data test-queue))))

(defmethod dequeue ((queue queue))
  "Remove an item from the front of a queue."
  (pop (queue-data queue)))

(assert (let ((test-queue (make-queue (list 1 2 3))))
	  (and (= 1 (dequeue test-queue))
	       (equal (list 2 3) (queue-data test-queue)))))
