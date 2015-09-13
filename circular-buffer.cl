(defpackage :circular-buffer
  (:use :common-lisp)
  (:export :circular-buffer
	   :make-circular-buffer
	   :circular-buffer-push
	   :circular-buffer-pop
	   :circular-buffer-resize)
  (:documentation "A module implementing a circular buffer."))

(in-package :circular-buffer)

(defclass circular-buffer ()
  ((circular-buffer-contents
    :initform nil
    :initarg :contents
    :accessor circular-buffer-contents
    :type list
    :documentation "The contents of the circular buffer.")
   (circular-buffer-max-items
    :initform 1
    :initarg max-items
    :accessor circular-buffer-max-items
    :type (integer 1 *)
    :documentation "The max number of items in the circular buffer."))
  (:documentation "A circular buffer"))

(defgeneric circular-buffer-push (circular-buffer arg))

(defgeneric circular-buffer-pop (circular-buffer))

(defgeneric circular-buffer-resize (circular-buffer new-size))

(declaim
 (ftype (function (list) list) circular-list)
 (ftype (function (list integer) circular-buffer) make-circular-buffer))

(defun circular-list (arg)
  "Create a circular list."
  (setf (cdr (last arg)) arg))

(assert (= 1 (nth 3 (circular-list (list 1 2 3)))))

(defun make-circular-buffer (initial-data max-items)
  (let ((data-length (length initial-data)))
    (assert (>= max-items data-length))
    (let ((empty-space (loop repeat (- max-items data-length) collect nil)))
      (make-instance 'circular-buffer
		     :contents (circular-list (append empty-space initial-data))))))

(assert (let ((test-circular-buffer (make-circular-buffer (list 1 2 3) 3)))
	  (and (equal (circular-list (list 1 2 3))
		      (circular-buffer-contents test-circular-buffer))
	       (= 3 (circular-buffer-max-items test-circular-buffer)))))

(defmethod circular-buffer-push ((circular-buffer circular-buffer) arg)
  (let ((contents (circular-buffer-contents circular-buffer)))
    (setf (cdr contents) (cons arg (cddr contents)))))

(assert (let ((test-circular-buffer (make-circular-buffer (list 1 2 3) 4)))
	  (and (progn (circular-buffer-push test-circular-buffer 4)
		      (equal (circular-list (list 4 1 2 3))
			     (circular-buffer-contents test-circular-buffer)))
	       (progn (circular-buffer-push test-circular-buffer 5)
		      (equal (circular-list (list 5 4 1 2))
			     (circular-buffer-contents test-circular-buffer))))))

(defmethod circular-buffer-pop ((circular-buffer circular-buffer))
  (let ((retval (copy-list (list (car (circular-buffer-contents circular-buffer))))))
    (setf (circular-buffer-contents circular-buffer)
	  (
