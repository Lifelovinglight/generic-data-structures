(defpackage :quad-tree
  (:use :common-lisp
	:fixnum-math)
  (:export :quad-tree
	   :make-quad-tree)
  (:documentation "A quad-tree implementation."))

(declaim
 (optimize
  (compilation-speed 0)
  (debug 3)
  (safety 3)
  (space 2)
  (speed 2)))


(defclass quad-tree ()
  ((depth
    :initform 1
    :initarg :depth
    :type fixnum
    :accessor quad-tree-depth
    :documentation "The depth of the quad-tree.")
   (contents
    :initform (list nil nil nil)
    :initarg :contents
    :type list
    :accessor quad-tree-contents
    :documentation "The contents of the quad-tree."))
  (:documentation "A quad-tree implementation."))

(declaim
 (type (simple-array fixnum 1) *2pow*)
 (ftype (function (fixnum *) quad-tree) make-quadtree)
 (ftype (function (fixnum fixnum quad-tree *) *) quadtree-set)
 (ftype (function (fixnum fixnum quad-tree) *) quadtree-get))

(defparameter *2pow*
  (make-array '(61)
	      :element-type 'fixnum
	      :initial-contents (cons 0 (cons 1 (loop for n from 0 to 58
					   collect (fixnum-math:pow 2 n))))))

(defun make-quad-tree (depth)
  "A constructor for a quad-tree."
  (make-instance 'quad-tree :depth depth))

(assert (let ((test-quad-tree (make-quad-tree 2)))
	  (and (= 2 (the fixnum (quad-tree-depth test-quad-tree)))
	       (equal (list nil nil nil)
		      (quad-tree-contents test-quad-tree)))))

(defun quadtree-set (x y quadtree arg)
  (labels ((inner (x y depth pow2 tree arg)
	     (declare (type (simple-array fixnum 1) pow2)
		      (fixnum x y depth))
	     (if (>= x 0)
		 (if (>= y 0)
		     (if (> depth 0)
			 (inner (- x (the fixnum (aref pow2 depth)))
				(- y (the fixnum (aref pow2 depth)))
				(1- depth)
				pow2 (if (null tree)
					 (progn (setf tree (list nil nil nil))
						(caaaar tree))
					 (caaaar tree)) arg)
			 (setf tree arg))
		     (if (> depth 0)
			 (inner (- x (the fixnum (aref pow2 depth)))
				y
				(1- depth)
				pow2 (if (null tree)
					 (progn (setf tree (list nil nil nil))
						(caaar tree))
					 (caaar tree)) arg)
			 (setf tree arg)))
		 (if (>= y 0)
		     (if (> depth 0)
			 (inner x
				(- y (the fixnum (aref pow2 depth)))
				(1- depth)
				pow2 (if (null tree)
					 (progn (setf tree (list nil nil nil))
						(car tree))
					 (car tree)) arg)
			 (setf tree arg))
		     (if (> depth 0)
			 (inner x
				y
				(1- depth)
				pow2 (if (null tree)
					 (progn (setf tree (list nil nil nil))
						(caar tree))
					 (caar tree)) arg)
			 (setf tree arg))))))
    (inner x y (quad-tree-depth quadtree) *2pow*
	   (quad-tree-contents quadtree) arg)))

(defun quadtree-get (x y quadtree)
  (labels ((inner (x y depth pow2 tree)
	     (declare (type (simple-array fixnum 1) pow2)
		      (fixnum x y depth))
	     (if (null tree)
		 nil
		 (if (>= x 0)
		     (if (>= y 0)
			 (if (> depth 0)
			     (inner (- x (the fixnum (aref pow2 depth)))
				    (- y (the fixnum (aref pow2 depth)))
				    (1- depth)
				    pow2 (caaaar tree))
			     (caaaar tree))
			 (if (> depth 0)
			     (inner (- x (the fixnum (aref pow2 depth)))
				    y
				    (1- depth)
				    pow2 (caaar tree))
			     (caaar tree)))
		     (if (>= y 0) 
			 (if (> depth 0)
			     (inner x
				    (- y (the fixnum (aref pow2 depth)))
				    (1- depth)
				    pow2 (car tree))
			     (car tree))
			 (if (> depth 0)
			     (inner x
				    y
				    (1- depth)
				    pow2 (caar tree))
			     (caar tree)))))))
    (inner x y (quad-tree-depth quadtree) *2pow*
	   (quad-tree-contents quadtree))))
		 
(assert (let ((test-quad-tree (make-quad-tree 2)))
	       (quadtree-set 1 1 test-quad-tree 4)
	       (= 4 (quadtree-get 1 1 test-quad-tree))))
