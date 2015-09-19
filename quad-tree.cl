(defpackage :quad-tree
  (:use :common-lisp)
  (:export :quad-tree
	   :make-quad-tree)
  (:documentation "A quad-tree implementation."))

(declaim
 (optimize
  (compilation-speed 0)
  (debug 0)
  (safety 0)
  (space 2)
  (speed 3)))


(defclass quad-tree ()
  ((depth
    :initform 1
    :initarg :depth
    :type fixnum
    :accessor quad-tree-depth
    :documentation "The depth of the quad-tree.")
   (contents
    :initform (cons nil (cons nil (cons nil nil)))
    :initarg :contents
    :type cons
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
	      :initial-contents (cons 0 (loop for n from 0 to 59
					   collect (expt 2 n)))))

(defun make-quad-tree (depth)
  "A constructor for a quad-tree."
  (make-instance 'quad-tree :depth depth))

(assert (let ((test-quad-tree (make-quad-tree 2)))
	  (and (= 2 (the fixnum (quad-tree-depth test-quad-tree)))
	       (equal (cons nil (cons nil (cons nil nil)))
		      (quad-tree-contents test-quad-tree)))))

(defun quadtree-set (x y quadtree arg)
  (labels ((inner (x y depth pow2 tree arg)
	     (declare (type (simple-array fixnum 1) pow2)
		      (fixnum x y depth))
	     (if (>= x 0)
		 (if (>= y 0)
		     (if (= 1 depth) 
			 (setf (cdddr tree) arg)
			 (inner (- x (the fixnum (aref pow2 depth)))
				(- y (the fixnum (aref pow2 depth)))
				(1- depth)
				pow2 (if (null (cdddr tree))
					 (progn (setf (cdddr tree) (cons nil (cons nil (cons nil nil))))
						(cdddr tree))
					 (cdddr tree)) arg))
		     (if (= 1 depth)
			 (setf (caddr tree) arg)
			 (inner (- x (the fixnum (aref pow2 depth)))
				y
				(1- depth)
				pow2 (if (null (caddr tree))
					 (progn (setf (caddr tree) (cons nil (cons nil (cons nil nil))))
						(caddr tree))
					 (caddr tree)) arg)))
		 (if (>= y 0)
		     (if (= 1 depth)
			 (setf (car tree) arg)
			 (inner x
				(- y (the fixnum (aref pow2 depth)))
				(1- depth)
				pow2 (if (null (car tree))
					 (progn (setf (car tree) (cons nil (cons nil (cons nil nil))))
						(car tree))
					 (car tree)) arg))
		     (if (= 1 depth)
			 (setf (cadr tree) arg)
			 (inner x
				y
				(1- depth)
				pow2 (if (null (cadr tree))
					 (progn (setf (cadr tree) (cons nil (cons nil (cons nil nil))))
						(cadr tree))
					 (cadr tree)) arg))))))
    (inner x y (quad-tree-depth quadtree) *2pow*
	   (quad-tree-contents quadtree) arg)))

(defun quadtree-get (x y quadtree)
  (labels ((inner (x y depth pow2 tree)
	     (declare (type (simple-array fixnum 1) pow2)
		      (fixnum x y depth))
	     (if (null tree)
		 nil
		 (if (zerop depth)
		     tree
		     (if (>= x 0)
			 (if (>= y 0)
			     (inner (- x (the fixnum (aref pow2 depth)))
				    (- y (the fixnum (aref pow2 depth)))
				    (1- depth)
				    pow2 (cdddr tree))
			     (inner (- x (the fixnum (aref pow2 depth)))
				    y
				    (1- depth)
				    pow2 (caddr tree)))
			 (if (>= y 0)
			     (inner x
				    (- y (the fixnum (aref pow2 depth)))
				    (1- depth)
				    pow2 (car tree))
			     (inner x
				    y
				    (1- depth)
				    pow2 (cadr tree))))))))
    (inner x y (quad-tree-depth quadtree) *2pow*
	   (quad-tree-contents quadtree))))
		 
(assert (equal 4 (let ((test-quad-tree (make-quad-tree 2)))
		   (quadtree-set 1 1 test-quad-tree 4)
		   (quadtree-get 1 1 test-quad-tree))))
