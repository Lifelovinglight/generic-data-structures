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
    :documentation "The contents of the quad-tree.")
   (default
     :initform nil
     :initarg :default
     :type *
     :accessor quad-tree-default
     :documentation "The content of the empty regions of the quad-tree."))
  (:documentation "A quad-tree implementation."))

(declaim
 (type (simple-array fixnum 1) *2pow*)
 (ftype (function (fixnum *) quad-tree) make-quadtree)
 (ftype (function (fixnum fixnum quad-tree) *) quadtree-get))

(defparameter *2pow*
  (make-array '(61)
	      :element-type 'fixnum
	      :initial-contents (cons 0 (loop for n from 0 to 59
					   collect (fixnum-math:pow 2 n)))))

(defun make-quad-tree (depth arg)
  "A constructor for a quad-tree."
  (make-instance 'quad-tree :depth depth :default arg))

(assert (let ((test-quad-tree (make-quad-tree 2 t)))
	  (and (= 2 (the fixnum (quad-tree-depth test-quad-tree)))
	       (quad-tree-default test-quad-tree)
	       (equal (cons nil (cons nil (cons nil nil)))
		      (quad-tree-contents test-quad-tree)))))

(defun quadtree-get (x y quadtree)
  (labels ((inner (x y depth pow2 tree default)
	     (declare (type (simple-array fixnum 1) pow2)
		      (fixnum x y depth)
		      (cons tree))
	     (if (>= x 0)
		 (if (>= y 0)
		     (if (> depth 0)
			 (inner (- x (the fixnum (aref pow2 depth)))
				(- y (the fixnum (aref pow2 depth)))
				(1- depth)
				pow2 (caaaar tree) default)
			 (caaaar tree))
		     (if (> depth 0)
			 (inner (- x (the fixnum (aref pow2 depth)))
				y
				(1- depth)
				pow2 (caaar tree) default)
			 (caaar tree)))
		 (if (>= y 0)
		     (if (> depth 0)
			 (inner x
				(- y (the fixnum (aref pow2 depth)))
				(1- depth)
				pow2 (car tree) default)
			 (car tree))
		     (if (> depth 0)
			 (inner x
				y
				(1- depth)
				pow2 (caar tree) default)
			 (caar tree))))))
    (inner x y (quad-tree-depth quadtree) *2pow*
	   (quad-tree-contents quadtree) (quad-tree-default quadtree))))
		     
		 
