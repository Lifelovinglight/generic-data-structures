(defpackage :octree
  (:use :common-lisp)
  (:export :octree
	   :make-octree)
  (:documentation "An octree implementation."))

(declaim
 (optimize
  (compilation-speed 0)
  (debug 0)
  (safety 0)
  (space 2)
  (speed 3)))

(defclass quad-tree ()
  ((

(declaim
 (ftype (function () cons) make-quadtree)
 (ftype (function (fixnum fixnum cons *) quadtree-get)))

(defun make-quadtree ()
  (cons nil (cons nil (cons nil nil))))

(defun quadtree-get (x y quadtree depth default-value)
  (if (pow 2 depth) 
