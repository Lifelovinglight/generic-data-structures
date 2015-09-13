(defpackage :fixnum-math
  (:use :common-lisp)
  (:export :pow
	   :add
	   :sum
	   :mul
	   :inc
	   :dec)
  (:documentation "A collection of fixed-arity fixnum-only math functions for use in certain highly optimized data structures."))

(in-package :fixnum-math)

(declaim
 (optimize
  (compilation-speed 0)
  (debug 0)
  (safety 0)
  (space 2)
  (speed 3)))

(declaim
 (inline pow add sum mul dec inc)
 (ftype (function (fixnum) fixnum)
	dec inc)
 (ftype (function (fixnum fixnum) fixnum)
	pow add mul)
 (ftype (function (list) fixnum) sum))

(defun dec (x)
  "FIXNUM decrementation of X."
  (the fixnum (1- x)))

(defun inc (x)
  "FIXNUM incrementation of X."
  (the fixnum (1+ x)))

(defun pow (x y)
  "FIXNUM X to the power of Y."
  (labels ((inner (x n r)
	     (declare (fixnum x n r))
	      (if (zerop n)
		  r
		  (inner x (1- n) (mul r x)))))
    (inner x y x)))

(defun add (x y)
  "FIXNUM addition of X and Y."
  (the fixnum (+ x y)))

(defun sum (ln)
  "FIXNUM summation of the LIST LN."
  (labels ((inner (ln r)
	     (declare (list ln)
		      (fixnum r))
	     (if (null ln)
		 r
		 (inner (cdr ln)
			(add r (car ln))))))
    (inner ln 0)))

(defun mul (x y)
  "FIXNUM product of X and Y."
  (the fixnum (* x y)))
