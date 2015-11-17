(defpackage :simple-xml
  (:use :common-lisp)
  (:export :parse-xml
	   :tokenize-xml))

(in-package :simple-xml)

(locally
    (declaim
     (optimize (speed 3)
	       (space 2)
	       (safety 2)
	       (debug 1)
	       (compilation-speed 0)))

  (declaim
   (ftype (function (list list) list) parse-xml))

  (defun whitespacep (char)
    (member char '(#\Space #\Tab #\Newline #\Return)))

  (defun letterp (char)
    (member char (coerce "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" 'list)))

  (defun open-bracketp (char)
    (eq char #\<))

  (defun question-markp (char)
    (eq char #\?))

  (defun equalsp (char)
    (eq char #\=))

  (defun double-quotep (char)
    (eq char #\"))

  (defun closing-bracketp (char)
    (eq char #\>))

  (defun forward-slashp (char)
    (eq char #\/))

  (defun produce-string (input &optional (stack (list 'empty-stack)))
    (if (symbolp (car input))
	(cons
	 (coerce (butlast stack) 'string)
	 input)
	(produce-string (cdr input) (cons (car input) stack))))
  
  (defun produce-symbol (input &optional (stack (list 'empty-stack)))
    (if (symbolp (car input))
	(cons
	 (intern (string-upcase (coerce (butlast stack) 'string)))
	 input)
	(produce-symbol (cdr input) (cons (car input) stack))))

  (defun tokenize-tag-data (input stack)
    (if (null input)
	(error "EOF in tag data string.")
	(if (open-bracketp (car input))
	    (values input (produce-string stack))
	    (tokenize-tag-data (cdr input) (cons (car input) stack)))))
  
  (defun tokenize-tag-value (input stack)
    (if (null input)
	(error "EOF in tag value string.")
	(if (double-quotep (car input))
	    (values (cdr input) (produce-string stack))
	    (tokenize-tag-value (cdr input) (cons (car input) stack)))))
  
  (defun tokenize-symbol (input stack)
    (if (null input)
	(error "EOF in symbol.")
	(if (letterp (car input))
	    (tokenize-symbol (cdr input) (cons (car input) stack))
	    (values input (produce-symbol stack)))))
  
  (defun tokenize-xml (input &optional (stack (list 'empty-stack)))
    (if (null input)
	(cdr (reverse stack))
	(cond
	  ((whitespacep (car input))
	   (tokenize-xml (cdr input) stack))
	  ((open-bracketp (car input))
	   (tokenize-xml (cdr input) (cons :xml-bracket-open stack)))
	  ((question-markp (car input))
           (if (eq :xml-bracket-open (car stack))
	       (tokenize-xml (cdr input) (cons :xml-metadata-begin (cdr stack)))
	       (tokenize-xml (cdr input) (cons :xml-question-mark stack))))
	  ((double-quotep (car input))
	   (multiple-value-bind
		 (input stack)
	       (tokenize-tag-value (cdr input) stack)
	       (tokenize-xml input stack)))
	  ((equalsp (car input))
	   (tokenize-xml (cdr input) (cons :xml-equals stack)))
	  ((forward-slashp (car input))
	   (if (eq :xml-bracket-open (car stack))
	       (tokenize-xml (cdr input) (cons :xml-closing-tag-open (cdr stack)))
	       (tokenize-xml (cdr input) (cons :xml-forward-slash stack))))
	  ((closing-bracketp (car input))
	   (cond
	     ((eq :xml-question-mark (car stack))
	      (tokenize-xml (cdr input) (cons :xml-metadata-end (cdr stack))))
	     ((eq :xml-forward-slash (car stack))
	      (tokenize-xml (cdr input) (cons :xml-self-close-tag (cdr stack))))
	     (t (tokenize-xml (cdr input) (cons :xml-bracket-close stack)))))
	  ((member (car stack) '(:xml-metadata-end :xml-bracket-close :xml-self-close-tag))
	   (multiple-value-bind
		 (input stack)
	       (tokenize-tag-data input stack)
	     (tokenize-xml input stack)))
	  ((letterp (car input))
	   (multiple-value-bind
		   (input stack)
	       (tokenize-symbol input stack)
	     (tokenize-xml input stack)))
	  (t (error (format nil "Unknown character in XML data: ~a" (car input)))))))

  (defun parse-xml (input &optional (stack (list 'empty-stack)))
    (if (null input)
	(if (eq 2 (length stack))
	    (car stack)
	    (error (format nil "Tag not closed in ~a" stack)))
	(case (car input)
	  ((:xml-bracket-open)
	   (parse-xml-tag (cdr input) (cons (car input) stack)))
	  ((:xml-metadata-begin)
	   (skip-xml-metadata (cdr input) stack))
	  ((:xml-closing-tag-open)
	   (parse-xml-closing-tag (cdr input) stack))
	  (t (error (format nil "Wrong token: ~a" (car input)))))))

  (defun parse-xml-tag (input stack)
    (if (null input)
	(error (format nil "EOF in tag after: ~a" (car stack)))
	(if (eq :xml-bracket-open (car stack))
	    (parse-xml-tag (cdr input) (cons (car input) stack))
	    (case (car input)
	      ((:xml-bracket-close)
	       (parse-xml (cdr input) stack))
	      ((:xml-self-close-tag)
	       (parse-xml (cdr input) (produce-xml-tag stack)))
	      ((:xml-metadata-begin)
	       (skip-xml-metadata (cdr input) stack))
	      (t (if (symbolp (car input))
		     (parse-xml-value-pair (cdr input) (cons (car input) stack))
		     (error (format nil "Unexpected token: ~a" (car input)))))))))

  (defun parse-xml-value-pair (input stack)
    (if (null input)
	(error "Error in value pair.")
	(if (and (symbolp (car stack))
		 (eq :xml-equals (car input)))
	    (parse-xml-value-pair (cdr input) (cons (car input) stack))
	    (if (and (eq :xml-equals (car stack))
		     (stringp (car input)))
		(parse-xml-tag (cdr input) (cons (car input) (cdr stack)))
		(error (format nil "Expected :XML-EQUALS instead of ~a" (car input)))))))

  (defun skip-xml-metadata (input stack)
    (if (null input)
	(error "EOF in xml metadata tag")
	(if (eq (car input) :xml-metadata-end)
	    (parse-xml (cdr input) stack))))

  (defun produce-xml-tag (input &optional (stack (list 'empty-stack)))
    (if (eq :xml-bracket-open (car input))
	(cons (butlast stack) (cdr input))
	(produce-xml-tag (cdr input) (cons (car input) stack))))

  (defun parse-xml-closing-tag (input stack)
    (if (symbolp (car input))
	(parse-xml-tag (cdr input) (close-xml-tag stack (car input)))
	(error (format nil "Expected symbol, not ~a" (car input)))))
  
  (defun close-xml-tag (input tag &optional (stack (list 'empty-stack)))
    (if (null input)
	(error (format nil "Could not close tag: ~a" tag))
	(if (eq tag (car input))
	    (cons (butlast (cons (car input) stack)) (cddr input))
	    (close-xml-tag (cdr input) tag (push (car input) stack))))))
