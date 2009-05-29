;;; This file is part of Skij.
;;; Author: Michael Travers (mt@watson.ibm.com)

;;; Licensed Materials - See the file license.txt.
;;; (c) Copyright IBM Corp. 1997, 1998. All rights reserved.

;;; anaphoric macros 

(defmacro (aif pred then . else)
  `(let ((it ,pred))
     (if it
	 ,then
	 ,@else)))

(defmacro (aand . args)
  (cond ((null? args) #t)
        ((null? (cdr args)) (car args))
        (#t `(aif ,(car args) (aand ,@(cdr args)) #f))))

(defmacro (awhen pred . body)
  `(aif ,pred (begin ,@body)))

(defmacro (acond . clauses)
  (if (null? clauses) #f
      (if (null? (cdar clauses))
	  `(or ,(caar clauses)		;handle (cond ((foo)) ..) case
	       (acond ,@(cdr clauses)))
	  `(aif ,(caar clauses)
	       (begin ,@(cdar clauses))
	       (acond ,@(cdr clauses))))))

; Keyword argument handler
(defmacro (key args key default)
  `(define ,key (aif (memq ',key ,args)
		     (cadr it)
		     ,default)))

(defmacro (repeat n . body)
  `(let loop ((i ,n))
     (unless (= i 0)
      ,@body
      (loop (- i 1)))))

