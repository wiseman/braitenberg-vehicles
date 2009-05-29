;;; This file is part of Skij.
;;; Author: Michael Travers (mt@watson.ibm.com)

;;; Licensed Materials - See the file license.txt.
;;; (c) Copyright IBM Corp. 1997, 1998. All rights reserved.

;;; random control forms

(defmacro (cond . clauses)
  (if (null? clauses) #f
      (let ((clause (car clauses)))
	(if (eq? (car clause) 'else)
	    (cadr clause)
	    (if (eq? (cadr clause) '=>)
		`(let ((%temp% ,(car clause)))
		   (if %temp%
		       (,(caddr clause) %temp%)
		       (cond ,@(cdr clauses))))
		`(if ,(caar clauses)
		     (begin ,@(cdar clauses))
		     (cond ,@(cdr clauses))))))))

; not done yet
(defmacro (case item . clauses)
  (let ((sym '%%gensym%%))
    `(let ((,sym ,item))
       ,(cons 'cond (map (lambda (clause)
			   (cons (if (eq? (car clause) 'else)
				     #t
				     `(memv ,sym ',(car clause)))
				 (cdr clause)))
			 clauses)))))

; this should probably be a primitive but lets get it right first
; i imagine this is slower than equal? most of the time
(define (eqv? a b)
  (or (eq? a b)
      (and (instanceof a 'java.lang.Number)
	   (instanceof b 'java.lang.Number)
	   (= a b))
      (and (instanceof a 'java.lang.Character)
	   (instanceof b 'java.lang.Character)
	   (invoke a 'equals b))
      ))

(define (not x)
  (if x #f #t))

(defmacro (when pred . body)
  `(if ,pred (begin ,@body)))

(defmacro (unless pred . body)
  `(if (not ,pred) (begin ,@body)))

