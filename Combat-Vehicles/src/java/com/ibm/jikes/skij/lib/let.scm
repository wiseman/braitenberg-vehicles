;;; This file is part of Skij.
;;; Author: Michael Travers (mt@watson.ibm.com)

;;; Licensed Materials - See the file license.txt.
;;; (c) Copyright IBM Corp. 1997, 1998. All rights reserved.

(defmacro (let clauses . body)
  (if (symbol? clauses)
      (named-let clauses (car body) (cdr body))
      `((lambda ,(map car clauses)
	  ,@body)
	,@(map cadr clauses))))

(define (named-let name clauses body)
  `(letrec ((,name (lambda ,(map car clauses)
		     ,@body)))
     (,name ,@(map cadr clauses))))

;;; not quite right since later values can refer to earlier variables
(defmacro (letrec clauses . body)
  `((lambda ,(map car clauses)
      ,@(map (lambda (clause)
	       `(set! ,(car clause) ,(cadr clause)))
	     clauses)
      ,@body)
    ,@(map (lambda (c) #f) clauses)))

(defmacro (let* clauses . body)
  `((lambda ()
      ,@(map (lambda (clause)
	       (cons 'define clause))
	     clauses)
      ,@body)))

(defmacro (do clauses end . body)
  `(let %%loop ,(map (lambda (clause) (list (car clause) (cadr clause)))
		     clauses)
     (if ,(car end)
	 (begin ,@(cdr end))
	 (begin
	   ,@body
	   (%%loop ,@(map (lambda (clause)
			    (if (null? (cddr clause))
				(car clause)
				(caddr clause)))
			  clauses))))))


(defmacro (fluid-let clauses . body)
  `(let ((%saved-values #f))
     (dynamic-wind
      (lambda ()
	(set! %saved-values (list ,@(map car clauses)))
	,@(map (lambda (clause)
		 (cons 'set! clause))
	       clauses))
      (lambda () ,@body)
      (lambda ()
	,@(map (lambda (clause)
		 `(set! ,(car clause)
			(pop %saved-values)))
	       clauses)))))
