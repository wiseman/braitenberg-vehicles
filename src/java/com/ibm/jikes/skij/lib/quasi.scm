;;; This file is part of Skij.
;;; Author: Michael Travers (mt@watson.ibm.com)

;;; Licensed Materials - See the file license.txt.
;;; (c) Copyright IBM Corp. 1997, 1998. All rights reserved.

;;; Correct, but VERY slow.
;;; Macro expansions are memoized now, but using quasi in code still loses


(define (eval-quasi exp env)
  ;  (print (string-append '"eval-quasi on " (invoke exp 'toString)))
  (car (eval-quasi1 exp env 0)))

(begin					;hide from autoload
  ; returns value with extra list wrapping (so unquote-splicing can work)
  ; COND depends on quasi so we can't use it here.
  (define (eval-quasi1 exp env level)
    (if (pair? exp)
	(if (null? exp) (list '())
	    (if (eq? 'quasiquote (car exp))
		;; nested quasiquote
		(list (list 'quasiquote (car (eval-quasi1 (cadr exp) env (+ level 1)))))
		(if (eq? 'unquote (car exp))
		    (if (= level 0)
			(list (eval (cadr exp) env)) ;expanded unquote
			(list (list 'unquote (car (eval-quasi1 (cadr exp) env (- level 1)))))) ;deferred unquote
		    (if (eq? 'unquote-splicing (car exp))
			(if (= level 0)
			    (copy-list (eval (cadr exp) env))	;expanded unquote-splicing, copy is necessary to prevent possibility of argument getting munged by nconc
			    (list (list 'unquote-splicing (car (eval-quasi1 (cadr exp) env (- level 1)))))) ; deferred unquote-splicing
			;; everything else
			(list (nconc (eval-quasi1 (car exp) env level)
				     (car (eval-quasi1 (cdr exp) env level))))
			))))
	(if (vector? exp)
	    (list (list->vector
		   (car (eval-quasi1 (vector->list exp) env level))))
	    ; not a pair or vector
	    (list exp))))

(define (copy-list lis)
  (if (pair? lis)
      (cons (car lis) (copy-list (cdr lis)))
      lis))

  ) ;end begin



