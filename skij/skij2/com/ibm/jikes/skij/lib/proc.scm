;;; This file is part of Skij.
;;; Author: Michael Travers (mt@watson.ibm.com)

;;; Licensed Materials - See the file license.txt.
;;; (c) Copyright IBM Corp. 1997, 1998. All rights reserved.

(define (procedure? thing)
  (instanceof thing 'com.ibm.jikes.skij.Procedure))

;;; from MIT Scheme -- not valid for primitives
(define (procedure-arity-valid? proc k)
  (let ((arity (procedure-arity proc)))
    (and (>= k (car arity))
	 (or (not (cdr arity))
	     (<= k (cdr arity))))))

;;; from MIT Scheme -- not valid for primitives
(define (procedure-arity proc)
  (let loop ((rest (peek proc 'args))
	     (count 0))
    (cond ((null? rest)
	   (cons count count))
	  ((pair? rest)
	   (loop (cdr rest) (+ count 1)))
	  ((symbol? rest)
	   (cons count #f))
	  (#t (error "bad arglist")))))
  
;;; from MIT Scheme
(define (procedure-environment proc)
  (peek proc 'env))

;;; UI function
(define (args proc)
  (peek proc 'args))
