;;; This file is part of Skij.
;;; Author: Michael Travers (mt@watson.ibm.com)

;;; Licensed Materials - See the file license.txt.
;;; (c) Copyright IBM Corp. 1997, 1998. All rights reserved.

; vector-ref, -set!, -length are primitives
;vector? is in init.scm

; essential procedure
(define (vector->list vector)
  (define l (vector-length vector))
  (define (a2l n)
    (if (= n l)
	'()
	(cons (vector-ref vector n)
	      (a2l (+ n 1)))))
  (a2l 0))

(define (list->vector list)
  (invoke-static 'com.ibm.jikes.skij.PrimProcedure 'makeArray list))

(define (make-vector n . optional)
  (define newvec (invoke-static 'java.lang.reflect.Array 
			 'newInstance 
			 (class-named 'java.lang.Object)
			 n))
  (if (not (null? optional))
      (let loop ((elt (car optional))
		 (index 0))
	(unless (= index n)
		(vector-set! newvec index elt)
		(loop elt (+ index 1)))))
  newvec)

(define (vector . args)
  (list->vector args))

; extension
(define (map-vector func vector)
  (define len (vector-length vector))
  (define (map1 n)
    (if (= n len)
	'()
	(cons (func (vector-ref vector n))
	      (map1 (+ n 1)))))
  (map1 0))

; extension
(define (for-vector func vector)
  (define len (vector-length vector))
  (define (map1 n)
    (if (= n len)
	(%null)
	(begin (func (vector-ref vector n))
	       (map1 (+ n 1)))))
  (map1 0))

; extension
(define (memq-vector item vector)
  (define len (vector-length vector))
  (define (map1 n)
    (if (= n len)
	#f
	(or (eq? item (vector-ref vector n))
	    (map1 (+ n 1)))))
  (map1 0))

;;; functions for manipulating type-specific arrays. These use java.lang.reflect.Array which handles
;;; the unboxing automatically.

;;; note: primitive class metaobjects may be obtained by doing something like
;;;   (peek-static 'java.lang.Integer 'TYPE)

(define (%make-vector n class)
  (invoke-static 'java.lang.reflect.Array 
	  'newInstance 
	  class
	  n))

(define (%fill-vector vector list)
  (define index 0)
  (for-each (lambda (elt)
	      (vector-set! vector index elt)
	      (set! index (+ index 1)))
	    list))



