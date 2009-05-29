;;; This file is part of Skij.
;;; Author: Michael Travers (mt@watson.ibm.com)

;;; Licensed Materials - See the file license.txt.
;;; (c) Copyright IBM Corp. 1997, 1998. All rights reserved.

;;; multi-dimensional arrays 

(define (%make-array dimensions class)
  (invoke-static 'java.lang.reflect.Array 
	  'newInstance 
	  class
	  (list->int-vector dimensions)))

(define (list->int-vector list)
  (define vec (%make-vector (length list) (peek-static 'java.lang.Integer 'TYPE)))
  (%fill-vector vec list)
  vec)

(define (aref array . idxs)
  (if (null? idxs)
      array
      (apply aref (%vector-ref array (car idxs)) (cdr idxs))))

(define (aset array val . idxs)
  (if (null? (cdr idxs))
      (vector-set! array (car idxs) val)
      (apply aset (vector-ref array (car idxs)) val (cdr idxs))))

