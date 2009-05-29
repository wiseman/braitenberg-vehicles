;;; This file is part of Skij.
;;; Author: Michael Travers (mt@watson.ibm.com)

;;; Licensed Materials - See the file license.txt.
;;; (c) Copyright IBM Corp. 1997, 1998. All rights reserved.

;;; everything here is a local extension

(define (make-hashtable)
  (new 'java.util.Hashtable))

(define (identity thing) thing)

(define (hashtable-get ht name default)
  (%or-null (invoke ht 'get name)
	    default))

(define (hashtable-put ht name value)
  (invoke ht 'put name value))

(define (hashtable-remove ht name)
  (invoke ht 'remove name))

; OK, here is an instance (and a good one) where you actually want a closure passed in...
; this does what you almost always want
;;; Synchronize on NAME rather than ht. This prevents two computations
;;; for the same name, but will allow already-computed entries to be returned quickly.
;;; one problem: synchronized uses eq? equality, hashtable uses equal?
(define (hashtable-lookup ht name generator)
  (synchronized				
   name
   (%or-null (invoke ht 'get name)
	      (begin
		(define new (generator name))
		(invoke ht 'put name new)
		new))))

(define (clear-hashtable ht)
  (invoke ht 'clear))

(define (map-hashtable func ht)
  (map-enumeration (lambda (key)
		     (func key (hashtable-get ht key #f)))
		   (invoke ht 'keys)))

(define (for-hashtable func ht)
  (for-enumeration (lambda (key)
		     (func key (hashtable-get ht key #f)))
		   (invoke ht 'keys)))

; get contents as list
(define (hashtable-contents ht)
  (map-hashtable (lambda (key value) (cons key value)) ht))
