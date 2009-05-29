;;; This file is part of Skij.
;;; Author: Michael Travers (mt@watson.ibm.com)

;;; Licensed Materials - See the file license.txt.
;;; (c) Copyright IBM Corp. 1997, 1998. All rights reserved.

;;; Contains the non-interface dependent parts of inspector

(define (describe x)
  (for-each (lambda (item)
	      (display (car item))	;in some cases this should be write (ie, hashtables)
	      (display ": ")
	      (write (cadr item))
	      (newline))
	    (inspect-data x)))


; returns list, first element is header, rest is data. Each element is a 2-list
(define (inspect-data object)
  (cond ((vector? object)
	 (inspect-data-vector object))
	((list? object)
	 (inspect-data-list object))
	((instanceof object 'java.util.Enumeration)
	 (inspect-data-list (map-enumeration (lambda (x) x) object)))
	((instanceof object 'java.util.Vector)
	 (inspect-data-list (map-enumeration (lambda (x) x) (invoke object 'elements))))
	((instanceof object 'java.util.Hashtable)
	 (inspect-data-ht object))
	(#t
	 (inspect-data-obj object))))

(begin					; hide support functions from autoload
(define (xor b1 b2)
  (or (and b1 (not b2))
      (and b2 (not b1))))

(define (inspect-data-obj object)
  (define class (invoke object 'getClass))
  (define data (inspect-data-obj1 object class #f))
  (when (eq? class (class-named 'java.lang.Class))
	(define static-data (inspect-data-obj1 object object #t))
	(for-each (lambda (entry)
		    (set-car! entry
			      (string-append "[static] " (car entry))))
		  static-data)
	(set! data (nconc static-data data)))
  (cons '(Member Value) data))

(define (inspect-data-obj1 object class static?)
  (define data '())
  (define (make-entry name value)
    (set! data (cons (list name value) data)))
  (for-each (lambda (ivar)
		(if (not (xor static? (member-static? ivar)))
		    (make-entry (invoke ivar 'getName)
				(invoke ivar 'get object))))
	      ;(invoke class 'getFields)
	    (all-fields class)
	    )
  (catch
   (for-vector (lambda (method)
		 (if (not (xor static? (member-static? method)))
		     (begin
		       (define name (invoke method 'getName))
		       (if (and (>= (string-length name) 3)
				(or (equal? (substring name 0 3) "get")
				    (equal? (substring name 0 2) "is"))
				(zero? (vector-length (invoke method 'getParameterTypes))))
			   (catch
			    (make-entry
			     (string-append name "( )")
			     (invoke method 'invoke object no-args)
			     ))))))
	       (invoke class 'getMethods)))
  (set! data (sort data (lambda (e1 e2)
			  (string<? (car e1) (car e2)))))
  data)

;;; new improved...
(define-memoized (all-fields class) 
  (if java2?
      (let ((basefields (vector->list (invoke class 'getDeclaredFields)))
	    (superclass  (invoke class 'getSuperclass)))
	(for-each (lambda (f) (invoke f 'setAccessible #t)) basefields)
	(if (%%null? superclass)
	    basefields
	    (merge-fields basefields
			  (all-fields superclass))))
      (vector->list (invoke class 'getFields))))

(define (merge-fields class-fields super-fields)
  (define class-field-names (map (lambda (f) (invoke f 'getName)) class-fields))
  (for-each (lambda (f) 
	      (if (member (invoke f 'getName) class-field-names)
		  '()
		  (push f class-fields)))
	    super-fields)
  class-fields)

(define (member-static? member)
  (invoke-static 'java.lang.reflect.Modifier 'isStatic (invoke member 'getModifiers)))

(define no-args (list->vector '()))

(define (inspect-data-list lst)
  (define i -1)
  (cons '(Index Element)
	(map (lambda (elt)
	       (set! i (+ i 1))
	       (list i elt))
	     lst)))

(define (inspect-data-vector vector)
  (define i -1)
  (cons '(Index Element)
	(map-vector (lambda (elt)
		      (set! i (+ i 1))
		      (list i elt))
		    vector)))


(define (inspect-data-ht ht)
  (cons '(Key Value)
	(map-hashtable (lambda (key val)
			 (list key val))
		       ht)))
)  
