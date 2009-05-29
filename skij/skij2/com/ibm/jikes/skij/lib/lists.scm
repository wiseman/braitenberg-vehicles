;;; This file is part of Skij.
;;; Author: Michael Travers (mt@watson.ibm.com)

;;; Licensed Materials - See the file license.txt.
;;; (c) Copyright IBM Corp. 1997, 1998. All rights reserved.

(define (caar x) (car (car x)))
;(define (cadr x) (car (cdr x)))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))

(define (caaar x) (car (car (car x))))
(define (caadr x) (car (car (cdr x))))
(define (cadar x) (car (cdr (car x))))
(define (caddr x) (car (cdr (cdr x))))

(define (cdaar x) (cdr (car (car x))))
(define (cdadr x) (cdr (car (cdr x))))
(define (cddar x) (cdr (cdr (car x))))
(define (cdddr x) (cdr (cdr (cdr x))))

(define (cadddr x) (car (cdr (cdr (cdr x)))))

; etc

(define (list? l)
  (if (null? l) #t
      (if (pair? l)
	  (list? (cdr l))
	  #f)))

(define (length l)
  (if (null? l) 0
      (+ 1 (length (cdr l)))))

; member defined in init.scm

(define (memq thing lst)
  (if (eq? lst '()) #f
      (if (eq? thing (car lst))
	  lst
	  (memq thing (cdr lst)))))

(define (memv thing list)
  (if (null? list) #f
      (if (eqv? thing (car list))
	  list
	  (memv thing (cdr list)))))

;;; i'm lazy
(define (assq obj alist)
  (ass obj alist eq?))

(define (assv obj alist)
  (ass obj alist eqv?))

(define (assoc obj alist)
  (ass obj alist equal?))

(define (ass obj alist compare)
  (if (null? alist) #f
      (if (compare obj (caar alist))
	  (car alist)
	  (ass obj (cdr alist) compare))))  

; essential procedure
(define (reverse lst)
  (define (reverse1 lst rev)
    (if (null? lst)
	rev
        (reverse1 (cdr lst) (cons (car lst) rev))))
  (reverse1 lst '()))


; extension
; nondestructive mergesort
(define (sort lst comp)
  (if (null? lst) lst
      (if (null? (cdr lst)) lst
	  (begin
	    (define pivot (car lst))	;bad way to pick pivot
	    (define less '())
	    (define more '())
	    (for-each 
		     (lambda (elt)
		       (if (comp pivot elt)
			   (set! more (cons elt more))
			   (set! less (cons elt less))))
		     (cdr lst))
	    (append (sort less comp) (cons pivot (sort more comp)))))))

; essential procedure
(define append 
  (lambda lists
    (if (null? lists)
	'()
	(if (null? (cdr lists))
	    (car lists)
	    (append1 (car lists)
		     (apply append (cdr lists)))))))

; was internal proc, pulled out because interpreter is not smart
(define (append1 l1 l2)
  (if (null? l1) l2
      (cons (car l1)
	    (append1 (cdr l1) l2))))

(define (list-tail lst k)
  (if (= k 0) lst
      (list-tail (cdr lst) (- k 1))))

(define (list-ref lst k)
  (car (list-tail lst k)))

; local extension
(define (position elt lst test)
  (define rest lst)
  (define i 0)
  (loop
   (if (null? rest)
       (break #f))
   (if (test (car rest) elt)
       (break i))
   (set! rest (cdr rest))
   (set! i (+ i 1))))

; local extension
(define (last-cdr lst)
  (if (null? (cdr lst))
      lst
      (last-cdr (cdr lst))))

; local extension
(define (filter-out pred list)
  (if (null? list) list
      (if (pred (car list))
	  (filter-out pred (cdr list))
	  (cons (car list)
		(filter-out pred (cdr list))))))

(define (filter pred list)
  (if (null? list) list
      (if (pred (car list))
	  (cons (car list)
		(filter pred (cdr list)))
	  (filter pred (cdr list)))))

; extension
(define (butlast list)
  (if (= (length list) 1)
      '()
      (cons (car list)
	    (butlast (cdr list)))))

; extension
(define (last list)
  (if (= (length list) 1)
      (car list)
      (last (cdr list))))

; extension, non-destructive
(define (delete object list)
  (cond ((null? list) list)
	((eq? (car list) object)
	 (delete object (cdr list)))
	(#t (cons (car list) 
		  (delete object (cdr list))))))

; extension, destructive
(define (ndelete object list)
  (if (eq? object (car list))
      (ndelete object (cdr list))
      (let loopx ((rest list))
	(cond ((null? rest) #f)
	      ((and (pair? (cdr rest))
		    (eq? (car (cdr rest)) object))
	       (set-cdr! rest (cdr (cdr rest)))
	       (loopx (cdr rest)))
	      (#t (loopx (cdr rest))))
	list)))

(define (remove-duplicates list)
  (cond ((null? list) '())
	((memq (car list) (cdr list))
	 (remove-duplicates (cdr list)))
	(#t (cons (car list)
		  (remove-duplicates (cdr list))))))

; internal
(begin
  (define (nconc1 l1 l2)
    (set-cdr! (last-cdr l1) l2)
    l1))

; local extension
(define nconc 
  (lambda args
    (if (null? args) '()
	(if (null? (car args))
	    (apply nconc (cdr args))
	    (if (null? (cdr args)) (car args)
		(nconc1 (car args) (apply nconc (cdr args))))))))