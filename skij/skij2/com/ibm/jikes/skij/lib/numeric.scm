;;; This file is part of Skij.
;;; Author: Michael Travers (mt@watson.ibm.com)

;;; Licensed Materials - See the file license.txt.
;;; (c) Copyright IBM Corp. 1997, 1998. All rights reserved.

;;; We only have Scheme reals and integers, expressed as Java Double and Integer classes

(define (number? x)
  (instanceof x 'java.lang.Number))

(define complex? number?)
(define real? number?)
(define rational? number?)

(define (integer? x)
  (instanceof x 'java.lang.Integer))

(define real? number?)

(define exact? integer?)

(define (inexact? x)
  (instanceof x 'java.lang.Double))

(define (exact->inexact x)
  (double x))

(define (inexact->exact x)
  (if (= (round x) x) 
      (invoke-static 'java.lang.Math 'round x)
      (error `(,x cant be made exact))))

(define (zero? x)
  (= 0 x))

(define (negative? x)
  (< x 0))

(define (positive? x)
  (> x 0))

(define (number->string n . radix)
  (if (null? radix)
      (to-string n)
      (if (instanceof n 'java.lang.Integer)
	  (invoke-static 'java.lang.Integer 'toString n (car radix))
	  (error "can't use radix for non-integers in number->string"))))

(define (string->number string . radix)
  (define try-int
    (catch
     (invoke-static 'java.lang.Integer 'valueOf string (if (null? radix) 10 (car radix)))))
  (if (instanceof try-int 'java.lang.Integer)
      try-int
      (if (null? radix)
	  (let ((try-double (catch (invoke-static 'java.lang.Double 'valueOf string))))
	    (if (instanceof try-double 'java.lang.Number)
		try-double
		#f))
	  (error "Can't parse floats in non-decimal radices"))))

(define (modulo x y)
  (define rem (remainder x y))
  (if (or (and (< rem 0)
	       (> y 0))
	  (and (< y 0)
	       (> rem 0)))
      (+ rem y)
      rem))

(define (even? x)
  (= 0 (modulo x 2)))

(define (odd? x)
  (= 1 (modulo x 2)))

(define (abs x)
  (if (> x 0)
      x
      (* -1 x)))

; type converters. integer is defined as a primitive, for no very good reason.
; these might want to be, for efficiency (also they could check type and return arg if it's already as required)
(define (long x)
  (invoke x 'longValue))

(define (byte x)
  (invoke x 'byteValue))

(define (short x)
  (invoke x 'shortValue))

(define (float x)
  (invoke x 'floatValue))

(define (double x)
  (invoke x 'doubleValue))

(defmacro (def-math name)
  `(define (,name x)
     (invoke-static 'java.lang.Math ',name (double x))))

;;; some of these have been made primitives
(def-math sqrt)
(def-math exp)
(def-math log)
(def-math floor)
(def-math atan)
(def-math acos)
(def-math atan)				;+++ 2-arg form
(def-math ceil)
(define ceiling ceil)

(define (expt a b)
  (if (zero? a)
      (if (zero? b)
	  1
	  0)
      (exp (* b (log a)))))

;;; correct for integers, more or less...
;;; but probably division is wrong.
(define (quotient a b)
  (/ (integer a) (integer b)))

(define (modulo a b)
  (let ((rem (remainder a b)))
    (if (eq? (negative? b) (negative? rem))
	rem
	(+ rem b))))

(begin
  (define (gcd2 a b)
    (if (zero? b) a
	(gcd b (remainder a b))))

  (define (lcm2 a b)
    (/ (* a b)
       (gcd a b)))

  (define (make-nary 2func identity)
    (letrec ((nfunc (lambda args
		      (cond ((null? args) identity)
			    ((null? (cdr args))
			     (abs (car args)))
			    ((null? (cddr args))
			     (2func (car args) (cadr args)))
			    (#t (2func (car args)
				       (apply nfunc (cdr args))))))))
      nfunc))
  )

(define gcd (make-nary gcd2 0))
(define lcm (make-nary lcm2 1))  
