;;; This file is part of Skij.
;;; Author: Michael Travers (mt@watson.ibm.com)

;;; Licensed Materials - See the file license.txt.
;;; (c) Copyright IBM Corp. 1997, 1998. All rights reserved.

; a weak form of nondeterministic computation
; we' like to be able to say 
;    (* (amb 1 2 3) (amb 1 5 7))
; but that requires full call/cc. So instead:
;
; example
;(let-amb x '(1 2 3)
;   (let-amb y '(1 5 7)
;      (print (* x y))
;      (fail)))
;

(define *backtrack-points* '())

; args is just for debug information
(define (fail . args)
  (if (pair? *backtrack-points*)
      ((pop *backtrack-points*) ':failed)
      (error "fail failed: no more choices")))

(define call/cc call-with-current-continuation)

(defmacro (let-amb var values . body)
 `(let loop ((values ,values))
  (if (null? values)
      (fail "no more choices")
      (let* ((,var (car values))
	     (result
	      (call/cc 
	       (lambda (k)
		 (push k *backtrack-points*)
		 ,@body))))
	(if (eq? result ':failed)
	    (loop (cdr values))
	    result)))))
  
(define (integers a b)
  (if (= a b) '()
      (cons a (integers (+ a 1) b))))

(define (square? x)
  (let ((sqt (round (sqrt x))))
    (= (* sqt sqt) x)))

; example
(define (pythagorean n)
  (let ((ints (integers 1 n)))
    (let-amb x ints
       (let-amb y ints
	  (if (square? (+ (* x x) (* y y)))
	      (print `(,x ,y ,(integer (sqrt (+ (* x x) (* y y)))))))
	  (fail)))))