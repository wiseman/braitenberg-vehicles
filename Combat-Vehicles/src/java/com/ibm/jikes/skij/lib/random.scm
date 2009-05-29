;;; This file is part of Skij.
;;; Author: Michael Travers (mt@watson.ibm.com)

;;; Licensed Materials - See the file license.txt.
;;; (c) Copyright IBM Corp. 1997, 1998. All rights reserved.

;;; these are extensions to the Scheme standard

;;; #f --> between 0 and 1.
;;; a number n ---> integer between 0 an n

(define (random arg)
  (if arg
      (integer (random-range arg))
      (invoke-static 'java.lang.Math 'random)))

(define (random-range n)
  (* n (random #f)))

(define (arand center range)
  (- (+ center
	(* 2 (* range (random #f))))
     range))
