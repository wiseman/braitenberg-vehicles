;;; This file is part of Skij.
;;; Author: Michael Travers (mt@watson.ibm.com)

;;; Licensed Materials - See the file license.txt.
;;; (c) Copyright IBM Corp. 1997, 1998. All rights reserved.

(defmacro (delay exp)
  `(make-promise (lambda () ,exp)))

(define (make-promise proc)
  (let ((forced? #f)
	(value #f))
    (lambda ()
      (if forced?
	  value
	  (begin
	    (set! value (proc))
	    (set! forced? #t)
	    value)))))

(define (force promise)
  (promise))