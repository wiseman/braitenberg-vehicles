;;; This file is part of Skij.
;;; Author: Michael Travers (mt@watson.ibm.com)

;;; Licensed Materials - See the file license.txt.
;;; (c) Copyright IBM Corp. 1997, 1998. All rights reserved.

(define *trace-level* 0)

(define (traceprint msg)
  (newline)
  (let loop ((i (dynamic *trace-level*)))
    (unless (zero? i)
      (display "  ")
      (loop (- i 1))))
  (write msg))

(defmacro (with-traceprint msg . body)
  `(begin (traceprint ,msg)
	  (let ((*trace-level* (+ (dynamic *trace-level*) 1)))
	    ,@body)))

(define (proc-name proc)
  (%or-null (peek proc 'name)
	    proc))

(define (trace-encapsulate procedure)
  (lambda args
    (traceprint`(Entering ,(proc-name procedure) with args ,@args))
    (define result #f)
    (let ((*trace-level* (+ (dynamic *trace-level*) 1)))
      (set! result (apply procedure args)))
    (traceprint `(Exit ,(proc-name procedure) with ,result))
    result))
  
(defmacro (ttrace procname)
  `(set! ,procname
	 (trace-encapsulate ,procname)))