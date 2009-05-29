;;; This file is part of Skij.
;;; Author: Michael Travers (mt@watson.ibm.com)

;;; Licensed Materials - See the file license.txt.
;;; (c) Copyright IBM Corp. 1997, 1998. All rights reserved.

; write is supposed to generate readable output
; this somewhat duplicates Cons.toString logic, could that get flushed?

(define *print-string-escapes* #f)

(define (write thing . rest)
  (define port (if (null? rest) 
		   (current-output-port)
		   (car rest)))
  (cond ((%%null? thing) 
	 (display "#<null>" port))
	((string? thing)		;quoting odd chars?
	 (display "\"" port)
	 (if *print-string-escapes*	;+++ perhaps this should be dynamic
	     (display-escaped-string thing port)
	     (display thing port))
	 (display "\"" port))
	((pair? thing)
	 (write-pair thing port))
	((null? thing)
	 (display "()" port))
	((or (symbol? thing) (number? thing))
	 (display thing port))
	((vector? thing)
	 (display "#" port)
	 (write (vector->list thing) port))
	((boolean? thing)
	 (if thing 
	     (display "#t" port)
	     (display "#f" port)))
	((char? thing)
	 (display "#\\" port)
	 (display (to-string thing) port))
	(#t
	 (display "#<" port)
	 (display (to-string thing) port)
	 (display ">" port)
	 ))
  thing)

(define eol-character (integer->char 10))

(begin ; hide from autoload-generator

;;; See JLS 3.10.6
(define (display-escaped-string string port)
  (define len (string-length string))
  (let loop ((pos 0))
    (unless (= pos len)
     (define char (string-ref string pos))
     (aif (assv char '((#\" "\\\"")
		       (#\\ "\\\\")
		       (#,eol-character "\\n") ;note the difference
		       (#\' "\\\'")))	;is this necessary? It's 
	  (display (cadr it) port)
	  (write-char char port))
     (loop (+ pos 1)))))
	  
(define (write-pair pair port)
  (if (and (not (null? (cdr pair)))
	   ;;; The #, hair solves a problem in the .java file generator...
	   (memq (car pair) '#,(list 'quote 'quasiquote 'unquote 'unquote-splicing '%%read-eval)))
      (begin
	(display
	 (case (car pair)
	   ((quote) "'")
	   ((quasiquote) "`")
	   ((unquote) ",")
	   ((unquote-splicing) ",@")
	   ((%%read-eval) "#,"))
	 port)
	(write (cadr pair) port))
      (begin
	(display "(" port)
	(write (car pair) port)
	(write-list (cdr pair) port))))

(define (write-list list port)
  (if (null? list)
      (display ")" port)
      (if (not (pair? list))
	  (begin
	    (display " . " port)
	    (write list port)
	    (display ")" port)) 
	  (begin 
	    (display " " port)
	    (write (car list) port)
	    (write-list (cdr list) port)
	    ))))
) ;end begin




