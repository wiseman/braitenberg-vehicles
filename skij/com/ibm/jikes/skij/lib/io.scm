;;; This file is part of Skij.
;;; Author: Michael Travers (mt@watson.ibm.com)

;;; Licensed Materials - See the file license.txt.
;;; (c) Copyright IBM Corp. 1997, 1998. All rights reserved.

(define eol-character (integer->char 10))
(define cr-character (integer->char 13))

(define newline
  (lambda args
    (if (null? args)
	(write-char eol-character)
	(write-char eol-character (car args)))))

; local extension
(define print
  (lambda args
    (require-write)
    (apply write args)
    (apply newline (cdr args))
    (car args)))

(define (close-input-port inport)
  (invoke (peek inport 'reader) 'close))

(define (close-output-port outport)
  (invoke (peek outport 'writer) 'close))

(define (eof-object? obj)
  (instanceof obj 'com.ibm.jikes.skij.EOFObject))

(define (copy-until-eof instream outstream)
  (define char (read-char instream))
  (if (not (eof-object? char))
      (begin
	(write-char char outstream)
	(copy-until-eof instream outstream))))

; I have no idea whether this will work across implementations
(define (read-line in) 
  (if (class-supports-readLine? (invoke (peek in 'reader) 'getClass))
      (invoke (peek in 'reader) 'readLine)      
      (with-string-output-port 
       (lambda (outport)
	 (let loop ((char (read-char in)))
	   (cond ((equal? char eol-character))
		 ((equal? char cr-character)
		  (define lf? (read-char in))
		  (if (equal? lf? eol-character)
		      #f
		      (error `(Non-linefeed ,lf? seen after CR)))) ;not sure what's right
		 ((equal? char -1))			;whups, we want to do a return-from here, can't do it...
		 (#t
		  (write-char char outport)
		  (loop (read-char in)))))))))

(define (input-port? thing)
  (instanceof thing 'com.ibm.jikes.skij.InputPort))

(define (output-port? thing)
  (instanceof thing 'com.ibm.jikes.skij.OutputPort))

;;; new read-line that takes advantage of existing java methods
;;; semi-ludicrous
(define-memoized (class-supports-readLine? class)
  (instanceof
   (catch
    (invoke class 'getMethod "readLine" (%make-vector 0 (class-named 'java.lang.Class))))
   'java.lang.reflect.Method))


