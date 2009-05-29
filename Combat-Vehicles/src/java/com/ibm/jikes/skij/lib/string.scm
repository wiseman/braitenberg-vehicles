;;; This file is part of Skij.
;;; Author: Michael Travers (mt@watson.ibm.com)

;;; Licensed Materials - See the file license.txt.
;;; (c) Copyright IBM Corp. 1997, 1998. All rights reserved.

;string? moved to write

; I was using this as an equivalent to Java toString, but it happens to be 
; a Scheme primitive with a different meaning...so support both through a crock.
(define (string . args)
  (if (null? (cdr args))
      (to-string (car args))
      (let ((chars (%make-vector (length args) '#,(peek-static 'java.lang.Character 'TYPE))))
	(%fill-vector chars args)
	(new 'java.lang.String chars))))

(define (make-string k . rest)
  (define chars (%make-vector k (peek-static 'java.lang.Character 'TYPE)))
  (when (not (null? rest))
	(define char (car rest))
	(let loopx ((i 0))
	  (when (< i k)
		(vector-set! chars i char)
		(loopx (+ i 1)))))
  (new 'java.lang.String
       chars))

(define (string-ref string k)
  (invoke string 'charAt k))

(define (string-set! string k char)
  (error '"Sorry, string-set! is not supported."))

; essential procedure
(define (substring string start end)
  (invoke string 'substring start end))

; string-append is in init.scm

;skij extension
(define (string-trim string)
  (invoke string 'trim))

;skij extension 
(define (string-search string char from)
  (define result
    (if from 
	(invoke string 'indexOf (integer char) from)
	(invoke string 'indexOf (integer char))))
  (if (= result -1) #f result))

;skij extension
(define (string-replace string char-in char-out)
  (invoke string 'replace char-in char-out))

;skij extension
; +++ needs way to deal with multiple occurances
(define (string-replace-string string old new)
  (let ((index (invoke string 'indexOf old)))
    (if (negative? index)
	string
	(string-append (substring string 0 index)
		       new
		       (substring string (+ index (string-length old)) (string-length string))))))
	

; skij extension (MIT Scheme compatible)
(define (with-string-output-port func)
  (define writer (new 'java.io.StringWriter))
  (define out (new 'com.ibm.jikes.skij.OutputPort writer))
  (func out)
  (to-string  writer))

(define (display-to-string thing)
  (with-string-output-port (lambda (port) (display thing port))))

(define (write-to-string thing)
  (with-string-output-port (lambda (port) (write thing port))))

; skij extension (MIT Scheme compatible)
(define (with-input-from-string string func)
  (define reader (new 'java.io.StringReader string))
  (define in (new 'com.ibm.jikes.skij.InputPort reader))
  (func in))

(define (read-from-string string)
  (with-input-from-string string
    (lambda (in)
      (read in))))

(define (string-length string)
  (invoke string 'length))

(define string=? equal?)

;;; +++ note: stupidly inefficient since the int result has to be boxed. Maybe make
;;; this and brethren primitives. CI versions are even worse -- makes new strings.
(defmacro (def-string-compare comp)
  `(begin
     (define (,(symbol-conc 'string comp '?) s1 s2)
       (,comp (invoke s1 'compareTo s2) 0))
     (define (,(symbol-conc 'string-ci comp '?) s1 s2) 
       (,comp (invoke (invoke s1 'toLowerCase) 'compareTo (invoke s2 'toLowerCase)) 0))))

(def-string-compare <)
(def-string-compare >)
(def-string-compare >=)
(def-string-compare <=)

(define (string-ci=? s1 s2)
  (invoke s1 'equalsIgnoreCase s2))

(define (string-ref string i)
  (invoke string 'charAt i))

; local extension
; recursive version
(define (parse-substrings string separator)
  (let loopx ((results '())
	      (index0 0)
	      (index1 (invoke string 'indexOf separator)))
    (if (negative? index1)
	(reverse (cons (substring string index0 (string-length string))
		       results))
	(loopx (cons (substring string index0 index1)
		     results)
	       (+ index1 1)
	       (invoke string 'indexOf separator (+ index1 1))
	       ))))


(define (string->list string)
  (define len (string-length string))
  (let loop ((i 0))
    (if (= i len) '()
	(cons (string-ref string i)
	      (loop (+ i 1))))))

(define (list->string list)
  (apply string list))

	