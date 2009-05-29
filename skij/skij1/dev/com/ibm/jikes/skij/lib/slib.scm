;;; this is a (stab at a) SLIB init file for Skij

; Problems: 
; SLIB redefines require
; SLIB redefines nconc (in comlist.scm), breaking backquote expansion.
;  it's commented out, but must be redone for future SLIB releases
; SLIB uses case inconsistently, so some symbols won't match their defs (ie, yasyn.scm)

; define this to point to SLIB
(define (library-vicinity)
;;; This file is part of Skij.
;;; Author: Michael Travers (mt@watson.ibm.com)

;;; Licensed Materials - See the file license.txt.
;;; (c) Copyright IBM Corp. 1997, 1998. All rights reserved.

  "d:/scheme/slib/")

;(define (software-type) 'JAVA)		;it's a platform!
(define (software-type) 'UNIX) 		;no, it's a dessert topping!
(define (scheme-implementation-type) 'Skij)
'(define (scheme-implementation-version) 
  (peek-static 'com.ibm.jikes.skij.Scheme 'version))
(define (scheme-implementation-version) "1.3")

(define *features*
      '(
	source				;can load scheme source files
					;(slib:load-source "filename")
;	compiled			;can load compiled files
					;(slib:load-compiled "filename")
	rev4-report			;conforms to
;	rev3-report			;conforms to
;	ieee-p1178			;conforms to
;	sicp				;runs code from Structure and
					;Interpretation of Computer
					;Programs by Abelson and Sussman.
;	rev4-optional-procedures	;LIST-TAIL, STRING->LIST,
					;LIST->STRING, STRING-COPY,
					;STRING-FILL!, LIST->VECTOR,
					;VECTOR->LIST, and VECTOR-FILL!
;	rev2-procedures			;SUBSTRING-MOVE-LEFT!,
					;SUBSTRING-MOVE-RIGHT!,
					;SUBSTRING-FILL!,
					;STRING-NULL?, APPEND!, 1+,
					;-1+, <?, <=?, =?, >?, >=?
	multiarg/and-			;/ and - can take more than 2 args.
	multiarg-apply			;APPLY can take more than 2 args.
;	rationalize
	delay				;has DELAY and FORCE
	with-file			;has WITH-INPUT-FROM-FILE and
					;WITH-OUTPUT-FROM-FILE
	string-port			;has CALL-WITH-INPUT-STRING and
					;CALL-WITH-OUTPUT-STRING
;	transcript			;TRANSCRIPT-ON and TRANSCRIPT-OFF
;	char-ready?
;	macro				;has R4RS high level macros
	defmacro			;has Common Lisp DEFMACRO
;	eval				;SLIB:EVAL is single argument eval
;	record				;has user defined data structures
;	values				;proposed multiple values
;	dynamic-wind			;proposed dynamic-wind
;	ieee-floating-point		;conforms to
	full-continuation		;can return multiple times
;	object-hash			;has OBJECT-HASH

;	sort
;	queue				;queues
	pretty-print
;	object->string
;	format
	trace				;has macros: TRACE and UNTRACE
;	compiler			;has (COMPILER)
;	ed				;(ED) is editor
;	system				;posix (system <string>)
;	getenv				;posix (getenv <string>)
;	program-arguments		;returns list of strings (argv)
;	Xwindows			;X support
;	curses				;screen management package
;	termcap				;terminal description package
;	terminfo			;sysV terminal description
;	current-time			;returns time in seconds since 1/1/1970
	))

;SLIB uses these names
(define with-input-from-file call-with-input-file)
(define with-output-from-file call-with-output-file)
(define call-with-input-string with-input-from-string)
(define call-with-output-string with-string-output-port)

(define (object-hash obj)
  (invoke obj 'hashCode))

(define system shell)

;current-time

;;; (OUTPUT-PORT-WIDTH <port>)
(define (output-port-width . arg) 79)

;;; (OUTPUT-PORT-HEIGHT <port>)
(define (output-port-height . arg) 24)

;;; (CURRENT-ERROR-PORT)
(define current-error-port
  (let ((port (current-output-port)))
    (lambda () port)))

;;; (TMPNAM) makes a temporary file name.
(define tmpnam (let ((cntr 100))
		 (lambda () (set! cntr (+ 1 cntr))
			 (string-append "slib_" (number->string cntr)))))

;;; (FILE-EXISTS? <string>)
(define (file-exists? f) #f)

;;; (DELETE-FILE <string>)
(define (delete-file f) #f)

;;; FORCE-OUTPUT flushes any pending output on optional arg output port
;;; use this definition if your system doesn't have such a procedure.
(define (force-output . arg) #t)

;;; CALL-WITH-INPUT-STRING and CALL-WITH-OUTPUT-STRING are the string
;;; port versions of CALL-WITH-*PUT-FILE.

;;; CHAR-CODE-LIMIT is one greater than the largest integer which can
;;; be returned by CHAR->INTEGER.
(define char-code-limit 256)

;;; MOST-POSITIVE-FIXNUM is used in modular.scm
(define most-positive-fixnum (peek-static 'java.lang.Integer 'MAX_VALUE))

;;; Return argument
(define (identity x) x)

;;; If your implementation provides eval SLIB:EVAL is single argument
;;; eval using the top-level (user) environment.
(define slib:eval eval)

;;; If your implementation provides R4RS macros:
;(define macro:eval slib:eval)
;(define macro:load load)

; This breaks Skij's facilities
;(define *defmacros*
;  (list (cons 'defmacro
;	      (lambda (name parms . body)
;		`(set! *defmacros* (cons (cons ',name (lambda ,parms ,@body))
;				      *defmacros*))))))
;(define (defmacro? m) (and (assq m *defmacros*) #t))

;(define (macroexpand-1 e)
;  (if (pair? e) (let ((a (car e)))
;		  (cond ((symbol? a) (set! a (assq a *defmacros*))
;				     (if a (apply (cdr a) (cdr e)) e))
;			(else e)))
;      e))

;(define (macroexpand e)
;  (if (pair? e) (let ((a (car e)))
;		  (cond ((symbol? a)
;			 (set! a (assq a *defmacros*))
;			 (if a (macroexpand (apply (cdr a) (cdr e))) e))
;			(else e)))
;      e))

(define gentemp
  (let ((*gensym-counter* -1))
    (lambda ()
      (set! *gensym-counter* (+ *gensym-counter* 1))
      (string->symbol
       (string-append "slib:G" (number->string *gensym-counter*))))))

(define base:eval slib:eval)
(define (defmacro:eval x) (base:eval (defmacro:expand* x)))
(define (defmacro:expand* x)
  (require 'defmacroexpand) (apply defmacro:expand* x '()))

(define (defmacro:load <pathname>)
  (slib:eval-load <pathname> defmacro:eval))

(define (slib:eval-load <pathname> evl)
  (if (not (file-exists? <pathname>))
      (set! <pathname> (string-append <pathname> (scheme-file-suffix))))
  (call-with-input-file <pathname>
    (lambda (port)
      (let ((old-load-pathname *load-pathname*))
	(set! *load-pathname* <pathname>)
	(do ((o (read port) (read port)))
	    ((eof-object? o))
	  (evl o))
	(set! *load-pathname* old-load-pathname)))))

;;; define an error procedure for the library
(define (slib:error . args)
  (error args))

;;; define these as appropriate for your system.
(define slib:tab (integer->char 9))
(define slib:form-feed (integer->char 12))

;;; Support for older versions of Scheme.  Not enough code for its own file.
(define (last-pair l) (if (pair? (cdr l)) (last-pair (cdr l)) l))
(define t #t)
(define nil #f)

;;; Define these if your implementation's syntax can support it and if
;;; they are not already defined.

(define (1+ n) (+ n 1))
(define (-1+ n) (+ n -1))
(define 1- -1+)

(define in-vicinity string-append)

;;; Define SLIB:EXIT to be the implementation procedure to exit or
;;; return if exitting not supported.
(define slib:exit exit)

;;; Here for backward compatability
(define scheme-file-suffix
  (lambda () ".scm"))

;;; (SLIB:LOAD-SOURCE "foo") should load "foo.scm" or with whatever
;;; suffix all the module files in SLIB have.  See feature 'SOURCE.

(define (slib:load-source f) (load (string-append f ".scm")))

;;; (SLIB:LOAD-COMPILED "foo") should load the file that was produced
;;; by compiling "foo.scm" if this implementation can compile files.
;;; See feature 'COMPILED.

;(define slib:load-compiled load)

;;; At this point SLIB:LOAD must be able to load SLIB files.

(define slib:load slib:load-source)

(slib:load (in-vicinity (library-vicinity) "require"))

