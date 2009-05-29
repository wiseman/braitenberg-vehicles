;;; This file is part of Skij.
;;; Author: Michael Travers (mt@watson.ibm.com)

;;; Licensed Materials - See the file license.txt.
;;; (c) Copyright IBM Corp. 1997, 1998. All rights reserved.

;;; misc java interfaces
;;; wrap this around things that might not return a value, to turn nulls to 'null
; obso, or so I hope
  
; Like OR, but returns first non-null clause
(defmacro (%or-null . clauses)
  (if (null? clauses) '(%null)
      `(let ((temp ,(car clauses)))
	 (if (%%null? temp)
	     (%or-null ,@(cdr clauses))
	     temp))))

(defmacro (catch . body)
  `(%catch (lambda () ,@body)))

(define (error msg)
  (throw msg))

;instanceof moved to init

; moved to primitives
;(define (class-of x)
;  (invoke x 'getClass))

;;; These procedures patch themselves with primitives. 
(define (map-enumeration func enum)
  (class-named 'com.ibm.jikes.skij.misc.Hashpatch)
  (map-enumeration func enum))

(define (for-enumeration func enum)
  (class-named 'com.ibm.jikes.skij.misc.Hashpatch)
  (for-enumeration func enum))

(define (enumeration->list enum)
  (let ((lst '()))
    (for-enumeration (lambda (x) (push x lst)) enum)
    (reverse lst)))

(define (exit)
  (invoke-static 'java.lang.System 'exit 0))

(define (start-application class . args)
  (if (not (instanceof class 'java.lang.Class))
      (set! class (class-named class)))
  (let ((arg-vector (%make-vector (length args) (class-named 'java.lang.String))))
    (%fill-vector arg-vector args)
    (invoke-static class 'main arg-vector)))

(defmacro (ignore-errors . body)
  `(catch ,@body))

(defmacro (ignore-errors-and-warn . body)
  `(let ((result (catch ,@body #f)))
     (if (instanceof result 'java.lang.Throwable)
	 (begin
	   (display "\nIgnoring exception: ")
	   (display (invoke result 'getMessage))))
     result))

; analagous to invoke but just returns method object
(define (get-method obj mname . args)
  (invoke-static 'com.ibm.jikes.skij.util.Invoke 'getMethod
		 obj
		 (string mname)
		 (invoke-static 'com.ibm.jikes.skij.PrimProcedure 'makeArray args)))

(define (coerce-class thing)
  (cond ((instanceof thing 'java.lang.Class) thing)
	((symbol? thing) (class-named thing))
	(#t (class-of thing))))

; better name: method-apropos?
; +++ would be nice if case was ignored?
(define (method-apropos class mname)
  (set! mname (to-string mname))
  (filter (lambda (method)
;	    (equal? mname (invoke method 'getName)))
	    (>= (invoke (invoke method 'getName) 'indexOf mname) 0))
	  (vector->list (invoke (coerce-class class) 'getMethods))))

; works for constructors too.
(define (document-method method)
  (require-resource 'scm/browser.scm)
  (browse-url (method-doc-url method)))

;;; See backtrace, which is in init.scm

(define (backtrace-inspect . exception)
  (set! exception (if (null? exception)
		      (peek-static 'com.ibm.jikes.skij.SchemeException 'lastForUser)
		      (car exception)))
  (inspect (reverse (peek exception 'backtrace))))

(define (java-backtrace . exception)
  (set! exception (if (null? exception)
		      (peek-static 'com.ibm.jikes.skij.SchemeException 'lastForUser)
		      (car exception)))
  (invoke (peek exception 'encapsulated) 'printStackTrace))

