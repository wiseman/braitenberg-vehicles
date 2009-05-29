;;; This file is part of Skij.
;;; Author: Michael Travers (mt@watson.ibm.com)

;;; Licensed Materials - See the file license.txt.
;;; (c) Copyright IBM Corp. 1997, 1998. All rights reserved.

;;; bootstrap defmacro
(define defmacro
  (new 'com.ibm.jikes.skij.Macro
       (current-environment)
       'defmacro
       '(ignore name args . body)
       '(begin
	  (if (symbol? name)	;handle (defmacro (name args*) . body)
	      '()
	      (begin (set! body (cons args body))
		     (set! args (cdr name))
		     (set! name (car name))))
	  `(define ,name
	     (new 'com.ibm.jikes.skij.Macro 
		  (current-environment)
		  ',name
		  ',(cons 'ignore args)
		  '(begin ,@body))))))

(define (macroexpand form)
  (if (instanceof (eval (car form)) 'com.ibm.jikes.skij.Macro)
      (apply (eval (car form)) form)
      form))

; akin to lambda, used by SILK
(define macro
  (new 'com.ibm.jikes.skij.Macro
       (current-environment)
       'macro
       '(ignore args . body)
       ''`(new 'com.ibm.jikes.skij.Macro 
	       (current-environment)
	       '<anon-macro>
	       ',(cons 'ignore args)
	       '(begin ,@body))))

(define (gensym . prefix)
  (define gensym-counter 0)
  (if (null? prefix)
    (set! prefix "gensym")
    (set! prefix (car prefix)))
  (set! gensym-counter (+ gensym-counter 1))
  (string->symbol (string-append "%%" prefix "-" (to-string gensym-counter))))