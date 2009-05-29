;;; This file is part of Skij.
;;; Author: Michael Travers (mt@watson.ibm.com)

;;; Licensed Materials - See the file license.txt.
;;; (c) Copyright IBM Corp. 1997, 1998. All rights reserved.

;;; bootstrap defmacro
(define defmacro
  (new 'com.ibm.jikes.skij.Macro
       (current-environment)
       'defmacro
       '(ignore form . body)
       '(begin
	  (define name (car form))
	  (define args (cdr form))
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
