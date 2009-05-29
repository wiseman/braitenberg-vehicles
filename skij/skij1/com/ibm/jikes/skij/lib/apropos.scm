;;; This file is part of Skij.
;;; Author: Michael Travers (mt@watson.ibm.com)

;;; Licensed Materials - See the file license.txt.
;;; (c) Copyright IBM Corp. 1997, 1998. All rights reserved.

;;; +++ only does bound symbols now, maybe that's better than MCL style?
(define (apropos str)
  (set! str (to-string str))
  (for-bindings (lambda (var)
		  (when (>= (invoke (symbol->string var) 'indexOf str) 0)
			(newline)
			(display var)
			(awhen (where-is var)
			       (display " ")
			       (display (list it)))))
		(global-environment)))

(define (for-bindings proc environment)
  (invoke environment 'forBindings proc))

(define (where-is symbol)
  (acond ((not (symbol? symbol))
	  (error "Argument to where-is must be a symbol"))
	 ((%or-null (invoke-static 'com.ibm.jikes.skij.TopEnvironment 'getAutoLoad symbol)
		    #f)
	  it)
	 ((instanceof (eval symbol) 'com.ibm.jikes.skij.PrimProcedure)
	  "primitive")
	 (#t #f)))
	    
  