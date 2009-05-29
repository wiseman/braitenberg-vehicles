;;; This file is part of Skij.
;;; Author: Michael Travers (mt@watson.ibm.com)

;;; Licensed Materials - See the file license.txt.
;;; (c) Copyright IBM Corp. 1997, 1998. All rights reserved.

;;; only works on 1-arg functions, equality as is defined by hashtables.
;;; optional second arg can provide pre-lookup processing to argument.
;;; ie (define-memoized (person-record name (lambda (n) (invoke n 'intern))) ...)
;;;  special abstraction violater tool: 
(defmacro (define-memoized form . body)
  (let ((fname (car form))
	(arg (cadr form))
	(process (cddr form)))		;optional arg: process key before hash
    `(begin 
       (define ,fname #f)
       (let ((ht (make-hashtable)))
	 (set! ,fname
	       (lambda (key)
		 (if (eq? key ':hashtable)
		     ht
		     (hashtable-lookup ht
				       ,(if (null? process) 
					    'key
					    `(,(car process) key))
				       (lambda (,arg)
					 ,@body)))))))))

