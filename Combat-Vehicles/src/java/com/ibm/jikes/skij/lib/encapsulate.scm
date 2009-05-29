;;; This file is part of Skij.
;;; Author: Michael Travers (mt@watson.ibm.com)

;;; Licensed Materials - See the file license.txt.
;;; (c) Copyright IBM Corp. 1997, 1998. All rights reserved.

; +++ this should support around in addition to or instead of before and after
(define (encapsulate proc-name before after)
  (save-procedure proc-name)
  (define original (eval proc-name))
  (define encapsulated
    (lambda args
      (before args)
      (define result (apply original args))
      (after result)
      result))
  (eval `(set! ,proc-name ',encapsulated)))

(define *saved-procs* (make-hashtable))

(define (save-procedure name)
  (hashtable-put *saved-procs* name (eval name)))

(define (restore-procedure name)
  (eval `(set! ,name (hashtable-get *saved-procs* ',name))))

(define (restore-all-procedures)
  (map-hashtable (lambda (name proc)
		   (restore-procedure name))
		 *saved-procs*))

