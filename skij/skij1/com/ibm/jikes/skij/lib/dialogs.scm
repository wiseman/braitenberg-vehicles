;;; This file is part of Skij.
;;; Author: Michael Travers (mt@watson.ibm.com)

;;; Licensed Materials - See the file license.txt.
;;; (c) Copyright IBM Corp. 1997, 1998. All rights reserved.

;;; utility dialogs

;;; block and return value
;;; +++ can I abstract the thread management stuff here?
;;; +++ needs better layout
;;; +++  keys for size
(define (string-from-user prompt . keys)
  (let* ((thread #f)
	 (lock (cons 1 2))		;unique cell...
	 (value #f)
	 (cont   (lambda (xvalue)
		    (synchronized 
		     lock
		     (set! value xvalue)
		     (invoke lock 'notify)))))
    (set! thread 
	  (in-own-thread
	   (apply string-from-user1 
		  prompt
		  cont
		  (cons 'cancel-continuation (cons (lambda () (cont #f)) keys)))))
    (synchronized
     lock
     (invoke lock 'wait))
    (if (string? value)
	value
	(throw "User Cancelled"))))

(define (string-from-user1 prompt continuation . keys)
  (key keys password #f)
  (key keys cancel-continuation (lambda () (error "User Cancelled")))
  (define w (make-window "String Prompt" 300 150))
  (invoke w 'setLayout (new 'java.awt.FlowLayout))
  (define prompt (new 'java.awt.Label prompt ))
  (invoke w 'add prompt)
  (define response (new 'java.awt.TextField "" 30))
  (if password
      (invoke response 'setEchoChar #\•))
  (invoke w 'add response)
  (invoke w 'add (make-button "OK" 
			      (lambda (evt)
				(invoke w 'dispose)
				(continuation
				 (invoke response 'getText)))))
  (invoke w 'add (make-button "Cancel"
			      (lambda (evt)
				(invoke w 'dispose)
				(cancel-continuation))))
  (invoke w 'show))