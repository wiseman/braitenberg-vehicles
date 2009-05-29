;;; This file is part of Skij.
;;; Author: Michael Travers (mt@watson.ibm.com)

;;; Licensed Materials - See the file license.txt.
;;; (c) Copyright IBM Corp. 1997, 1998. All rights reserved.

; turning into a more general console
; Interrupter only works some of the time

(define *old-listener-thread* #f)

; listener threads/objects are weird; all the listening stuff goes on inside
; the constructor of SchemeListener, thus the (new SchemeListener) NEVER 
; RETURNS. When stop is called, it throws ThreadDeath from the constructor.

(define (make-interrupter)
  (define thread (current-thread))
  (define window #f)
  (define panel (new 'java.awt.Panel))
  (define button
    (make-button 'Interrupt 
		 (lambda (evt)
		   (newline)
		   (print "Creating new Listener, old thread is in *old-listener-thread*")
		   (set! *old-listener-thread* thread)
		   (invoke thread 'stop)
		   (run-in-thread (lambda ()
				    (make-interrupter)
				    (catch    ; catches ThreadDeath
				     (invoke (new 'com.ibm.jikes.skij.SchemeListener) 'repl))))
		   (invoke window 'dispose))))
  (define checkbox (new 'java.awt.Checkbox "Trace"))
  (invoke checkbox 'addItemListener
	  (new 'com.ibm.jikes.skij.misc.GenericCallback
	       (lambda (evt)
		 (trace (= (invoke evt 'getStateChange)
			   (peek-static 'java.awt.event.ItemEvent 'SELECTED))))))
  (invoke panel 'add button)
  (invoke panel 'add checkbox)
  (set! window (make-window-for-panel "Skij Control" panel)))

(make-interrupter)

