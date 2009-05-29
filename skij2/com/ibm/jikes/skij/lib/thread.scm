;;; This file is part of Skij.
;;; Author: Michael Travers (mt@watson.ibm.com)

;;; Licensed Materials - See the file license.txt.
;;; (c) Copyright IBM Corp. 1997, 1998. All rights reserved.

(defmacro (synchronized thing . body)
  `(%synchronized ,thing (lambda () ,@body)))

(define (run-in-thread thunk)
  (define thread (new 'java.lang.Thread thunk))
  (invoke thread 'start)
  thread)

(defmacro (in-own-thread . body)
  `(run-in-thread 
    (lambda () 
      ,@body)))

(define (current-thread)
  (invoke-static 'java.lang.Thread 'currentThread))

(define (all-threads group)
  (define count (invoke group 'activeCount))
  (define array (%make-vector count (class-named 'java.lang.Thread)))
  (invoke group 'enumerate array)
  (vector->list array))
    
;;; returns threads in current group, probably not right thing
(define (user-threads)
  (all-threads (invoke (current-thread) 'getThreadGroup)))

; n is in milliseconds
(define (sleep n)
  (invoke-static 'java.lang.Thread 'sleep (long n)))

; needs work
(define (thread-inspector)
;  (require 'swing)
  (define bigpanel (new 'com.sun.java.swing.JPanel))
  (invoke bigpanel 'setLayout (new 'java.awt.GridLayout 0 1))
  (for-each (lambda (thread)
	      (define smallpanel (new 'com.sun.java.swing.JPanel))
	      (invoke smallpanel 'add (new 'com.sun.java.swing.JTextArea (invoke thread 'getName)))
	      (invoke smallpanel 'add (make-button 'Suspend (lambda (evt) (invoke thread 'suspend))))
	      (invoke smallpanel 'add (make-button 'Resume (lambda (evt) (invoke thread 'resume))))
	      (invoke smallpanel 'add (make-button 'Interrupt (lambda (evt) (invoke thread 'interrupt))))
	      (invoke bigpanel 'add smallpanel))
	    (user-threads))
  (make-window 'Threads bigpanel))