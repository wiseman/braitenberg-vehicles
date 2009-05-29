;;; This file is part of Skij.
;;; Author: Michael Travers (mt@watson.ibm.com)

;;; Licensed Materials - See the file license.txt.
;;; (c) Copyright IBM Corp. 1997, 1998. All rights reserved.

;;; applet runner -- not fully developed, but can start most simple applets.

(define *last-applet* #f)

(define (run-applet class-or-name base-url width height parameters)
  (install-null-security-manager)
  (map (lambda (param)
	 (set-car! param (to-string (car param))))
       parameters)
  (let ((applet (new class-or-name))
	(stub (new 'com.ibm.jikes.skij.misc.SkijAppletStub 
		   parameters
		   (new 'java.net.URL base-url)
		   (new 'java.net.URL base-url)))
	(window (make-window (string-append "Applet " (string class-or-name)) width height)))
    (set! *last-applet* applet)
    (invoke applet 'setStub stub)
    (invoke applet 'init)
    (invoke applet 'start)
    (invoke window 'add applet)
    (invoke window 'show)
    applet))

;;; to load images, a security manager must be set up...
(define (install-null-security-manager)
  (if (%%null? (invoke-static 'java.lang.System 'getSecurityManager))
      (let ((manager (new 'com.ibm.jikes.skij.misc.SkijSecurityManager
			  (lambda (type . args)
			    ;; applets that exit java are really annoying!
			    (cond ((equal? type "checkExit") #f)
				  (#t #t))))))
	(invoke-static 'java.lang.System 'setSecurityManager manager)
	manager)))
