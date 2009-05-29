;;; This file is part of Skij.
;;; Author: Michael Travers (mt@watson.ibm.com)

;;; Licensed Materials - See the file license.txt.
;;; (c) Copyright IBM Corp. 1997, 1998. All rights reserved.

(define (make-window name width height)
  (define w (new 'java.awt.Frame name))
  (primp-window w width height #f)
  w)

(define (make-window-for-panel title panel)
  (define w (new 'java.awt.Frame title))
  (invoke w 'add panel)
  (invoke w 'pack)
  (invoke w 'show)
  (add-window-close-handler w (lambda () #f))
  w)

(define (primp-window w width height proc)
  (invoke w 'setSize width height)
  (invoke w 'setVisible #t)
  (add-window-close-handler w (or proc (lambda () #f))))

(define window-closing-event-id
  (peek-static 'java.awt.event.WindowEvent 'WINDOW_CLOSING))

; proc is a thunk that performs any additional closing operations
(define (add-window-close-handler window proc)
  (invoke window 'addWindowListener
	  (new 'com.ibm.jikes.skij.misc.GenericCallback
	       (lambda (evt)
		 (if (= (invoke evt 'getID)
			window-closing-event-id)
		     (begin
		       (invoke window 'dispose)
		       (proc)))))))

; painter is a procedure that is called with the Graphics object
(define (make-refreshed-window name width height painter)
  (define w (new 'com.ibm.jikes.skij.misc.Window name painter))
  (primp-window w width height #f)
  w)

; action should be a procedure of one argument, which will be an AWT ActionEvent
(define (make-button name action)
  (define b (new 'java.awt.Button (to-string name)))
  (define listener (new 'com.ibm.jikes.skij.misc.GenericCallback action))
  (invoke b 'addActionListener listener)
  b)