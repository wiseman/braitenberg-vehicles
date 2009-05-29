;;; Silly reproducing button example. Illustrates basic object creation,
;;; method calling, and callbacks from AWT events.

(define button-window (make-window "Buttons" 400 300))
(invoke button-window 'setLayout (new 'java.awt.FlowLayout))

(define (make-reproducing-button parent)
  (let ((button (make-button
		 (if parent 
		     (string-append "Child of " (invoke parent 'getLabel))
		     "Eve")
		 (lambda (evt)
		   (make-reproducing-button (invoke evt 'getSource)))))) 
    (invoke button-window 'add button)
    (invoke button-window 'show)	;force redisplay
    button))

(define (make-button name action)
  (let* ((b (new 'java.awt.Button name))
	 (listener (new 'com.ibm.jikes.skij.misc.GenericCallback action)))
    (invoke b 'addActionListener listener)
    b))

(make-reproducing-button #f)
