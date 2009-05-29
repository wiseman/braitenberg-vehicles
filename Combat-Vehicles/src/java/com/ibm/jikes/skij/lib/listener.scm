;;; This file is part of Skij.
;;; Author: Michael Travers (mt@watson.ibm.com)

;;; Licensed Materials - See the file license.txt.
;;; (c) Copyright IBM Corp. 1997, 1998. All rights reserved.

;;; make a listener in AWT or Swing

;;; Java implementations are very bad about sticking to either CR or LF
;;; as a terminator.
(define (line-end? x)
  (if (char? x)
      (set! x (char->int x)))
  (or (= x 10) (= x 13)))


(define *avoid-1.1-events* #f)

(define (make-swing-listener-window)
  (set! *avoid-1.1-events* #f)		;ugly but will do for now
  (define textarea (new '#,(swing-class 'JTextArea)))
  (define panel (new '#,(swing-class 'JPanel) (new 'java.awt.GridLayout 1 1)))
  (define scroller (new '#,(swing-class 'JScrollPane)))
  (invoke (invoke scroller 'getViewport) 'add textarea)
  (invoke panel 'add scroller)
  (define w (make-swing-window-for-panel 'Listener panel))
  (define thread (make-listener** textarea))
  (add-window-close-handler
   w (lambda () (print `(stopping ,thread)) (invoke thread 'stop)))
  (invoke w 'setSize 300 200)
  (invoke w 'show)			;trying to get layout to happen properly.
  (invoke textarea 'grabFocus)
  textarea)

(define (make-awt-listener-window)
  (define textarea (new (if *avoid-1.1-events*
			    'com.ibm.jikes.skij.misc.ListenerTextArea
			    'java.awt.TextArea)))
  (define panel (new 'java.awt.Panel))
  (invoke panel 'setLayout (new 'java.awt.GridLayout 1 1))
  (invoke panel 'add textarea)
  (define w (make-window-for-panel "Listener" panel))
  (define thread (make-listener** textarea))
  (invoke w 'setMenuBar (awt-listener-menubar w))
  (add-window-close-handler
   w (lambda () (print `(stopping ,thread)) (invoke thread 'stop)))
  textarea)

(begin ; suppress autoload

; too expensive in load time
'(defstruct listener
  text-area
  input-start)

; so do it quick-n-dirty
(define make-listener cons)
(define listener-text-area car)
(define listener-input-start cdr)
(define set-listener-input-start! set-cdr!)

;;; always write to end
(define (listener-write listener string)
  (let* ((textarea (listener-text-area listener))
	 (swing? (not (instanceof textarea 'java.awt.TextArea) )))
    (invoke textarea
	    (if swing? 'append 'appendText) ;see note in Skijlet.java
	    string)
    (if swing? (invoke textarea 'setCaretPosition (listener-end listener)))
    (set-listener-input-start! listener (listener-end listener))))

;;; read from input-start to current position, and set input/caret to end
;;; not currently used
(define (listener-read listener)
  (define string (invoke (listener-text-area listener) 
			 'getText 
			 (listener-input-start listener)
			 (- (listener-end listener) 
			    (listener-input-start listener))))
  (set! input-start (listener-end listener))
  string)

;;; we might as well get the whole string, because there appears to be no other
;;; way to get the size of the buffer
;;; +++ this seems to always get called with n=1. So, very inefficient, if
;;; getText is doing anything nontrivial.
(define (listener-read-n listener n)
  (define start (listener-input-start listener))
  (do ((text (invoke (listener-text-area listener) 'getText)
	     (invoke (listener-text-area listener) 'getText)))
      ((>= (invoke text 'length)
	   (+ start n))
       (set-listener-input-start! listener (+ start n))
       (substring text start (+ start n)))
    (synchronized 
     listener
     (invoke listener 'wait))))

(define (listener-end listener)
  (invoke (invoke (listener-text-area listener) 'getText) 'length))

(define key-typed
  (let ((v (catch (peek-static 'java.awt.event.KeyEvent 'KEY_TYPED))))
    (if (instanceof v 'java.lang.Throwable) #f v)))

) ;end begin



(define (awt-listener-menubar frame)
  (define directory (%null))
  (define (load-proc . args)
    (let ((fd (new 'java.awt.FileDialog frame "Open File"
                   (peek-static 'java.awt.FileDialog 'LOAD))))
      (invoke fd 'setDirectory directory)
      (invoke fd 'show)
      (set! directory (invoke fd 'getDirectory))
      (let ((f (new 'java.io.File
                    (invoke fd 'getDirectory)
                    (invoke fd 'getFile))))
        (load (invoke f 'getPath)))))
  (let ((menubar (new 'java.awt.MenuBar)))
    (let ((file (new 'java.awt.Menu "File")))
      (invoke menubar 'add file)
      (let ((load (make-menu-item "Load..."
                                  'procedure load-proc)))
        (invoke file 'add load)))
    menubar))


;;; Do most of the work.
(define (make-listener** text-area)
  (define thread #f)
  (make-listener*** 
   text-area
   (lambda (reader writer)
     (define sl (new 'com.ibm.jikes.skij.SchemeListener reader writer #t))
     (set! thread (new 'java.lang.Thread
		       (lambda ()
			 (invoke sl 'repl))
		       "Skij Listener"))
     (invoke thread 'start)))
  thread)

;;; Originally this did not make a new thread, but that hung for reasons unknown.
(define (make-applet-listener text-area)
  (define thread #f)
  (make-listener*** 
   text-area
   (lambda (reader writer)
     (define sl (new 'com.ibm.jikes.skij.SchemeListener reader writer #t))
     (poke-static 'com.ibm.jikes.skij.Environment 'top
		  (invoke sl 'getTopEnv))
     (set! thread (new 'java.lang.Thread
		       (lambda ()
			 (invoke sl 'repl))
		       "Skij Listener"))
     (invoke thread 'start)))
  thread)

;;; listener-proc is a procedure that is passed 2 args: reader and writer
(define (make-listener*** text-area listener-proc)
  (define swing? (not (instanceof text-area 'java.awt.TextArea)))
  (define l
    (make-listener text-area
		   0))
  (define reader
    (new 'com.ibm.jikes.skij.SchemeReader
	 ; +++ doesn't handle eof or other conditions
	 (lambda (chars start count)
	   (define string (listener-read-n l count))
	   (define rcount (invoke string 'length))
	   (define index 0)
	   (define bytes (invoke string 'getBytes))
	   (do ()
	       ((= index rcount))
	     (vector-set! chars (+ start index) (int->char (vector-ref bytes index)))
	     (set! index (+ 1 index)))
	   rcount)
	 (lambda () ) ))

  (define writer
    (new 'com.ibm.jikes.skij.SchemeWriter
	 (lambda (chars start count)
	   (define string (new 'java.lang.String chars start count))
	   (listener-write l string))
	 (lambda () )
	 (lambda () ) ))

  (if *avoid-1.1-events*
      (poke (listener-text-area l) 'handler
	    (lambda (evt key)
	      (if (line-end? key)
		  (synchronized
		   l
		   (invoke l 'notify)
		   #f)
		  #f)))
      ;; 1.1 model handler 
      (invoke (listener-text-area l) 'addKeyListener 
	      (new 'com.ibm.jikes.skij.misc.GenericCallback 
		   (lambda (evt)
		     (if (and (equal? (invoke evt 'getID) key-typed)
			      (line-end? (invoke evt 'getKeyChar)))
			 (synchronized 
			  l
			  (invoke l 'notify))))))
      )


;;; substitute for stupid browsers

  (listener-proc reader writer)
  
  )
      


