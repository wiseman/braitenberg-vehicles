;;; This file is part of Skij.
;;; Author: Michael Travers (mt@watson.ibm.com)

;;; Licensed Materials - See the file license.txt.
;;; (c) Copyright IBM Corp. 1997, 1998. All rights reserved.

;;; Object inspector

;;; todo:
; bug: close-all-inspectors leaves pointers, so reinspecting an object does nothing?
; Inspect button gets error if no field selected
; better layout of button
;   improved but still needs work
; shrink window to fit table
; class-specific commands and displays
; arrays should include class probably
; empty array gets error(?)
; refresh button, edit options
;   autorefresh (with thread)
; would be nice if we didn't need button
; clicking on the left-hand column should maybe get the method object, or at least show what class it comes from
; diddle table: no column selection, handle or disallow multiple row selection
; set position of window before making visible?

(define *jive-mode* #f)			;set to t for Jive inspector

(define (jive-inspect obj)
  (set! *jive-mode* #t)
  (inspect obj))

(define (inspect obj)
  (define window
    (hashtable-lookup *inspector-table* obj
		      (lambda (obj)
			(define window (inspect1 obj))
			(invoke window 'addWindowListener
				(new 'com.ibm.jikes.skij.misc.GenericCallback
				     (lambda (evt)
				       (if (= (invoke evt 'getID)
					      window-closing-event-id)
					   (begin
					     (invoke window 'dispose)
					     (hashtable-remove *inspector-table* obj)
					     (set! inspect-top-window #f)
					     (inspect-set-top-object #f))))))
			window)))
  (invoke window 'toFront)
  window)

(begin ; don't autoload the supporting functions
	   
;;; get selected item
(define (selected-item table)
  (define model (invoke table 'getModel))
  (define row (invoke table 'getSelectedRow))
  (list (invoke model 'getValueAt row 0)
	(deadapt (invoke model 'getValueAt row 1))))

(define *inspect-history* '())

(define (inspect-add-to-history obj)
  (push obj *inspect-history*))

;;; this variable is for the user to access the object in topmost inspector
(define inspected #f)
(define (inspect-set-top-object obj)
  (set! inspected obj))

(define window-activate-event-id
  (peek-static 'java.awt.event.WindowEvent 'WINDOW_ACTIVATED))

(define window-closed-event-id
  (peek-static 'java.awt.event.WindowEvent 'WINDOW_CLOSED))

(define window-closing-event-id
  (peek-static 'java.awt.event.WindowEvent 'WINDOW_CLOSING))

;;; used only for positioning new windows
(define inspect-top-window #f)

(define *inspector-table* (make-hashtable))

(define (close-all-inspectors)
  (for-hashtable (lambda (obj window)
		   (invoke window 'dispose))
		 *inspector-table*))

;;; inspector with button 
;;; this function generates a new window
(define (inspect1 obj)

  ;;; these vars are used by refresh; hence defined before value is set
  (define table #f)
  (define table-panel #f)		
;;; This file is part of Skij.
;;; Author: Michael Travers (mt@watson.ibm.com)

;;; Licensed Materials - See the file license.txt.
;;; (c) Copyright IBM Corp. 1997, 1998. All rights reserved.

  (define table-maker #f)

  (define panel (new '#,(swing-class 'JPanel)))
  (define layout (new 'java.awt.GridBagLayout))
  (invoke panel 'setLayout layout)
  (define constraints (new 'java.awt.GridBagConstraints))

  (define (add-component comp)
    (invoke layout 'setConstraints comp constraints)
    (invoke panel 'add comp))

  (add-component
   (make-swing-button "Inspect" (lambda (evt)
				  (let ((item (selected-item table)))
				    (jump obj (car item) (cadr item))))))

  (add-component
   (make-swing-button "Refresh" (lambda (evt)
				 (in-own-thread
				  (define parent (invoke table-panel 'getParent))
				  (invoke parent 'remove table-panel)
				  (table-maker)
				  (invoke parent 'validate)
				  ))))
  
  (define remainder (peek-static 'java.awt.GridBagConstraints 'REMAINDER))

  ;;; this code is wrapped in a lambda so refresh can call it
  (set! table-maker
	(lambda ()
	  (poke constraints 'gridwidth remainder)
	  (poke constraints 'gridheight remainder)
	  (poke constraints 'weightx 1.0)
	  (poke constraints 'weighty 1.0)
	  (poke constraints 'gridx 0)
	  (poke constraints 'gridy 1)
	  (poke constraints 'fill (peek-static 'java.awt.GridBagConstraints 'BOTH))

	  (define data (adapt-inspect-data (inspect-data obj)))
	  (set! table (make-table (cdr data) (car data)))

	  (set! table-panel (make-table-panel table 350 200))
	  (add-component table-panel)))

  (table-maker)

  (inspect-add-to-history obj)

  (define window (make-swing-window-for-panel 
		  (if *jive-mode*
		      (to-string obj)
		      (write-to-string obj))
		  panel))

  (inspect-set-top-object obj)

  (invoke window 'addWindowListener
	  (new 'com.ibm.jikes.skij.misc.GenericCallback
	       (lambda (evt)
		 (if (= (invoke evt 'getID)
			window-activate-event-id)
		     (begin
		       (set! inspect-top-window window)
		       (inspect-set-top-object obj))))))


  (if inspect-top-window
      (invoke window 'setLocation
	      (let ((newloc (new 'java.awt.Point (invoke inspect-top-window 'getLocation))))
		(invoke newloc 'translate 22 22)
		newloc)))

  window)

(define (jump from-obj link to-obj)
  (in-own-thread (inspect to-obj)))

;;; use adaptors so objects get displayed properly

(define (write-adaptor object)
  (let ((adaptor (new 'com.ibm.jikes.skij.misc.Adaptor object)))
    (invoke adaptor 'addBinding 'toString (lambda () (write-to-string object)))
    adaptor))

; destructively add adpators to inspect-data
(define (adapt-inspect-data data)
  (unless *jive-mode*
	  (for-each (lambda (item)
		      (define datum (cadr item))
		      (unless (or (symbol? datum) (number? datum)) ;see write
			      (set-car! (cdr item)
					(write-adaptor datum))))
		    (cdr data)))
  data)

(define (deadapt thing)
  (if (instanceof thing 'com.ibm.jikes.skij.misc.Adaptor)
      (peek thing 'object)
      thing))

(require-write)

) ;end begin


;;; Random stuff
;;; inspect public methods, sorted by name
(define (inspect-class-methods class)
  (unless (instanceof class 'java.lang.Class)
	  (set! class (class-named class)))
  (inspect
   (sort
    (vector->list (invoke class 'getMethods))
    (lambda (a b) (string<? (invoke a 'getName) (invoke b 'getName))))))

;;; inspect a tree of AWT components. This probably belongs elsewhere? Or generalize
;;; to other treelike structures.


(define (inspect-tree from child-generator)
  (define tree 
    (make-tree-window 
     (to-string from)
     (generate-tree from child-generator
		    (lambda (x) x))))
  (tree-add-mouse-listener 
   tree
   (lambda (node evt)
     (inspect (invoke node 'getUserObject))))
  tree)

; inspect an AWT component tree
(define (inspect-components from)
  (inspect-tree from (lambda (comp)
		       (if (instanceof comp 'java.awt.Container)
			   (vector->list (invoke comp 'getComponents))
			   '()))))

