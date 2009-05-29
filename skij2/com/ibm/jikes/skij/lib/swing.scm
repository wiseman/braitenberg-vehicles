;;; This file is part of Skij.
;;; Author: Michael Travers (mt@watson.ibm.com)

;;; Licensed Materials - See the file license.txt.
;;; (c) Copyright IBM Corp. 1997, 1998. All rights reserved.

;;; This file is part of Skij.
;;; Author: Michael Travers (mt@watson.ibm.com)

;;; Licensed Materials - See the file license.txt.
;;; (c) Copyright IBM Corp. 1997, 1998. All rights reserved.

;;; random set of interfaces to Swing

;in JDK1.2beta --  java.awt.swing
(define swing-package
  (if jdk1.2?
      'javax.swing
      'com.sun.java.swing))

(define (swing-class name)
  (symbol-conc swing-package "." name))

(define (make-swing-window name width height)
  (define w (new '#,(swing-class 'JFrame) (string name)))
  (primp-window w width height #f))

(define (make-swing-window-for-panel name panel)
  (define w (new '#,(swing-class 'JFrame) (string name)))
  (define content-pane (invoke w 'getContentPane))
  (invoke content-pane 'add panel)
  (invoke w 'pack)
  (invoke w 'show)
  w)

(define (make-swing-button name action)
  (define b (new '#,(swing-class 'JButton) (string name)))
  (define listener (new 'com.ibm.jikes.skij.misc.GenericCallback action))
  (invoke b 'addActionListener listener)
  b)

; panel/scroller/table

(define (make-table data columns)
  (define data (lists->array data))
  (define columns (list->vector columns))
  (new '#,(swing-class 'JTable) data columns))

(define (make-table-panel table width height)
  (define panel (new '#,(swing-class 'JPanel)))
  (invoke table 'setPreferredScrollableViewportSize 
	  (new 'java.awt.Dimension width height))
  (define scroller (invoke table 'createScrollPaneForTable table))
  (invoke panel 'setLayout (new 'java.awt.GridLayout 1 1))
  (invoke panel 'add scroller)
  panel)

;;; given a list of lists, make a 2D array. Assumes all inner lists have the same length
(define (lists->array lists)
  (define len (length lists))
  (define wid (if (null? lists)
		  (error '"No elements for array")
		  (length (car lists))))
  (define array (%make-array (list len wid) (class-named 'java.lang.Object)))
  (define i 0)
  (for-each (lambda (lst)
	      (define j 0)
	      (for-each (lambda (elt)
			  (aset array elt i j)
			  (set! j (+ 1 j)))
			lst)
	      (set! i (+ 1 i)))
	    lists)
  array)

(define (containers component)
  (if component
      (cons component
	    (containers (invoke component 'getParent)))
      '()))

