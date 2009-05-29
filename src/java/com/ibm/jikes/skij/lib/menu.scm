;;; This file is part of Skij.
;;; Author: Michael Travers (mt@watson.ibm.com)

;;; Licensed Materials - See the file license.txt.
;;; (c) Copyright IBM Corp. 1997, 1998. All rights reserved.

;;; Menus

(define (make-popup-menu menu-items)
  (define menu (new 'java.awt.PopupMenu))
  (for-each (lambda (menu-item)
	      (invoke menu 'add menu-item))
	    menu-items)
  menu)

(define (make-menu title menu-items . keys)
  (key keys enabled? #t)
  (key keys bold? #f)
  (define menu (new 'java.awt.Menu title))
  (for-each (lambda (menu-item)
	      (invoke menu 'add menu-item))
	    menu-items)
  (set-menu-item-properties menu enabled? bold?)
  menu)

(define (display-popup-menu menu component x y)
  (invoke (invoke component 'getParent) 'add menu)
  (invoke menu 'show component x y))
	    
(define (make-menu-item title . keys)
  (key keys procedure #f)
  (key keys enabled? #t)
  (key keys bold? #f)
  (define menu-item (new 'java.awt.MenuItem title))
  (if procedure
      (invoke menu-item 'addActionListener 
	      (new 'com.ibm.jikes.skij.misc.GenericCallback procedure)))
  (set-menu-item-properties menu-item enabled? bold?)
  menu-item)

(define (set-menu-item-properties menu-item enabled? bold?)
  (invoke menu-item 'setEnabled enabled?)
  (if bold?
      (invoke menu-item 'setFont
	      (make-font (invoke menu-item 'getFont) #f (peek-static 'java.awt.Font 'BOLD)  #f))))
  
; belongs elsewhere
(define (make-font from-font name style size)
  (new 'java.awt.Font
       (or name (invoke from-font 'getName))
       (or style (invoke from-font 'getStyle)) ; or should we OR in style bits?
       (or size (invoke from-font 'getSize))))