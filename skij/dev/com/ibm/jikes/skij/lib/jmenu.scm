;;; This file is part of Skij.
;;; Author: Michael Travers (mt@watson.ibm.com)

;;; Licensed Materials - See the file license.txt.
;;; (c) Copyright IBM Corp. 1997, 1998. All rights reserved.

;;; Swing menus

(define (make-jpopup-menu menu-items)
  (define menu (new (swing-class 'JPopupMenu))
  (for-each (lambda (menu-item)
	      (invoke menu 'add menu-item))
	    menu-items)
  menu)

(define (display-jpopup-menu menu component x y)
  (invoke menu 'show component x y))
	    
(define (make-jmenu-item title . keys)
  (key keys procedure #f)
  (key keys enabled? #t)
  (key keys bold? #f)
  (define menu-item (new (swing-class 'JMenuItem) title))
  (if procedure
      (invoke menu-item 'addActionListener 
	      (new 'com.ibm.jikes.skij.misc.GenericCallback procedure)))
  (invoke menu-item 'setEnabled enabled?)
  (if bold?
      (invoke menu-item 'setFont
	      (make-font (invoke menu-item 'getFont) #f (peek-static 'java.awt.Font 'BOLD)  #f)))
  menu-item)  
  
; belongs elsewhere
(define (make-font from-font name style size)
  (new 'java.awt.Font
       (or name (invoke from-font 'getName))
       (or style (invoke from-font 'getStyle)) ; or should we OR in style bits?
       (or size (invoke from-font 'getSize))))