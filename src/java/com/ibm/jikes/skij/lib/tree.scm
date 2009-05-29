;;; This file is part of Skij.
;;; Author: Michael Travers (mt@watson.ibm.com)

;;; Licensed Materials - See the file license.txt.
;;; (c) Copyright IBM Corp. 1997, 1998. All rights reserved.

; (require 'swing)

;;; trees
;;; new version uses swing, more or less compatible with old

(define node-class '#,(swing-class 'tree.DefaultMutableTreeNode))

(define (make-tree-window title top)
  (define tree (make-tree top))
  (define panel (make-tree-panel tree))
  (make-swing-window-for-panel title panel)
  tree)

(define (make-tree-panel tree)
  (define panel (new '#,(swing-class 'JPanel)))
  (invoke panel 'setLayout (new 'java.awt.GridLayout 1 1))
  (define scroller (new '#,(swing-class 'JScrollPane)))
  (invoke (invoke scroller 'getViewport) 'add tree)
  (invoke panel 'add scroller)
  panel)

(define (make-tree top)
  (if (not top)
      (set! top (new node-class 'top)))
  (new '#,(swing-class 'JTree) top))

(define (node? n)
  (instanceof n '#,(class-named node-class)))

(define (make-node thing)
  (new node-class thing))

(define (coerce-node thing)
  (if (node? thing)
      thing
      (make-node thing)))

; you can't actually change the root node, so we just change the value
(define (set-root tree thing)
  (if (node? thing)
      (error '"can't set root of tree to new node"))
  (invoke (root-node tree) 'setUserObject thing)
  (root-node tree))

(define (root-node tree)
  (invoke (invoke tree 'getModel) 'getRoot))

; has a new argument
(define (add-child tree node child)
  (define nchild #f)
  (synchronized 
   tree
   (set! nchild (coerce-node child))
   (invoke node 'add nchild))
  (if tree (invoke (invoke tree 'getModel) 'reload))
  nchild)

(define (node-parent node)
  (invoke node 'getParent))

; has a new argument
(define (set-open tree node open?)
  (define path (new '#,(swing-class 'tree.TreePath) (invoke node 'getPath)))
  (invoke tree (if open? 'expandPath 'collapsePath) path))

;;; generalized tree builder (returns top node)
(define (generate-tree root child-generator node-content-generator)
  (define (generate-tree-1 item)
    (define node (make-node (node-content-generator item)))
    (for-each (lambda (child)
		(add-child #f node (generate-tree-1 child)))
	      (child-generator item))
    node)
  (generate-tree-1 root))

; +++ could avoid consing procedure
(define (make-adaptor name contents)
  (define a (new 'com.ibm.jikes.skij.misc.Adaptor contents))
  (invoke a 'addBinding 'toString (lambda () name))
  a)
 
;;; add a listener for clicks. PROC is called with 2 args (node, evt) WAS (node, x, y)
(define (tree-add-mouse-listener tree proc)
  (define listener
    (new 'com.ibm.jikes.skij.misc.GenericCallback 
	 (lambda (evt)
	   (if (and (eq? tree (invoke evt 'getComponent)) ;ignore events for other objects
		    (= (invoke evt 'getID) #,(peek-static 'java.awt.event.MouseEvent 'MOUSE_CLICKED))) ;and non-click events
	       (begin
		 (define path (%or-null (invoke tree 'getPathForLocation (invoke evt 'getX) (invoke evt 'getY)) #f))
		 (awhen path
		    (define node (invoke path 'getLastPathComponent))
		    (proc node evt)))))))
  (invoke tree 'addMouseListener listener)
  listener)




