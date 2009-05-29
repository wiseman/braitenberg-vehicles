;;; object grapher; works with inspector

;;; todo
; use shape for arrays
; fading, colorizing
;  partially done
; 2 links between same pair of objects is way ugly
; links to self are also weird (ie TopEnvironment)
; better layout
; deletion bugs: links don't always go away
; directional links
; use relaxation (started)
; auto-layout
;   stop relaxaton to eliminate constant jiggle
;   somehow lock objects
; mouse-selection is pretty sucky; needs to know about layering
; path-generator

;;; interesting discovery: getGraphics returns a new object each time it is called!
(begin ; don't autoload -- these must come first because of read-time-eval below
(define (random-color)
  (random-bright-color 100))

(define (random-bright-color min-component)
  (define (random-bright-component)
    (+ min-component (integer (random-range (- 256 min-component)))))
  (new 'java.awt.Color (random-bright-component) (random-bright-component) (random-bright-component)  ))

) ;end begin
(begin ; don't autoload 
(define-memoized (ob-node object)
  (invoke *object-graph* 
	  'addNode
	  (truncate-string (to-short-string object) 25)
	  object
	  (object-color object)))

(define (delete-node node)
  (let ((ht (ob-node ':hashtable))
	(ob (node-ob node)))
    (hashtable-remove ht ob))
  (invoke *object-graph* 'deleteNode node)
  (invoke *object-graph* 'repaint))

(define (node-ob node)
  (invoke *object-graph* 'nodeObject node))

(define (truncate-string string n)
  (if (> (string-length string) n)
      (string-append (substring string 0 n) "...")
      string))
  
(define (extend-all ob)
  (for-each (lambda (entry)
	      (define nobj (cadr entry))
	      (unless (or (instanceof nobj 'java.lang.Number)
			  (instanceof nobj 'java.lang.Boolean))
		      (catch			;null pointer errors, ie
		       (add-edge ob (car entry) nobj))))
	    (cdr (inspect-data ob))))
    
(define (add-edge from link to)
  (invoke *object-graph* 'addEdge (ob-node from) (ob-node to) (to-string link)))

;;; remove package prefixes
;;; +++ doesn't do well on arrays
(define (to-short-string object)
  (define string (to-string object))
  (define class-name (invoke (invoke object 'getClass) 'getName))
  (define dot-pos (invoke class-name 'lastIndexOf (char->int #\.)))
  (if (positive? dot-pos)
      (string-replace-string string (substring class-name 0 (+ 1 dot-pos)) "")
      string))

;;; Relaxation

(define relaxer #f)
(define (relax val)
  (if val
      (begin
	(invoke *object-graph* 'setSpread (double val))
	(unless relaxer
		(set! relaxer (in-own-thread (invoke *object-graph* 'run)))))
      (begin
	(catch (invoke relaxer 'stop))
	(set! relaxer #f))))

;;; Menus

; +++ inspect-data does too much; we just need members, not values
(define (make-node-popup-menu node)
  (let ((extensions (cdr (inspect-data (node-ob node)))))
    (make-popup-menu
     (list
      (make-menu-item 
       "Inspect"
       'procedure (lambda args (in-own-thread (inspect (node-ob node)))))
      (make-menu "Extend" 
		 (map (lambda (item)
			(define label (to-string (car item)))
			(make-menu-item label
					'procedure 
					(lambda args
					  (add-edge (node-ob node) label (cadr item))
					  (invoke *object-graph* 'repaint))
					'enabled? (not (%%null? (cadr item)))))
		      extensions)
		 'enabled? (not (null? extensions)))
    (make-menu-item
     "Extend All"
     'procedure (lambda args (in-own-thread
			      (extend-all (node-ob node))
			      (invoke *object-graph* 'repaint)))
     'enabled? (not (null? extensions)))
    (make-menu-item "Delete" 
		    'procedure
		    (lambda args (delete-node node)))))))

;;; Mousing

(define graph-listener 
  (let ((pick #f))
    (new 'com.ibm.jikes.skij.misc.GenericCallback
	 (lambda (evt)
	   (define id (invoke evt 'getID))
	   (case id
	     ((#,(peek-static 'java.awt.event.MouseEvent 'MOUSE_PRESSED))
	      (define x (invoke evt 'getX))
	      (define y (invoke evt 'getY))
	      (set! pick (%or-null (invoke *object-graph* 'closestNode x y) #f))
	      (if (and pick
		       (bit-test (invoke evt 'getModifiers) 
				 #,(peek-static 'java.awt.event.InputEvent 'BUTTON3_MASK)))
		  (display-popup-menu (make-node-popup-menu pick) *object-graph* x y)
;		  (unfade pick)
		  (invoke *object-graph* 'moveNode pick x y)))
	     ((#,(peek-static 'java.awt.event.MouseEvent 'MOUSE_RELEASED))
	      (set! pick #f))
	     ((#,(peek-static 'java.awt.event.MouseEvent 'MOUSE_DRAGGED))
	      (if pick
		  (invoke *object-graph* 'moveNode pick
			  (invoke evt 'getX) (invoke evt 'getY))))
	     )))))

;;; no logical and in scheme!
(define (bit-test word bit)
  (odd? (quotient word bit)))

; Hook to Inspector

(require 'inspect)
(define original-jump #f)
(if (not original-jump)
    (set! original-jump jump))

(define (jump from link to)
  (add-edge from link to)
  (invoke *object-graph* 'repaint)
  (original-jump from link to))

(define original-inspect #f)

(if (not original-inspect)
    (set! original-inspect inspect))

(define (inspect thing)
  (ob-node thing)
  (invoke *object-graph* 'repaint)
  (original-inspect thing))


; Color

; see random-color etc.


(define (object-color obj)
  (class-color (invoke obj 'getClass)))

(define-memoized (class-color class)
  (cond ((invoke class 'isArray)
	 '#,(random-color))
	(#t
	 (case class
	   ((#,(class-named 'java.lang.String))
	    '#,(random-color))
	   ((#,(class-named 'java.lang.Class))
	    '#,(random-color))
	   (else
	    (package-color (class-package class)))))))

(define (class-package class)
  (define class-name (invoke class 'getName))
  (define dotpos (invoke class-name 'lastIndexOf (char->int #\.)))
  (define package-prefix
    (if (> dotpos 0)
	(substring class-name 0 dotpos)
	"unnamed"))
  (intern package-prefix))
  
(define-memoized (package-color package-name)
  (random-color))

(define (paint-class-color-map g)
  (define i 1)
  (define (paint-map-entry name color)
    (invoke g 'setColor color)
    (invoke g 'drawString name 20 (* i 20))
    (set! i (+ i 1)))
  (map-hashtable (lambda (pkg color)
		   (paint-map-entry (string-append (to-string pkg) ".*") color))
		 (package-color ':hashtable))
  (define (paint-class-entry class)
    (paint-map-entry (invoke class 'getName) (class-color class)))
  (paint-class-entry (class-named 'java.lang.String))
  (paint-class-entry (class-named 'java.lang.Class)))

(define background-color (new 'java.awt.Color 60 60 60))

(define *object-graph* #f)    

(define (paint-graph-panel g)
  (invoke g 'setColor background-color)
  (invoke g 'fillRect 0 0 1000 1000)	;+++ get dimensions
  (paint-class-color-map g)
  (invoke *object-graph* 'paintNodes g))

(define (make-graph-window)
  (define p (new 'com.ibm.jikes.skij.misc.GraphPanel paint-graph-panel))
  (define w (make-window "Object Graph" 300 300))
  (invoke w 'add p)
  (invoke p 'addMouseListener graph-listener)
  (invoke p 'addMouseMotionListener graph-listener)
  (set! *object-graph* p)
  (invoke w 'show)
  w)

); end begin

(define (graph-inspect obj)
  (if (not *object-graph*)
      (make-graph-window))
  (ob-node obj)
  (invoke *object-graph* 'repaint)
  (invoke (invoke *object-graph* 'getParent) 'show))

  
      

