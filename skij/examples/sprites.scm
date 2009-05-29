;;; Some silly graphics examples.

;;; (make-rect-window) Puts up a window with some colored 
;;; rectangles, which move on their own and can also be dragged.

;;; (make-cannonade) puts up an animated fountain of balls.

;;; Illustrates:
;;;    defstruct (a simple structure facility)
;;;    graphics (including defining repaint methods with lambda)
;;;    threads and synchronization
;;;    event handling

;;; +++ use DBPanel instead of doing our own buffering?

;;; Sprites

(defstruct sprite x y xs ys drawme clock)

(define (paint sprite g)
  ((sprite-drawme sprite) sprite g))

(define (move-sprite sprite dx dy)
  (set-sprite-x! sprite (+ (sprite-x sprite) dx))
  (set-sprite-y! sprite (+ (sprite-y sprite) dy)))

(define (clock sprite)
  ((sprite-clock sprite) sprite))

(define (random-color)
  (new 'java.awt.Color (float (random #f)) (float (random #f)) (float (random #f))))

(define (make-random-square)
  (define xv (arand 0 4))
  (define yv (arand 0 4))
  (define persistance (random 25))
  (define color (random-color))
  (make-sprite (random 300) (random 300) (+ 20 (random 100)) (+ 20 (random 100))
	       (lambda (sprite g)
		 (invoke g 'setColor color)
		 (invoke g 'fillRect (integer (sprite-x sprite)) (integer (sprite-y sprite)) (integer (sprite-xs sprite)) (integer (sprite-ys sprite))))
	       (lambda (sprite)
		 (move-sprite sprite xv yv)
		 (when (= 0 (random persistance))
		       (set! xv (arand 0 4))
		       (set! yv (arand 0 4))))))

;;; Mousing

(define (point-in-sprite? sprite x y)
  (and (<= (sprite-x sprite) x)
       (<= (sprite-y sprite) y)
       (<= x (+ (sprite-x sprite) (sprite-xs sprite)))
       (<= y (+ (sprite-y sprite) (sprite-ys sprite)))))

(define mouse-clicked-event-id (peek-static 'java.awt.event.MouseEvent 'MOUSE_CLICKED))
(define mouse-pressed-event-id (peek-static 'java.awt.event.MouseEvent 'MOUSE_PRESSED))
(define mouse-released-event-id (peek-static 'java.awt.event.MouseEvent 'MOUSE_RELEASED))
(define mouse-dragged-event-id (peek-static 'java.awt.event.MouseEvent 'MOUSE_DRAGGED))

;;; Sprite manager handles most stuff

(defstruct sprite-manager sprites window buffer)

(define (make-sprite-window name)
  (define manager (make-sprite-manager '() #f #f))
  (define w 
    (make-window
     name 300 300 ))
  (define panel
    (new 'com.ibm.jikes.skij.misc.Panel   
     (lambda (graphics)
       (for-each (lambda (sprite)
		   ((sprite-drawme sprite) sprite graphics))
		 (sprite-manager-sprites manager)))))
  (invoke w 'add panel)
  (set-sprite-manager-window! manager w)
  (let* ((dragged #f)
	 (drag-x-offset 0) (drag-y-offset 0)
	 (listener (new 'com.ibm.jikes.skij.misc.GenericCallback
			(lambda (evt)
			  ;(print `(evt ,(invoke evt 'getID)))
			  (case (invoke evt 'getID)
			    ((#,mouse-pressed-event-id)
			     (define x (invoke evt 'getX))
			     (define y (invoke evt 'getY))
			     (for-each (lambda (sprite)
					 (when (point-in-sprite? sprite x y)
					       (set! dragged sprite)
					       (set! drag-x-offset
						     (- (sprite-x sprite) x))
					       (set! drag-y-offset
						     (- (sprite-y sprite) y))))
				       (sprite-manager-sprites manager)))
			    ((#,mouse-dragged-event-id)
			     (when dragged
				   (set-sprite-x! dragged (+ drag-x-offset (invoke evt 'getX)))
				   (set-sprite-y! dragged (+ drag-y-offset (invoke evt 'getY)))
				   (manager-repaint manager)))
			    ((#,mouse-released-event-id)
			     (set! dragged #f)))))))
			    
    (invoke w 'addMouseListener listener)
    (invoke w 'addMouseMotionListener listener))
  (set-sprite-manager-buffer! manager (invoke w 'createImage 300 300))
  manager)
			 
(define (add-sprite manager sprite)
  (set-sprite-manager-sprites! manager (cons sprite (sprite-manager-sprites manager)))
;  (manager-repaint manager)
  )

(define (remove-sprite manager sprite)
  (set-sprite-manager-sprites! manager (delete sprite (sprite-manager-sprites manager)))
;  (manager-repaint manager)
  )

(define (manager-clock man)
  (let ((*manager* man))
    (for-each clock (sprite-manager-sprites man))
    (manager-repaint man)))

(define (manager-repaint man)
  (synchronized man			;make sure only one thread at a time does this.
    (let* ((sprites (sprite-manager-sprites man))
	   (buffer (sprite-manager-buffer man))
	   (window (sprite-manager-window man))
	   ;; Kludge class is necessary workaround to an access problem
	   (graphics (invoke-static 'com.ibm.jikes.skij.misc.Kludge 'imageGraphics buffer)))
      ; clear the offscreen buffer
      (invoke graphics 'setColor (invoke window 'getBackground))
      (invoke graphics 'fillRect 0 0 300 300)
      ; paint everything into offscreen buffer
      (for-each (lambda (sprite) (paint sprite graphics)) sprites)
      ; copy onto window
      (invoke (invoke window 'getGraphics) 'drawImage buffer 0 0 window)
      )))

;;; bring back the sprites
(define (manager-retrieve man)
  (for-each (lambda (sprite)
	      (set-sprite-x! sprite (arand 150 100))
	      (set-sprite-y! sprite (arand 150 100)))
	    (sprite-manager-sprites man))
  (manager-repaint man))

;;; permanently animate the sprites
(define (manager-animate man)
  (in-own-thread (let loop () (manager-clock man) (loop))))

(define (make-rect-window)
  (define manager (make-sprite-window "Sprites"))
  (repeat 10 (add-sprite manager (make-random-square)))
  manager)

; (make-rect-window)

;;; Cannonballs

(define (make-cannonball x y)
  (define xv (arand 0 10))
  (define yv (arand -15 7))
  (define color (random-color))
  (make-sprite x y 25 25
	       (lambda (sprite g)
		 (invoke g 'setColor color)
		 (invoke g 'fillOval (integer (sprite-x sprite)) (integer (sprite-y sprite)) (integer (sprite-xs sprite)) (integer (sprite-ys sprite))))
	       (lambda (sprite)
		 (move-sprite sprite xv yv)
		 (if (> (sprite-y sprite) 300)
		     (remove-sprite (dynamic *manager*) sprite)
		     (set! yv (+ yv 1))))))

(define (make-cannonade freq n)
  (define man (make-sprite-window "Cannonade"))
  (manager-animate man)
  (in-own-thread
   (let loop ()
     (repeat n (add-sprite man (make-cannonball 100 260)))
     (sleep freq)
     (loop))))


; (make-cannonade 1000 4)


;;; John Maeda imitations

(define (make-text-sprite text follow)
  (define color (random-color))
  (make-sprite (random 200) (random 200) 10 10
	       (lambda (sprite g)
		 (invoke g 'setColor color)
		 (invoke g 'drawString text (integer (sprite-x sprite)) (integer (sprite-y sprite)))
		 )
	       (lambda (sprite)
		 (if follow
		     (move-towards sprite follow (random 10))
		     (begin (move-sprite sprite (arand 0 10) (arand 0 10)))))))


(define (move-towards mover target displacement)
  (let* ((dist (distance mover target))
	 (scale (/ displacement dist)))
    (unless (zero? dist)
	    (move-sprite mover 
			 (* scale (- (sprite-x target) (sprite-x mover)))
			 (* scale (- (sprite-y target) (sprite-y mover)))))))


(define (square x) (* x x))

(define (distance s1 s2)
  (sqrt (+ (square (- (sprite-x s1) (sprite-x s2)))
	   (square (- (sprite-y s1) (sprite-y s2))))))

(define (make-alphabet)
  (define manager (make-sprite-window "AlphaDance"))
  (invoke (sprite-manager-window manager) 'setBackground (new 'java.awt.Color 30 30 30))
  (manager-animate manager)
  (do ((i (char->int #\a) (+ 1 i))
       (sprite #f))
      ((> i (char->int #\z)))
    (set! sprite (make-text-sprite (string (int->char i)) sprite))
    (add-sprite manager sprite))
  manager)