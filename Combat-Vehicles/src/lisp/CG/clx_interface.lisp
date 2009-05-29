;;; -------------------------------------------------------------------
;;; -------------------------------------------------------------------
;;; Common Graphics: Graphics the right way!
;;;
;;; Copyright (c) R. James Firby 1989, 1991
;;; -------------------------------------------------------------------
;;; -------------------------------------------------------------------

(in-package "CG")

;(use-package "XLIB")

;(shadow '(draw-point draw-rectangle event-handler) (find-package "CG"))

(defvar x-default-display nil)

(defstruct clx-window
  display
  gcontext gcontext-clear
  window
  user)

(defvar current-display    nil)
(defvar current-gcontext   nil)
(defvar current-window     nil)
(defvar current-x-offset   nil)
(defvar current-y-offset   nil)
(defvar current-foreground nil)
(defvar current-background nil)
(defvar current-x0         nil)
(defvar current-y0         nil)
(defvar current-scale      nil)

(defun i.set-cg-current-view (view)
  (let ( (window (cg-view-window view))
         (x (cg-view-x view)) (y (cg-view-y view))
         (w (cg-view-w view)) (h (cg-view-h view)) )
    (let ( (x-window (cg-window-real window)) )
      (setf current-window   (clx-window-window   x-window))
      (setf current-gcontext (clx-window-gcontext x-window))
      (setf current-display  (clx-window-display  x-window)))
    (setf current-foreground (cg-window-foreground window))
    (setf current-background (cg-window-background window))
    (i.set-cg-current-mode :NORMAL)
    (let ( (size-x (xlib:drawable-width  current-window))
           (size-y (xlib:drawable-height current-window))
           (scale  (cg-window-scale window)) )
      (setf current-x-offset 0)
      (setf current-y-offset size-y)
      (cond ((or (null w) (null h))
             (setf (xlib:gcontext-clip-mask current-gcontext) :none))
            (T
             (i.set-cg-current-transform (* x scale) (* y scale) scale)
             (multiple-value-bind (x1 y1)
                                  (->current-coords 0 0)
               (multiple-value-bind (x2 y2)
                                    (->current-coords w h)
                 (setf (xlib:gcontext-clip-mask current-gcontext)
                       (list (list (max 0 (min size-x (min x1 x2)))
                                   (max 0 (min size-y (min y1 y2)))
                                   (max 0 (min size-x (max x1 x2)))
                                   (max 0 (min size-y (max y1 y2))))))))))))
    (values))

(defun i.set-cg-current-transform (x0 y0 scale)
  (setf current-x0    x0)
  (setf current-y0    y0)
  (setf current-scale scale)
  (values))

(defun ->current-coords (x y)
  (let ( (x (+ current-x-offset
               (truncate (+ current-x0 (* current-scale x)))))
         (y (- current-y-offset
               (truncate (+ current-y0 (* current-scale y))))) )
    (values (cond ((< x -32767) -32767)
                  ((> x  32767)  32767)
                  (T             x))
            (cond ((< y -32767) -32767)
                  ((> y  32767)  32767)
                  (T             y)))))

(defun i.erase-window (window x1 y1 x2 y2)
  (let ( (clx-window (cg-window-real window)) )
    (let ( (x-window (clx-window-window clx-window))
           (gcontext (clx-window-gcontext-clear clx-window)) )
      (let ( (width  (xlib:drawable-width  x-window))
             (height (xlib:drawable-height x-window)) )
        (setf (xlib:gcontext-foreground gcontext) (cg-window-background window))
        (xlib:draw-rectangle x-window gcontext
                             (max 0 (- (truncate x1) 1))
                             (max 0 (- (- height (truncate y2)) 1))
                             (min (+ (truncate (- x2 x1)) 2) width)
                             (min (+ (truncate (- y2 y1)) 2) height)
                             T)))))

;;; ----------------------------------------------------------------------
;;; Setting Global Drawing State
;;; ----------------------------

(defun i.set-cg-current-mode (mode)
  (cond ((eq mode :invert)
         (setf (xlib:gcontext-function current-gcontext) boole-xor))
        (T
         (setf (xlib:gcontext-function current-gcontext) boole-1))))

(defun set-current-texture (texture)
  (declare (ignore texture))
  (values))

(defun set-current-color (color)
  (let ( (primary (cg-color-primary color)) )
    (cond ((eq primary :foreground)
           (setf (xlib:gcontext-foreground current-gcontext) current-foreground))
          ((eq primary :background)
           (setf (xlib:gcontext-foreground current-gcontext) current-background))
          (T
           (let ( (pixel (cdr (assoc current-display 
                                     (cg-color-internal color)))) )
             (cond ((null pixel)
                    (let ( (pixel 
                            (xlib:alloc-color 
                             (xlib:screen-default-colormap
                              (car (xlib:display-roots current-display)))
                             (xlib:make-color :red   (cg-color-red color)
					      :green (cg-color-green color)
					      :blue  (cg-color-blue  color)))) )
                      (setf (cg-color-internal color)
                            (cons (cons current-display pixel)
                                  (cg-color-internal color)))
                      (setf (xlib:gcontext-foreground current-gcontext)
                            pixel)))
                   (T
                    (setf (xlib:gcontext-foreground current-gcontext)
                          pixel))))))))

;;; --- Fonts -----------------------------------------------------------

(defvar x-default-font-family "helvetica")

(defun font->x-font-name (font display size)
  (let ( (family  (cg-font-family font))
         (attribs (cg-font-options font)) ) 
    (let ( (bold?   (member :bold attribs))
           (italic? (member :italic attribs)) )
      (let ( (attrib-string (cond ((and bold? italic?) "*bold*italic")
                                   (bold?              "*bold")
                                   (italic?            "*italic")
                                   (T                  "*"))) )
        (if (xlib:list-font-names display family)
            family
            (or (nearest-x-font-name display family attrib-string attribs size)
                (nearest-x-font-name display family "" attribs size)
                (nearest-x-font-name display x-default-font-family "" 
                                     attribs size)
                "5x8"))))))

(defun nearest-x-font-name (display family attrib-string attribs size)
  (let ( (matches 
           (or (xlib:list-fonts display 
                           (format nil "*~a~a*~d*" family attrib-string size))
               (xlib:list-fonts display
                             (format nil "*~a~a*" family attrib-string size)))) )
    (if (null matches) 
        nil
        (xlib:font-name
          (extract-closest-font (cdr matches) size attribs
                                (car matches) 
                                (xlib:font-ascent (car matches)))))))

(defun extract-closest-font (fonts size attribs best best-size)
  (if (null fonts)
      best
      (let ( (this-size (xlib:font-ascent (car fonts))) )
         (cond ((= best-size this-size)
                (if (< (font-name-difference (xlib:font-name (car fonts)) attribs)
                       (font-name-difference (xlib:font-name best) attribs))
                    (extract-closest-font (cdr fonts) size attribs
                                          (car fonts) this-size)
                    (extract-closest-font (cdr fonts) size attribs
                                          best best-size)))
               ((and (< this-size size) (< size best-size))
                (extract-closest-font (cdr fonts) size attribs
                                      (car fonts) this-size))
               ((and (<= size this-size) (< this-size best-size))
                (extract-closest-font (cdr fonts) size attribs
                                      (car fonts) this-size))
               ((and (< best-size this-size) (<= this-size size))
                (extract-closest-font (cdr fonts) size attribs
                                      (car fonts) this-size))
               (T
                (extract-closest-font (cdr fonts) size attribs
                                      best best-size))))))

(defun font-name-difference (font-name attribs)
  (+ (if (and (not (member :bold attribs))  (search "bold" font-name))   1 0)
     (if (and (not (member :italic attribs)) (search "italic" font-name)) 1 0)
     (if (search "narrow" font-name) 1 0)))

(defun get-internal-font (font display size)
  (let ( (internal-fonts (cg-font-internal font)) )
    (let ( (font-list (assoc display internal-fonts)) )
      (if (null font-list)
          nil
          (cdr (assoc size (cdr font-list)))))))

(defun add-internal-font (font display size x-font)
  (let ( (internal-fonts (cg-font-internal font)) )
    (let ( (font-list (assoc display internal-fonts)) )
      (if (null font-list)
          (setf (cg-font-internal font)
                (cons (list display (cons size x-font))
                      (cg-font-internal font)))
          (let ( (duplicate (assoc size font-list)) )
            (if (null duplicate)
                (setf (cdr font-list)
                      (cons (cons size x-font)
                            (cdr font-list))))))))
  (values))
            
(defun font->real-x-font (font display scale)
  (let* ( (size (truncate (* (cg-font-size font) scale)))
          (size (if (< size 1) 1 size)) )
    (let ( (stored-x-font (get-internal-font font display size)) )
       (if stored-x-font
           stored-x-font
           (let ( (font-name 
                    (font->x-font-name font display size)) )
              (let ( (x-font (xlib:open-font display font-name)) )
                 (add-internal-font font display size x-font)
                 x-font))))))

(defun set-current-font (font)
  (let ( (real-x-font (font->real-x-font font current-display current-scale)) )
     (setf (xlib:gcontext-font current-gcontext) real-x-font)))

; --------------------------------------------------------------------
; Primitives Drawing Calls
; ------------------------

(defun draw-point (x y color)
  (multiple-value-bind (dot-x dot-y)
                       (->current-coords x y)
    (set-current-texture texture/100%)
    (set-current-color   color)
    (xlib:draw-point current-window current-gcontext dot-x dot-y))
  (values))

(defun draw-segment (x1 y1 x2 y2 color)
  (set-current-texture texture/100%)
  (set-current-color   color)
  (multiple-value-bind (x1 y1)
                       (->current-coords x1 y1)
    (multiple-value-bind (x2 y2)
                         (->current-coords x2 y2)
      (xlib:draw-line current-window current-gcontext
                      x1 y1 x2 y2)))
  (values))

(defun draw-rectangle (x1 y1 x2 y2 filled? color texture)
  (set-current-texture (if filled? texture texture/100%))
  (set-current-color   color)
  (multiple-value-bind (x1 y1)
                       (->current-coords x1 y1)
    (multiple-value-bind (x2 y2)
                         (->current-coords x2 y2)
      (xlib:draw-rectangle current-window current-gcontext
                           (min x1 x2) (min y1 y2)
                           (abs (- x2 x1)) (abs (- y2 y1))
                           filled?)))
  (values))

(defun draw-oval (x1 y1 x2 y2 filled? color texture)
  (set-current-texture (if filled? texture texture/100%))
  (set-current-color   color)
  (multiple-value-bind (x1 y1)
                       (->current-coords x1 y1)
    (multiple-value-bind (x2 y2)
                         (->current-coords x2 y2)
      (xlib:draw-arc current-window current-gcontext
                     (min x1 x2) (min y1 y2)
                     (abs (- x2 x1)) (abs (- y2 y1))
                     0 (* pi 2) filled?)))
  (values))

(defun draw-polygon (xs ys len filled? color texture)
  (set-current-texture (if filled? texture texture/100%))
  (set-current-color   color)
  (let ( (points (polygon-point-list xs ys len)) )
    (xlib:draw-lines current-window current-gcontext
                     points :fill-p filled?))
  (values))

(defun polygon-point-list (xs ys len)
  (multiple-value-bind (x0 y0)
                       (->current-coords (aref xs 0) (aref ys 0))
    (let ( (point-list (list x0 y0)) )
      (do ( (i (- len 1) (- i 1)) )
          ((< i 0) point-list)
        (multiple-value-bind (x y)
                             (->current-coords (aref xs i) (aref ys i))
          (setf point-list (cons x (cons y point-list))))))))

(defun draw-text (x y text font color)
  (set-current-texture texture/100%)
  (set-current-color   color)
  (set-current-font    font)
  (multiple-value-bind (x y)
                       (->current-coords x y)
    (xlib:draw-glyphs current-window current-gcontext
                      x y text))
  (values))

;;; ---------------------------------------------------------------
;;; Font Information
;;; ----------------

(defun open-x-default-display! ()
  (setq x-default-display (xlib:open-display "")))

(defun i.text-extent (text font)
  (let ( (display (if x-default-display
                      x-default-display
                      (open-x-default-display!))) )
     (let ( (real-x-font (font->real-x-font font display 1)) )
        (multiple-value-bind (width ascent descent left)
                             (xlib:text-extents real-x-font text)
           (values left (- descent) (+ width left) ascent)))))

;;; ---------------------------------------------------------------
;;; Opening a Window
;;; ----------------

(defparameter x-default-position '(100 100))
(defparameter x-default-size     '(500 400))

(defun i.window-size (clx-window)
  (let ( (window (clx-window-window clx-window)) )
    (values (xlib:drawable-width window)
            (xlib:drawable-height window))))

(defun i.window-colors (clx-window)
  (let ( (gcontext (clx-window-gcontext clx-window)) )
     (values (xlib:gcontext-foreground gcontext)
             (xlib:gcontext-background gcontext))))

(defun i.window-closed? (clx-window)
  (declare (ignore clx-window))
  nil)

(defmethod window-close ((clx-win clx-window))
  (when (not (null (clx-window-window clx-win)))
    (xlib:destroy-window (clx-window-window clx-win))
    (xlib:display-force-output (clx-window-display clx-win)))
  (setf (clx-window-window   clx-win) nil)
  (setf (clx-window-user     clx-win) nil)
  (setf (clx-window-gcontext clx-win) nil)
  (setf (clx-window-gcontext-clear clx-win) nil)
  (values))

(defun i.window-open (user x y w h title kind colorp positions? container)
  (declare (ignore colorp))
  (let ( (display (xlib:open-display (if (null kind) "" kind))) )
     (let ( (screen (xlib:display-default-screen display))
            (mask   (if positions?
                      (xlib:make-event-mask :exposure :structure-notify
					    :key-press
					    :button-press :button-release
					    :button-motion)
		      (xlib:make-event-mask :exposure :structure-notify
					    :key-press
					    :button-press :button-release)))
            (x      (if x x (car  x-default-position)))
            (y      (if y y (cadr x-default-position)))
            (w      (if w w (car  x-default-size)))
            (h      (if h h (cadr x-default-size))) )
        (let ( (window (xlib:create-window :parent (if container
						     container
						     (xlib:screen-root screen))
					   :x x :y y :width w :height h
					   :backing-store :always
					   :bit-gravity   :north-west
					   :event-mask    mask)) )
           (xlib:change-property window :wm_name title :string 16)
           (let ( (gcontext  (xlib:create-gcontext :drawable window))
                  (gcontext2 (xlib:create-gcontext :drawable window)) )
              (when (> (xlib:screen-root-depth screen) 1)
                (setf (xlib:gcontext-foreground gcontext) (xlib:screen-black-pixel screen))
                (setf (xlib:gcontext-background gcontext) (xlib:screen-white-pixel screen))
                (setf (xlib:gcontext-background gcontext2) (xlib:screen-white-pixel screen)))
              (let ( (clx-win (make-clx-window :display display
                                               :window window
                                               :gcontext gcontext
                                               :gcontext-clear gcontext2
                                               :user user)) )
                (xlib:map-window window)
                (start-x-event-handler clx-win)
                clx-win))))))

; ------------------------------------------------------------------------------
; Asynchronous Window Update Locks
; --------------------------------

(defvar current-locked-window nil)
(defvar functions-waiting-for-lock '())

(defun maybe-lock-window? (window)
  (mp:without-scheduling
    (cond ((not (null current-locked-window))
           NIL)
          (T
           (setf current-locked-window window)
           (setf (cg-window-lock window) T)
           T))))

(defun unlock-window (window)
  (cond ((not (eq current-locked-window window))
         (error "Window ~s putting lock but window ~s is holding lock! Serious bogosity."
                window current-locked-window)
         (values))
        (T
         (xlib:display-force-output (clx-window-display (cg-window-real window)))
         (let ( (fun nil) )
           (mp:without-scheduling
            (let ( (waiting (pop functions-waiting-for-lock)) )
              (cond (waiting
                     (setf current-locked-window (car waiting))
                     (setf (cg-window-lock (car waiting)) NIL)
                     (setf fun (cdr waiting)))
                    (T
                     (setf current-locked-window nil)
                     (setf (cg-window-lock window) NIL)))))
           (if (functionp fun) (funcall fun))))))

(defun queue-for-window-lock (window fun)
  (let ( (execute? nil) )
    (mp:without-scheduling
     (cond ((not (null current-locked-window))
            (setf functions-waiting-for-lock
                  (nconc functions-waiting-for-lock (list (cons window fun)))))
           (T
            (setf current-locked-window window)
            (setf (cg-window-lock window) T)
            (setf execute? T))))
    (if execute? (funcall fun))))

(defmacro with-window-lock (window &body body)
  (let ( (win (gentemp)) )
    `(let ( (,win ,window) )
       (when (not (window? ,win))
         (error "Cannot lock non-window: ~s" ,win))
       (queue-for-window-lock ,win 
                              #'(lambda () 
                                  (unwind-protect 
                                    (progn ,@body)
                                    (unlock-window ,win)))))))

; ------------------------------------------------------------------------------
; Special Efficiency Hacks
; ------------------------

(defun draw-view-image (view)
  (let ( (clx-window (cg-window-real (cg-view-window view))) )
    (i.set-cg-current-view view)
    (uncache-view-states view)
    (let ( (x0 (cg-view-x view)) (y0 (cg-view-y view))
           (scale (cg-window-scale (cg-view-window view))) )
      (draw (cg-view-picture view) view 
            (* x0 scale) (* y0 scale) scale
            T))
    (xlib:display-force-output (clx-window-display clx-window))
    (values)))

(defun build-view-image! (view)
  (declare (ignore view))
  (values))

(defun close-view-image! (view)
  (declare (ignore view))
  (values))

; ------------------------------------------------------------------------------
; Event Handling
; --------------

(defconstant button-1         (code-char 129))
(defconstant button-1-up      (code-char 130))
(defconstant button-no-up     (code-char 133))
(defconstant pointer-drag     (code-char 131))
(defconstant pointer-position (code-char 132))
(defconstant pointer-enter    (code-char 134))
(defconstant pointer-exit     (code-char 135))

(defvar last-mouse-position 0)
(defvar last-mouse-state    'up)

;;; --------------------------------------------------------------
;;; Handle X events
;;; ---------------

(defun handle-x-events (clx-window)
 (handler-case 
  (loop
    (if (null (clx-window-window clx-window)) (return))
    (xlib:event-case ((clx-window-display clx-window) :discard-p T)
      (:exposure (x y width height count)
         (let ( (gcontext (clx-window-gcontext-clear clx-window)) )
           (setf (xlib:gcontext-foreground gcontext) 
                 (xlib:gcontext-background gcontext))
           (xlib:draw-rectangle (clx-window-window clx-window)
                                gcontext x y width height T)
           (when (= count 0)
             (handle-window-redraw clx-window))
           T))
      (:destroy-notify ()
         (setf (clx-window-window clx-window) nil)
         (window-close (clx-window-user clx-window))
         T)
      (:key-press (code x y)
         (generate-cg-event (clx-window-user clx-window) code x y))
      (:button-press (code x y)
         (generate-cg-event (clx-window-user clx-window)
                            (case code ((2)       button-1) 
                                       ((3)       button-1)
                                       (otherwise button-1))
                            x y))
      (:button-release (code x y)
         (generate-cg-event (clx-window-user clx-window)
                            (case code ((2)       button-1-up) 
                                       ((3)       button-1-up)
                                       (otherwise button-1-up))
                            x y))
      (:motion-notify (x y)
         (generate-cg-event (clx-window-user clx-window) pointer-drag x y))
      (otherwise () T)))
  (xlib:closed-display ()
    (setf (clx-window-window clx-window) nil)
    (window-close (clx-window-user clx-window))
    (values))))

(defun handle-window-redraw (clx-window)
  (let ( (window (clx-window-user clx-window)) )
    (multiple-value-bind (w h)
                         (i.window-size clx-window)
      (cond ((or (cg-window-lock window)
                 (null (cg-window-real window)))
             (values))
            ((not (and (= w (cg-window-w window))
                       (= h (cg-window-h window))))
             (setf (cg-window-w window) w)
             (setf (cg-window-h window) h)
             (setf (cg-window-scale window)
                   (min (/ w (cg-window-init-w window))
                        (/ h (cg-window-init-h window))))
             (dolist (view (cg-window-views window))
               (signal-a-change view 'ERASE (cg-view-picture view))))
            ((maybe-lock-window? window)
             (unwind-protect
               (window-refresh window)
               (unlock-window window)
               (update-active-windows)))
            (T
             (setf (cg-window-deferred window) T))))))

;;; -----------------------------------------------------------------
;;; Starting the Event Handling Process
;;; ------------------------------------

(defun generate-cg-event (window event x y)
  (multiple-value-bind (w h)
                       (i.window-size (cg-window-real window))
    (declare (ignore w))
;   (event-dispatcher window event x (- h y)))
   (mp:process-run-function '(:name "CG Event Handler" :priority 2)
	                     #'event-dispatcher window event x (- h y)))
  T)
                           
(defun start-x-event-handler (clx-window)
  (mp:process-run-function (list :name (cg-window-name 
                                         (clx-window-user clx-window))
                                 :priority 1)
                           #'handle-x-events clx-window)
  (values))

