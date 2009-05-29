;;; -------------------------------------------------------------------
;;; -------------------------------------------------------------------
;;; Common Graphics: Graphics the right way!
;;;
;;; Copyright (c) R. James Firby 1989, 1991
;;; -------------------------------------------------------------------
;;; -------------------------------------------------------------------

(in-package "CG")

(require :traps)
;(require :records)
(require :quickdraw)

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
    (setf current-window     (cg-window-real       window))
    (setf current-foreground (cg-window-foreground window))
    (setf current-background (cg-window-background window))
    (i.set-cg-current-mode :NORMAL)
    (let ( (size  (view-size current-window))
           (scale (cg-window-scale window)) )
      (setf current-x-offset 0)
      (setf current-y-offset (point-v size))
      (let ( (r (new-region)) )
        (cond ((or (null w) (null h))
               (set-rect-region r 0 0 (point-h size) (point-v size))
               (set-clip-region current-window r))
              (T
               (i.set-cg-current-transform (* x scale) (* y scale) scale)
               (multiple-value-bind (x1 y1)
                                    (->current-coords 0 0)
                 (multiple-value-bind (x2 y2)
                                      (->current-coords w h)
                   (set-rect-region r
                                    (max 0 (min (point-h size) (min x1 x2)))
                                    (max 0 (min (point-v size) (min y1 y2)))
                                    (max 0 (min (point-h size) (max x1 x2)))
                                    (max 0 (min (point-v size) (max y1 y2))))
                   (set-clip-region current-window r)))))
        (dispose-region r)))
    (values)))

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
  (let ( (mac-window (cg-window-real window)) )
    (let ( (size (view-size mac-window)) )
      (erase-rect mac-window
                  (max 0 (truncate x1))
                  (max 0 (- (point-v size) (truncate y2)))
                  (min (truncate x2) (point-h size))
                  (min (- (point-v size) (truncate y1)) (point-v size))))))

;;; ----------------------------------------------------------------------
;;; Setting Global Drawing State
;;; ----------------------------

(defun i.set-cg-current-mode (mode)
  (cond ((eq mode :invert)
         (set-pen-mode current-window :patXor))
        (T
         (set-pen-mode current-window :patCopy))))

(defun set-current-texture (texture)
  (if (null (cg-texture-internal texture))
      (setf (cg-texture-internal texture)
            (let ( (name (cg-texture-name texture)) )
              (cond ((eq name :stipple-0)  *white-pattern*)
                    ((eq name :stipple-25) *light-gray-pattern*)
                    ((eq name :stipple-50) *gray-pattern*)
                    ((eq name :stipple-75) *dark-gray-pattern*)
                    (T                     *black-pattern*)))))
  (set-pen-pattern current-window (cg-texture-internal texture)))

(defconstant mac-primary-colors 
  '((:white 30) (:black 33) 
    (:red 205)  (:green 341)   (:blue 409)
    (:cyan 273) (:magenta 137) (:yellow 69)))

(defun set-current-color (color)
  (let ( (primary (cg-color-primary color)) )
    (cond ((eq primary :foreground)
           (if (numberp current-foreground)
               (set-fore-color current-window current-foreground)
               (let ( (actual (cadr (assoc current-foreground mac-primary-colors))) )
                 (if (null actual)
                     (error "Eeep! Internal color bogosity!")
                     (with-port (wptr current-window)
                       (_ForeColor :long actual :novalue))))))
          ((eq primary :background)
           (if (numberp current-background)
               (set-fore-color current-window current-background)
               (let ( (actual (cadr (assoc current-background mac-primary-colors))) )
                 (if (null actual)
                     (error "Eeep! Internal color bogosity!")
                     (with-port (wptr current-window)
                       (_ForeColor :long actual :novalue))))))
          ((null primary)
           (if (null (cg-color-internal color))
               (setf (cg-color-internal color)
                     (make-color (truncate (* (cg-color-red   color) 65535))
                                 (truncate (* (cg-color-green color) 65535))
                                 (truncate (* (cg-color-blue  color) 65535)))))
           (set-fore-color current-window (cg-color-internal color)))
          (T
           (let ( (actual (cadr (assoc primary mac-primary-colors))) )
             (if (null actual)
                 (error "Eeep! Internal color bogosity!")
                 (with-port (wptr current-window)
                   (_ForeColor :long actual :novalue))))))))
            
(defun set-current-font (font)
  (if (null (cg-font-internal font))
      (setf (cg-font-internal font)
            (cons (cg-font-family font)
                  (cons (cg-font-size font)
                        (if (null (cg-font-options font))
                          '(:plain)
                          (cg-font-options font))))))
  (let* ( (font-size (truncate (* (cg-font-size font) current-scale)))
          (font-size (cond ((< font-size 1)   1)
                           (T                 font-size))) )
    (setf (cadr (cg-font-internal font)) font-size)
    (set-view-font current-window (cg-font-internal font))))

; --------------------------------------------------------------------
; Primitives Drawing Calls
; ------------------------

(defun draw-point (x y color)
  (multiple-value-bind (dot-x dot-y)
                       (->current-coords x y)
    (set-current-texture texture/100%)
    (set-current-color   color)
    (move-to current-window dot-x dot-y)
    (line-to current-window dot-x dot-y))
  (values))

(defun draw-segment (x1 y1 x2 y2 color)
  (set-current-texture texture/100%)
  (set-current-color   color)
  (multiple-value-bind (x1 y1)
                       (->current-coords x1 y1)
    (move-to current-window x1 y1))
  (multiple-value-bind (x2 y2)
                       (->current-coords x2 y2)
    (line-to current-window x2 y2))
  (values))

(defun draw-rectangle (x1 y1 x2 y2 filled? color texture)
  (multiple-value-bind (x1 y1)
                       (->current-coords x1 y1)
    (multiple-value-bind (x2 y2)
                         (->current-coords x2 y2)
      (cond (filled?
             (set-current-texture texture)
             (set-current-color   color)
             (paint-rect current-window (min x1 x2) (min y1 y2) 
                                         (max x1 x2) (max y1 y2)))
            (T
             (set-current-texture texture/100%)
             (set-current-color   color)
             (frame-rect current-window (min x1 x2) (min y1 y2) 
                                        (max x1 x2) (max y1 y2))))))
  (values))

(defun draw-oval (x1 y1 x2 y2 filled? color texture)
  (multiple-value-bind (x1 y1)
                       (->current-coords x1 y1)
    (multiple-value-bind (x2 y2)
                         (->current-coords x2 y2)
      
      (cond (filled?
             (set-current-texture texture)
             (set-current-color   color)
             (paint-oval current-window (min x1 x2) (min y1 y2)
                                        (max x1 x2) (max y1 y2)))
            (T
             (set-current-texture texture/100%)
             (set-current-color   color)
             (frame-oval current-window (min x1 x2) (min y1 y2) 
                                        (max x1 x2) (max y1 y2))))))
  (values))

(defun draw-polygon (xs ys len filled? color texture)
  (cond (filled?
         (set-current-texture texture)
         (set-current-color   color)
         (let ( (poly nil) )
           (multiple-value-bind (xstart ystart)
                                (->current-coords (aref xs 0) (aref ys 0))
             (move-to current-window xstart ystart)
             (start-polygon current-window)
             (unwind-protect
               (do ( (i 1 (+ i 1)) )
                   ((>= i len) (line-to current-window xstart ystart)
                               (values))
                 (multiple-value-bind (nx ny)
                                      (->current-coords (aref xs i) (aref ys i))
                   (line-to current-window nx ny)))
               (setf poly (get-polygon current-window)))
             (paint-polygon current-window poly)
             (kill-polygon poly))))
           (T
            (set-current-texture texture/100%)
            (set-current-color   color)
            (multiple-value-bind (xstart ystart)
                                 (->current-coords (aref xs 0) (aref ys 0))
              (move-to current-window xstart ystart)
              (do ( (i 1 (+ i 1)) )
                  ((>= i len) (line-to current-window xstart ystart)
                              (values))
                (multiple-value-bind (nx ny)
                                     (->current-coords (aref xs i) (aref ys i))
                  (line-to current-window nx ny))))))
  (values))

(defun draw-text (x y text font color)
  (set-current-texture texture/100%)
  (set-current-color   color)
  (multiple-value-bind (x y)
                       (->current-coords x y)
    (move-to current-window x y))
  (set-current-font font)
  (write-string text current-window)
  (values))

;;; ---------------------------------------------------------------
;;; Font Information
;;; ----------------

(defun i.text-extent (text font)
  (if (null (cg-font-internal font))
      (setf (cg-font-internal font)
            (cons (cg-font-family font)
                  (cons (cg-font-size font)
                        (cg-font-options font)))))
  (setf (cadr (cg-font-internal font)) (cg-font-size font))
  (multiple-value-bind (ascent descent widmax leading)
                       (font-info (cg-font-internal font))
    (declare (ignore leading widmax))
    (let ( (width (string-width text (cg-font-internal font))) )
      (values 0 (- descent) width ascent))))

;;; ---------------------------------------------------------------
;;; Opening a Window
;;; ----------------

(defun i.window-size (mac-view)
  (let ( (size (view-size mac-view)) )
    (values (point-h size) (point-v size))))

(defun i.window-colors (mac-view)
  (let ( (bw-fore (rref (wptr mac-view) grafport.fgcolor))
         (bw-back (rref (wptr mac-view) grafport.bkcolor)) )
    (if (not (= bw-back 0))
        (values bw-fore bw-back)
        (let ( (rgb-fore 
                (rgb-to-color (rref (wptr mac-view) cgrafport.rgbFgColor)))
               (rgb-back
                (rgb-to-color (rref (wptr mac-view) cgrafport.rgbBkColor))) )
          (if (and (= 0 bw-back)
                   (= 1 bw-fore))
              (if (= rgb-fore 0)
                  (values :black :white)
                  (values :white :black))
              (values rgb-fore rgb-back))))))

(defun i.window-closed? (mac-view)
  (null (wptr mac-view)))

(defclass cg-mac-device
  (view)
  ((user       :accessor cg-mac-device-user       :initarg :user)
   (positions? :accessor cg-mac-device-positions? :initarg :positions?)))

(defmethod view-draw-contents ((self cg-mac-device))
  (let ( (window (cg-mac-device-user self)) )
    (multiple-value-bind (w h)
                         (i.window-size self)
      (cond ((or (cg-window-lock window)
                 (null (cg-window-real window)))
             (setf (cg-window-deferred window) T) ; Possible bug fix: partial redraw
             (values))
            ((not (and (= w (cg-window-w window))
                       (= h (cg-window-h window))))
             (setf (cg-window-w window) w)
             (setf (cg-window-h window) h)
             (setf (cg-window-scale window)
                   (min (/ w (cg-window-init-w window))
                        (/ h (cg-window-init-h window))))
             (invalidate-view self T)
             (dolist (view (cg-window-views window))
               (signal-a-change view 'ERASE (cg-view-picture view))))
            ((maybe-lock-window? window)
             (unwind-protect
               (window-refresh window)
               (window-draw-grow-icon self)
               (unlock-window window)
               (update-active-windows)))
            (T
             (setf (cg-window-deferred window) T))))))

(defclass cg-mac-view
  (cg-mac-device)
  ())

(defmethod window-close ((self cg-mac-view))
  (values))

(defclass cg-mac-window
  (cg-mac-device window)
  ())

(defmethod window-close ((self cg-mac-window))
  (let ( (window (cg-mac-device-user self)) )
    (window-close window)
    (if (and (not (window-open? window))
             (not (null (wptr self))))
        (call-next-method))))

(defmethod window-save-as ((self cg-mac-window))
  (let ( (window (cg-mac-device-user self)) )
    (cond ((maybe-lock-window? window)
           (unwind-protect
             (copy-window-to-file window)
             (unlock-window window))))
    (values)))

(defun i.window-open (user x y w h title kind colorp positions? container)
  (if (null container)
      (let* ( (active-window (front-window)) ; Save active window
              (window 
               (make-instance 'cg-mac-window
                              :view-position (if (and x y)
                                                 (make-point (truncate x)
                                                             (truncate y)) 
                                                 *window-default-position*)
                              :view-size     (if (and w h)
                                                 (make-point (truncate w)
                                                             (truncate h)) 
                                                 *window-default-size*)
                              :window-title  title
                              :color-p       colorp
                              :window-type   (cond ((eq kind :document)
                                                    :document-with-grow)
                                                   (T
                                                    :tool))
                              :user          user
                              :positions?    positions?)) )
        (window-ensure-on-screen window)
        (window-select active-window)       ; Restore active window
        window)
      (let ( (view
              (make-instance 'cg-mac-view
                             :view-position (if (and x y)
                                                (make-point (truncate x)
                                                            (truncate y)) 
                                                *window-default-position*)
                             :view-size     (if (and w h)
                                                (make-point (truncate w)
                                                            (truncate h)) 
                                                *window-default-size*)
                             :user          user
                             :positions?    positions?)) )
        (set-view-container view container)
        view)))
                            
(defun copy-window-to-file (window &optional filename)
  (if (not (null (cg-window-change window)))
      (window-draw window))
  (let ( (filename (or filename (choose-new-file-dialog :prompt "Save as PICT File ..."))) )
    (with-open-file (file filename :direction         :output
                                   :element-type      'unsigned-byte
                                   :if-exists         :overwrite
                                   :if-does-not-exist :create)
      (cond ((null file)
             NIL)
            (T
             (set-mac-file-type file :PICT)
             (let ( (mac-window (cg-window-real window)) )
               (let ( (new-image (progn
                                   (start-picture mac-window)
                                   (window-refresh window)
                                   (get-picture mac-window))) )
                 (if (not (handlep new-image))
                     NIL
                      (unwind-protect
                        (let ( (pic-size (rref new-image picture.picsize)) )
                          (do ( (i 0 (+ i 1)) )
                              ((>= i 512) nil)
                            (write-byte 0 file))  
                          (with-dereferenced-handles ( (pic new-image) )
                            (do ( (i 0 (+ i 1)) )
                                ((>= i pic-size) (values))
                              (write-byte (%get-byte pic i) file)))
                          (close file)
                          'T)
                        (kill-picture new-image))))))))))

; ------------------------------------------------------------------------------
; Asynchronous Window Update Locks
; --------------------------------

(defvar current-locked-window nil)
(defvar functions-waiting-for-lock '())

(defun maybe-lock-window? (window)
  (without-interrupts
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
         (let ( (fun nil) )
           (without-interrupts
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
    (without-interrupts
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
  (let ( (mac-window (cg-window-real (cg-view-window view))) )
    (with-focused-view mac-window
      (i.set-cg-current-view view)
      (if (handlep (cg-view-image view))
          (draw-picture current-window (cg-view-image view))))
    (values)))

(defun build-view-image! (view)
  (with-focused-view (cg-window-real (cg-view-window view))
    (i.set-cg-current-view view)
    (cond ((or (null (cg-view-image view))
               (not (eq (cg-view-image-state view) 'VALID)))
           (let ( (image (cg-view-image view)) )
             (setf (cg-view-image-state view) 'INVALID)
             (setf (cg-view-image       view) nil)
             (if (handlep image)
                 (kill-picture image)))
           (uncache-view-states view)
           (start-picture current-window)
           (unwind-protect
             (let ( (x0 (cg-view-x view)) (y0 (cg-view-y view))
                    (scale (cg-window-scale (cg-view-window view))) )
               (draw (cg-view-picture view) view 
                     (* x0 scale) (* y0 scale) scale
                     T))
             (setf (cg-view-image view) (get-picture current-window)))
           (setf (cg-view-image-state view) 'VALID))))
  (values))

(defun close-view-image! (view)
  (if (handlep (cg-view-image view))
      (kill-picture ; (cg-view-window view) 
                    (cg-view-image view)))
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

(defun mouse-cg-coords (mac-window where)
  (let ( (size  (view-size mac-window)) )
    (values (point-h where)
            (- (point-v size) (point-v where))
            (and (>= (point-h where) 0) 
                 (<= (point-h where) (point-h size))
                 (>= (point-v where) 0)
                 (<= (point-v where) (point-v size))))))

(defmethod view-key-event-handler ((mac-window cg-mac-device) char)
  (let ( (where (view-mouse-position mac-window)) )
    (multiple-value-bind (x y inside?)
                         (mouse-cg-coords mac-window where)
      (cond (inside?
             (setf last-mouse-position where)
             (event-dispatcher (cg-mac-device-user mac-window)
                               char 
                               x y)))))
  (values))

(defmethod view-click-event-handler ((mac-window cg-mac-device) where)
  (multiple-value-bind (x y inside?)
                       (mouse-cg-coords mac-window where)
    (cond (inside?
           (setf last-mouse-position where)
           (setf last-mouse-state    'down)
           (event-dispatcher (cg-mac-device-user mac-window)
                             button-1 
                             x y))))
  (values))

(defmethod window-mouse-up-event-handler ((mac-window cg-mac-device))
  (cond ((eq last-mouse-state 'down)
         (setf last-mouse-state 'up)
         (multiple-value-bind (x y inside?)
                              (mouse-cg-coords mac-window last-mouse-position)
           (declare (ignore inside?))
           (event-dispatcher (cg-mac-device-user mac-window)
                             button-1-up 
                             x y))))
  (values))

(defmethod window-null-event-handler ((mac-window cg-mac-device))
  (call-next-method)
  (if (cg-mac-device-positions? mac-window)
      (let ( (where (view-mouse-position mac-window)) )
        (multiple-value-bind (x y inside?)
                             (mouse-cg-coords mac-window where)
          (cond ((and (not (= where last-mouse-position))
                      inside?)
                 (setf last-mouse-position where)
                 (event-dispatcher (cg-mac-device-user mac-window)
                                   (if (mouse-down-p)
                                       pointer-drag
                                       pointer-position)
                                   x y))))))
  (values))
