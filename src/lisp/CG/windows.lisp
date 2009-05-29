;;; -------------------------------------------------------------------
;;; -------------------------------------------------------------------
;;; Common Graphics: Graphics the right way!
;;;
;;; Copyright (c) R. James Firby 1989,1991
;;; -------------------------------------------------------------------
;;; -------------------------------------------------------------------

(in-package "CG")

; --------------------------------------------------------------------
; Windows
; -------

(defconstant hit-points-to-close 2)

(defclass cg-window
  ()
  ((change      :accessor cg-window-change      :initform nil)
   (change-area :accessor cg-window-change-area :initform '()) 
   (deferred    :accessor cg-window-deferred    :initform NIL)
   (lock        :accessor cg-window-lock        :initform nil)
   (views       :accessor cg-window-views       :initform '())
   (backward    :accessor cg-window-backward    :initform '())
   (width       :accessor cg-window-w           :initform 0)
   (height      :accessor cg-window-h           :initform 0)
   (init-width  :accessor cg-window-init-w      :initform 0)
   (init-height :accessor cg-window-init-h      :initform 0)
   (scale       :accessor cg-window-scale       :initform 1.0)
   (real        :accessor cg-window-real        :initform nil)
   (foreground  :accessor cg-window-foreground  :initform nil)
   (background  :accessor cg-window-background  :initform nil)
   (hit-points  :accessor cg-window-hit-points  :initform hit-points-to-close)
   (close-fun   :accessor cg-window-close-fun   :initform nil)
   (name        :accessor cg-window-name        :initform "")))

(defmethod print-object ((self cg-window) stream)
  (print-unreadable-object (self stream :type NIL :identity T)
    (format stream "CG Window ~s" (cg-window-name self))))

(defmethod propogate-change ((self cg-window) kind cause x1 y1 x2 y2)
  (declare (ignore cause))
  (if (i.window-closed? (cg-window-real self))
      (window-close self)
      (let ( (change (cg-window-change self))
             (area   (cg-window-change-area self))
             (scale  (cg-window-scale self)) )
        (cond ((and (null change)
                    (or (eq kind 'ADD) (eq kind 'ERASE)))
               (setf (cg-window-change      self) kind)
               (setf (cg-window-change-area self) 
                     (list (* scale x1) (* scale y1)
                           (* scale x2) (* scale y2))))
              ((or (eq kind 'ADD)
                   (eq kind 'ERASE))
               (setf (cg-window-change self)
                     (if (eq change 'ERASE) 'ERASE kind))
               (setf (first area)  (min x1 (* scale (first area))))
               (setf (second area) (min y1 (* scale (second area))))
               (setf (third area)  (max x2 (* scale (third area))))
               (setf (fourth area) (max y2 (* scale (fourth area))))))))
  (values))

; ----------------------------------------------------------------------
; Released Window Functions
; -------------------------

(defun error-on-closed-window (window)
  (if (not (window-open? window))
      (error "Action on closed window: ~s." window))
  (values))

(defun window? (x) (typep x 'cg-window))

(defun window-open? (window)
  (assert (window? window) (window))
  (not (null (cg-window-real window))))

(defun window-size (window)
  (assert (window? window) (window))
  (error-on-closed-window window)
  (i.window-size (cg-window-real window)))

(defmethod window-close ((window cg-window))
  (assert (window? window) (window))
  (let ( (real-window (cg-window-real window)) )
    (cond ((and (cg-window-lock window)
                (> (cg-window-hit-points window) 0))
           (setf (cg-window-hit-points window) 
                 (- (cg-window-hit-points window) 1))
           (error "Cannot close window locked for update: ~s." window)))
    (if (functionp (cg-window-close-fun window))
      (funcall (cg-window-close-fun window)))
    (deactivate-window window)
    (mapc #'close-view (cg-window-views window))
    (setf (cg-window-views    window) nil)
    (setf (cg-window-backward window) nil)
    (setf (cg-window-change   window) 'closed)
    (setf (cg-window-real     window) nil)
    (if (cg-window-lock window) 
        (unlock-window window))
    (if (not (null real-window))
        (window-close real-window)))
  (values))

(defvar *window-counter* 0)

(defun open-window (&rest options)
  (let ( (position  (member :position  options :test #'eq))
         (size      (member :size      options :test #'eq))
         (frame     (member :window    options :test #'eq))
         (name      (member :name      options :test #'eq))
         (kind      (member :type      options :test #'eq))
         (container (member :container options :test #'eq))
         (close-fun (member :close     options :test #'eq))
         (eventsp   (member :position-events options :test #'eq))
         (colorp    (not  (member :no-color  options :test #'eq))) )
    (let ( (x    (or (and frame (second frame))    
                     (and position (second position)) nil))
           (y    (or (and frame (third frame))   
                     (and position (third position)) nil))
           (w    (or (and frame (fourth frame))  
                     (and size (second size)) nil))
           (h    (or (and frame (fifth frame))
                     (and size (third size)) nil))
           (name (if name 
                     (string (cadr name))
                     (format nil "CG Window ~s" (incf *window-counter*))))
           (kind (cadr kind)) )
      (if (not (null x)) (assert (numberp x) (x)))
      (if (not (null y)) (assert (numberp y) (y)))
      (if (not (null w)) (assert (numberp w) (w)))
      (if (not (null h)) (assert (numberp h) (h)))
      (let ( (window (make-instance 'cg-window)) )
        (setf (cg-window-name window) name)
        (let ( (real-window 
                (i.window-open window x y w h name kind colorp eventsp container)) )
          (multiple-value-bind (w h)
                               (i.window-size real-window)
            (setf (cg-window-w     window) w)
            (setf (cg-window-h     window) h)
            (setf (cg-window-init-w window) w)
            (setf (cg-window-init-h window) h))
          (multiple-value-bind (foreground background)
                               (i.window-colors real-window)
            (setf (cg-window-foreground window) foreground)
            (setf (cg-window-background window) background))
          (setf (cg-window-real window) real-window))
        (setf (cg-window-close-fun window)
              (if (functionp (cadr close-fun)) (cadr close-fun) nil))
        (activate-window window)
        window))))

;; ------------------------------------------------------------------
;; Code for drawing a window
;; -------------------------

(defun window-draw (window)
  (let ( (views       (cg-window-views       window))
         (change      (cg-window-change      window))
         (change-area (cg-window-change-area window)) )
    (cond ((eq change 'ADD)
           (dolist (view views) (erase-view-highlights view))
           (dolist (view views) (build-view-image! view)))
          ((eq change 'ERASE)
           (dolist (view views) (erase-view-highlights view))
           (dolist (view views) (build-view-image! view))
           (if (not (apply #'views-obscure-area? views change-area))
               (apply #'i.erase-window window change-area))))
    (setf (cg-window-change window) nil)
    (window-refresh window)))

(defun window-refresh (window)
  (cond ((null (cg-window-change window))
         (let ( (views (cg-window-views window)) )
           (dolist (view views) (draw-view-image view))
           (dolist (view views) (draw-view-cache view)))))
  (setf (cg-window-deferred   window) NIL)
  (setf (cg-window-hit-points window) hit-points-to-close))

;; -------------------------------------------------------------------
;; Screen Updating Stuff
;; ---------------------

(defvar active-windows '())

(defun activate-window (window)
  (if (not (member window active-windows :test #'eq))
      (setf active-windows (cons window active-windows))))

(defun deactivate-window (window)
  (setf active-windows (delete window active-windows :test #'eq)))

(defvar window-update-delays '())

(defun disable-window-updates (thing)
  (setf window-update-delays (cons thing window-update-delays))
  (values))

(defun enable-window-updates ()
  (setf window-update-delays (cdr window-update-delays))
  (if (null window-update-delays)
      (update-active-windows))
  (values))

(defun window-updates-enabled? ()
  (null window-update-delays))

(defun already-changing? (thing)
  (member thing window-update-delays :test #'eq))

(defun update-active-windows ()
  (let ( (candidate
          (do ( (windows active-windows (cdr windows)) )
              ((or (null windows)
                   (and (cg-window-deferred (car windows))
                        (null (cg-window-change (car windows))))
                   (and (not (null (cg-window-change (car windows))))
                        (null window-update-delays)))
               (car windows)))) )
    (cond ((null candidate)
           (values))
          ((maybe-lock-window? candidate)
           (unwind-protect
             (window-draw candidate)
             (unlock-window candidate))
           (update-active-windows))
          (T
           (values)))))

;; -------------------------------------------------------------------
;; Signalling a change in picture state
;; ------------------------------------

(defun signal-a-change (thing kind cause)
  (multiple-value-bind (x1 y1 x2 y2)
                       (if (listp cause)
                           (apply #'values cause)
                           (depiction-region cause))
    (propogate-change thing kind cause x1 y1 x2 y2))
  (if (window-updates-enabled?)
      (update-active-windows))
  (values))

(defmacro with-delayed-update (&body body)
  `(unwind-protect
     (progn
       (CG::disable-window-updates T)
       ,@body)
     (CG::enable-window-updates)))

; --------------------------------------------------------------------
; Views
; -----

(defclass cg-view
  ()
  ((window      :accessor cg-view-window      :initarg  :window)
   (picture     :accessor cg-view-picture     :initarg  :picture)
   (x           :accessor cg-view-x           :initarg  :x)
   (y           :accessor cg-view-y           :initarg  :y)
   (w           :accessor cg-view-w           :initarg  :w)
   (h           :accessor cg-view-h           :initarg  :h)
   (image       :accessor cg-view-image       :initform  nil)
   (image-state :accessor cg-view-image-state :initform  'INVALID)
   (cache       :accessor cg-view-cache       :initform  '())))

(defmethod print-object ((self cg-view) stream)
  (print-unreadable-object (self stream :type NIL :identity T)
    (format stream "CG View")))

(defmethod propogate-change ((self cg-view) kind cause x1 y1 x2 y2)
  (let ( (window (cg-view-window self))
         (x      (cg-view-x self))
         (y      (cg-view-y self)) )
    (cond ((window-open? window)
           (setf (cg-view-image-state self) 'INVALID)
           (let ( (x1 (+ x1 x)) (y1 (+ y1 y))
                  (x2 (+ x2 x)) (y2 (+ y2 y)) )
             (if (or (null (cg-view-w self)) (null (cg-view-h self)))
                 (propogate-change window kind cause x1 y1 x2 y2)
                 (propogate-change window kind cause
                                   (max x1 0) 
                                   (max y1 0)
                                   (min x2 (+ x (cg-view-w self)))
                                   (min y2 (+ y (cg-view-h self)))))))
          (T
           (close-view self)))))

(defun views-obscure-area? (views x1 y1 x2 y2)
  (if (null views)
      NIL
      (let ( (view (car views)) )
        (let ( (vxo (cg-view-x view))
               (vyo (cg-view-y view)) )
          (cond ((obscures-area? (cg-view-picture view)
                                 (if (null vxo) x1 (- x1 vxo))
                                 (if (null vyo) y1 (- y1 vyo))
                                 (if (null vxo) x2 (- x2 vxo))
                                 (if (null vyo) y2 (- y2 vyo)))
                 T)
                (T
                 (views-obscure-area? (cdr views) x1 y1 x2 y2)))))))

; ------------------
; Internal Functions
; ------------------

(defun erase-view-highlights (view)
  (let ( (cache (cg-view-cache view)) )
    (i.set-cg-current-view view)
    (dolist (state cache)
      (if (highlight-state? state)
          (draw-cached-item state))))
  (values))

(defun draw-view-cache (view)
  (dolist (state (cg-view-cache view))
    (draw-cached-item state))
  (values))

; ------------------
; Released Functions
; ------------------

(defun view? (x) (typep x 'cg-view))

(defun open-view (window thing &rest options)
  (assert (window? window)   (window))
  (let ( (position (member :position options :test #'eq))
         (size     (member :size     options :test #'eq)) )
    (let ( (x (if position (second position) 0))
           (y (if position (third position)  0))
           (w (if size     (second size)     nil))
           (h (if size     (third size)      nil))
           (picture (if (picture? thing)
                        thing
                        (insert! (make-empty-picture) thing))) )
      (if (not (null x)) (assert (numberp x) (x)))
      (if (not (null y)) (assert (numberp y) (y)))
      (if (not (null w)) (assert (numberp w) (w)))
      (if (not (null h)) (assert (numberp h) (h)))
      (let ( (view (make-instance 'cg-view
                                  :window  window
                                  :picture picture
                                  :x       x
                                  :y       y
                                  :w       w
                                  :h       h)) )
        (if (null (cg-window-views window))
            (setf (cg-window-views window) (list view))
            (let ( (end (last (cg-window-views window))) )
              (setf (cdr end) (list view))))
        (setf (cg-window-backward window)
              (cons view (cg-window-backward window)))
        (add-picture-user! picture view)
        (signal-a-change window 'ADD picture)
        view))))

(defun close-view (view)
  (assert (view? view) (view))
  (let ( (window  (cg-view-window view))
         (picture (cg-view-picture view)) )
    (multiple-value-bind (x1 y1 x2 y2)
                         (depiction-region picture)
      (cond ((not (null window))
             (close-view-image! view)
             (setf (cg-view-image       view) nil)
             (setf (cg-view-image-state view) nil)
             (setf (cg-window-views window)
                   (delete view (cg-window-views window) :test #'eq))
             (setf (cg-window-backward window)
                   (delete view (cg-window-views window) :test #'eq))
             (remove-picture-user! picture view)
             (uncache-view-states view)
             (setf (cg-view-cache   view) nil)
             (setf (cg-view-picture view) nil)
             (signal-a-change view 'ERASE (list x1 y1 x2 y2))
             (setf (cg-view-window  view) nil)))))
  (values))

(defun uncache-view-states (view)
  (let ( (cache (cg-view-cache view)) )
    (setf (cg-view-cache view) '())
    (dolist (state cache) (uncache state))))
