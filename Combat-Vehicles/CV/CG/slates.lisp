;;; -------------------------------------------------------------------
;;; -------------------------------------------------------------------
;;; Common Graphics: Graphics the right way!
;;;
;;; Copyright (c) R. James Firby 1989,1991
;;; -------------------------------------------------------------------
;;; -------------------------------------------------------------------

(in-package "CG")

(defclass cachable
  ()
  ((locations :accessor cached-locations :initform '())))

(defun cachable? (self) (typep self 'cachable))

(defun cache (object view x y scale)
  (let ( (state (list object view x y scale)) )
    (setf (cached-locations object) (cons state (cached-locations object)))
    (setf (cg-view-cache view)      (cons state (cg-view-cache view)))
    (values)))

(defun uncache (state)
  (let ( (item (car state))
         (view (cadr state)) )
    (setf (cached-locations item) 
          (delete state (cached-locations item) :test #'eq))
    (setf (cg-view-cache view) 
          (delete state (cg-view-cache view) :test #'eq))
    (values)))

(defun highlight-state? (state) (typep (car state) 'highlight))

(defun draw-cached-item (state)
  (let ( (item  (car state))
         (view  (car (cdr state)))
         (x     (car (cddr state)))
         (y     (car (cdddr state)))
         (scale (car (cddddr state))) )
    (unwind-protect
      (cond ((highlight? item)
             (i.set-cg-current-mode :INVERT)
             (draw item view x y scale NIL))
            (T
             (i.set-cg-current-mode :NORMAL)
             (draw item view x y scale NIL)))
      (i.set-cg-current-mode :NORMAL)))
  (values))

(defun draw-at-cached-locations (thing)
  (unwind-protect
    (dolist (state (cached-locations thing)) 
      (let ( (item  (car state))
             (view  (car (cdr state)))
             (x     (car (cddr state)))
             (y     (car (cdddr state)))
             (scale (car (cddddr state))) )
        (cond ((maybe-lock-window? (cg-view-window view))
               (unwind-protect
                 (progn
                   (i.set-cg-current-view view)
                   (cond ((highlight? item)
                          (i.set-cg-current-mode :INVERT)
                          (draw item view x y scale NIL))
                         (T
                          (i.set-cg-current-mode :NORMAL)
                          (draw item view x y scale NIL))))
                 (unlock-window (cg-view-window view))
                 (i.set-cg-current-mode :NORMAL))))))
    (update-active-windows))
  (values))

(defun funcall-at-cached-locations (thing fun)
  (unwind-protect
    (dolist (state (cached-locations thing))
      (let ( (item  (car state))
             (view  (car (cdr state)))
             (x     (car (cddr state)))
             (y     (car (cdddr state)))
             (scale (car (cddddr state))) )
        (with-window-lock (cg-view-window view)
          (i.set-cg-current-view view)
          (i.set-cg-current-transform x y scale)
          (cond ((highlight? item)
                 (i.set-cg-current-mode :INVERT)
                 (funcall fun))
                (T
                 (i.set-cg-current-mode :NORMAL)
                 (funcall fun))))
        (i.set-cg-current-mode :NORMAL)))
    (update-active-windows))
  (values))

; --------------------------------------------------------------------
; Highlights
; ----------

(defclass highlight
  (picture cachable)
  ((erased? :accessor highlight-erased? :initform NIL)))

(defmethod propogate-change ((self highlight) kind cause x1 y1 x2 y2)
  (if (and (highlight-erased? self)
           (or (eq kind 'ERASE)
               (eq kind 'ADD)))
      (propogate-change self 'REGION cause x1 y1 x2 y2)
      (call-next-method)))

(defun highlight (thing)
  (insert! (make-instance 'highlight :x 0.0 :y 0.0 :scale 1.0) thing))

(defun highlight? (thing)
  (typep thing 'highlight))

(defun erase-highlight-from-screen (highlight)
  (cond ((not (already-changing? highlight))
         (draw-at-cached-locations highlight)
         (setf (highlight-erased? highlight) T))))

(defun redraw-highlight-on-screen (highlight)
  (cond ((and (not (already-changing? highlight))
              (highlight-erased? highlight))
         (draw-at-cached-locations highlight)
         (setf (highlight-erased? highlight) NIL))))

(defmacro with-change-to-highlight (hilite &body body)
  (let ( (thing (gentemp "hilite")) )
    `(let ( (,thing ,hilite) )
       (assert (highlight? ,thing) (,thing))
       (unwind-protect
         (progn
           (CG::erase-highlight-from-screen ,thing)
           (CG::disable-window-updates ,thing)
           ,@body)
         (progn
           (CG::enable-window-updates)
           (CG::redraw-highlight-on-screen ,thing))))))

; --------------------------------------------------------------------
; Slates
; ------

(defclass slate-primitive
  (primitive cachable)
  ((update-fun :accessor slate-primitive-update :initarg :update)
   (extent-fun :accessor slate-primitive-extent :initarg :extent)
   (holds-fun  :accessor slate-primitive-holds  :initarg :holds)))

(defmethod draw-object ((self slate-primitive) x y scale)
  (let ( (update-fun (slate-primitive-update self)) )
    (cond ((not (null update-fun))
           (i.set-cg-current-transform x y scale)
           (funcall update-fun))))
  (values))

(defmethod extent ((self slate-primitive))
  (let ( (extent-fun (slate-primitive-extent self)) )
    (if (not (null extent-fun))
        (funcall extent-fun)
        (values nil nil nil nil))))

(defmethod holds-point? ((self slate-primitive) x y)
  (let ( (holds-fun (slate-primitive-holds self)) )
    (if (not (null holds-fun))
        (funcall holds-fun x y)
        NIL)))

(defmethod obscures-area? ((self slate-primitive) x1 y1 x2 y2)
  (declare (ignore self x1 y1 x2 y2))
  NIL)

(defclass slate
  ()
  ((surface    :accessor slate-surface :initarg :surface)
   (depicition :accessor depiction     :initarg :depict)))

(defun slate (&key update region includes)
  (assert (or (null update)   (functionp update))   (update))
  (assert (or (null region)   (functionp region))   (region))
  (assert (or (null includes) (functionp includes)) (includes))
  (let ( (surface (make-instance 'slate-primitive
                                 :name     :slate
                                 :update   update
                                 :extent   region
                                 :holds    includes)) )
    (make-instance 'slate
                   :surface surface
                   :depict  (insert! (make-empty-picture) surface))))

(defmacro with-drawing-to-slate (slate &body body)
  (let ( (slate-name (gentemp "SLATE")) )
    `(let ( (,slate-name (CG::slate-surface ,slate)) ) 
       (CG::funcall-at-cached-locations
        ,slate-name
        #'(lambda () ,@body))
       (when (CG::slate-primitive-extent ,slate-name)
         (CG::signal-a-change (CG::depiction ,slate-name)
                              'CG::REGION 
                              (CG::depiction ,slate-name))))))

