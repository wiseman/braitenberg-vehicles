;;; -------------------------------------------------------------------
;;; -------------------------------------------------------------------
;;; Common Graphics: Graphics the right way!
;;;
;;; Copyright (c) R. James Firby 1991
;;; -------------------------------------------------------------------
;;; -------------------------------------------------------------------

(in-package "CG")

;;; -------------------------------------------------------------------
;;; Handling Events
;;; ---------------

(defgeneric event-handler (object event x y))

(defvar compute-arg-list (list 'object T T T))

(defun handles-events? (object)
  (setf (car compute-arg-list) object)
  (not (null (compute-applicable-methods #'event-handler compute-arg-list))))

(defun event-dispatcher (window event x y)
  (multiple-value-bind (object new-x new-y)
                       (find-object-catching-event 
                        (cg-window-backward window) 
                        event 
                        (/ x (cg-window-scale window))
                        (/ y (cg-window-scale window)))
    (post-event object event new-x new-y))
  (values))

;; --------------------------------------------------------------------
;; Find Object Catching Event
;; --------------------------

(defun find-object-catching-event (views event x0 y0)
  (if (null views)
      (values nil x0 y0)
      (let ( (view (car views)) )
        (let ( (picture (cg-view-picture view))
               (x       (- x0 (cg-view-x view)))
               (y       (- y0 (cg-view-y view)))
               (w       (cg-view-w view))
               (h       (cg-view-h view)) )
          (multiple-value-bind (found? object new-x new-y)
                               (cond ((null (picture-events? picture))
                                      (values NIL nil x y))
                                     ((or (null w) (null h))
                                      (find-event-object picture
                                                         picture
                                                         event x y))
                                     ((and (>= x 0) (<= x w)
                                           (>= y 0) (<= y h))
                                      (find-event-object picture
                                                         picture
                                                         event x y))
                                     (T
                                      (values NIL nil x y)))
            (if (and found? object)
                (values object new-x new-y)
                (find-object-catching-event (cdr views) event x0 y0)))))))

(defun find-event-object (object picture event x y)
  (let ( (active-event (handles-events? object)) )
    (cond ((picture? picture)
           (let ( (x0 (/ (- x (picture-x picture)) (picture-scale picture)))
                  (y0 (/ (- y (picture-y picture)) (picture-scale picture))) )
             (if (may-hold-point? picture x y)
               (multiple-value-bind (found? piece new-x new-y)
                                    (find-event-object-in-pieces (picture-backward picture)
                                                                 NIL
                                                                 event
                                                                 x0 y0
                                                                 active-event)
                 (cond ((and found? piece)
                        (values T piece new-x new-y))
                       ((and found? active-event)
                        (values T object x0 y0))
                       (T
                        (values found? nil x0 y0))))
               (values nil nil x0 y0))))
          ((primitive? picture)
           (values (holds-point? picture x y) nil x y))
          ((depiction? picture)
           (find-event-object (depiction-thing picture)
                              (depiction-picture picture)
                              event x y))
          (T
           (find-event-object picture
                              (depiction picture)
                              event x y)))))

(defun find-event-object-in-pieces (pieces holds-point event x0 y0 active-event)
  (if (null pieces)
      (values holds-point nil x0 y0)
      (multiple-value-bind (found? new-object new-x new-y)
                           (find-event-object (car pieces)
                                              (car pieces)
                                              event x0 y0)
        (cond ((and found? new-object)
               (values T new-object new-x new-y))
              ((and found? active-event)
               (values T nil x0 y0))
              (found?
               (find-event-object-in-pieces (cdr pieces) T
                                            event x0 y0 active-event))
              (T
               (find-event-object-in-pieces (cdr pieces) holds-point
                                            event x0 y0 active-event))))))
  

;; --------------------------------------------------------------------
;; Post an Event to the Event Queue
;; --------------------------------

(defvar the-current-event-queue '(queue))

(defun add-to-queue! (queue thing)
  (setf (cdr (last queue)) (list thing))
  queue)

(defun get-from-queue! (queue)
  (let ( (value (cadr queue)) )
    (if (not (null (cdr queue)))
        (setf (cdr queue) (cddr queue)))
    value))

(defvar last-event-object nil)
(defvar last-event-event  nil)
(defvar last-event-x      0)
(defvar last-event-y      0)
(defvar button-pressed?   NIL)

(defun new-event (queue object event x y)
  (cond ((not (null object))
         (setf last-event-object object)
         (setf last-event-event  event)
         (setf last-event-x      x)
         (setf last-event-y      y)
         (cond ((handles-events? object)
                (add-to-queue! queue (list object event x y))
                (process-event-queue)))))
  (values))

(defun post-leave-object-events (queue)
  (if button-pressed?
      (new-event queue last-event-object button-no-up last-event-x last-event-y))
  (new-event queue last-event-object pointer-exit last-event-x last-event-y)
  (setf last-event-object nil)
  (setf button-pressed?   NIL)
  (values))
         
(defun post-event (object event x y)
  (cond ((null object)
         (if (not (null last-event-object))
             (post-leave-object-events the-current-event-queue)))
        ((not (eq object last-event-object))
         (post-leave-object-events the-current-event-queue)
         (new-event the-current-event-queue object pointer-enter x y)
         (post-event object event x y))
        ((eq event pointer-drag)
         (if button-pressed?
             (new-event the-current-event-queue object event x y)))
        ((eq event button-1-up)
         (if button-pressed?
             (new-event the-current-event-queue object event x y))
         (setf button-pressed? NIL))
        ((eq event button-1)
         (setf button-pressed? T)
         (new-event the-current-event-queue object event x y))
        (T
         (new-event the-current-event-queue object event x y))))

;; --------------------------------------------------------------------
;; Process the current event queue
;; -------------------------------

(defun process-event-queue ()
  (let ( (event-bundle (get-from-queue! the-current-event-queue)) )
    (if (null event-bundle)
        (values)
        (let ( (object (car event-bundle))
               (event  (cadr event-bundle))
               (x      (caddr event-bundle))
               (y      (cadddr event-bundle)) )
          (event-handler object event x y)
          (process-event-queue)))))
               
        