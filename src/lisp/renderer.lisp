;;;-*- Mode: Lisp; Package: VEHICLES -*-
;;----------------------------------------------------------------------
;; 
;; Copyright:   Copyright (c) 1999 John Wiseman
;; File:        renderer.lisp
;; Created:     8 June 1998
;; Author:      John Wiseman (jjwiseman@gmail.com)
;; 
;; Description:  Common renderer code for Braitenberg vehicles.
;; 
;; Changes:     
;; 
;;----------------------------------------------------------------------

(in-package :vehicles)


(defun animate (&rest args)
  (let ((renderer (apply #'make-instance args)))
    (render-animation renderer)
    renderer))


;; -------------------------------
;;
;; Generic text renderer.
;;
;; -------------------------------


(defclass vehicle-renderer ()
  ((world :accessor world :initform nil :initarg :world)
   (fps :accessor fps :initform 30 :initarg :fps)
   (start-time :accessor start-time :initform 0.0 :initarg :start-time)
   (stop-time :accessor stop-time :initform 1.0 :initarg :stop-time)
   (frame-action :accessor frame-action :initform nil :initarg :frame-action)
   (current-frame-number :accessor current-frame-number :initform 0)))



(defmethod render-frame ((self vehicle-renderer))
  (let ((world (world self)))
    (format T "~&~%** Time ~,3F (~D)"
            (world-time-seconds world)
            (world-time world))
    (dolist (v (world-vehicles world))
      (print v))
    (dolist (l (world-lamps world))
      (print l))))

(defmethod do-frame-action ((self vehicle-renderer))
  (let ((action (frame-action self)))
  (when action
    (funcall action self))))

(defmethod render-animation ((self vehicle-renderer))
  (let ((frame-interval (/ 1.0 (fps self)))
        (start-time (get-internal-real-time)))
    (advance-until self (start-time self))
    (setf (current-frame-number self) 0)
    (render-frame self)
    (do-frame-action self)
    (labels ((advance-and-render (n)
               (advance-until self (+ (start-time self) (* n frame-interval)))
               (incf (current-frame-number self))
               (render-frame self)
               (do-frame-action self)
               (unless (stop-condition self)
                 (advance-and-render (+ n 1)))))
      (advance-and-render 1)
      (let* ((stop-time (get-internal-real-time))
             (duration (/ (- stop-time start-time) internal-time-units-per-second))
             (frames (+ (current-frame-number self) 1))
             (fps (/ frames duration)))
        (format T "~&Animation took ~,3F seconds to render ~S frames (~F fps)."
                duration frames fps)))))


(defmethod stop-condition ((self vehicle-renderer))
  (and (stop-time self)
       (>= (world-time-seconds (world self)) (stop-time self))))


(defun world-time-seconds (world)
  (/ (world-time world) (world-ticks-per-second world)))

(defmethod absolute-frame-number ((self vehicle-renderer))
  (floor (* (world-time-seconds (world self))
            (fps self))))



(defmethod advance-until ((self vehicle-renderer) time)
  (let ((world (world self)))
    (do ()
        ((>= (world-time-seconds world) time)
         (values))
      (step-world world))))
