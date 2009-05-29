;;;-*- Mode: Lisp; Package: VEHICLES -*-
;;----------------------------------------------------------------------
;; 
;; Copyright:   Copyright (c) 1999 John Wiseman
;; File:        qd-renderer.lisp
;; Created:     12 October 1998
;; Author:      John Wiseman (jjwiseman@gmail.com)
;; 
;; Description: Beginning of an MCL QuickDraw renderer for
;;		Braitenberg vehicles.
;; 
;; Changes:     
;; 
;;----------------------------------------------------------------------

(in-package "VEHICLES")

(ccl:require :quickdraw)

(defclass quickdraw-renderer (vehicle-renderer)
  ((view :accessor window :initform nil :initarg :view)
   (view-size :accessor view-size :initform 300 :initarg :view-size)
   (scale :accessor scale :initform 20 :initarg :scale)
   (labels? :accessor labels? :initform NIL :initarg :labels?)))



(defmethod render-animation ((self quickdraw-renderer))
  (let* ((vsize (view-size self))
         (size (if (and vsize (< vsize 65535))
                 (make-point vsize vsize)
                 vsize)))
    (unless (view self)
      (setf (view self) (make-instance 'window :view-size size))))
  (setf (view-size self) (ccl:view-size (view self)))
  (call-next-method))

(defmethod render-frame ((self quickdraw-renderer))
  (let* ((world (world self))
         (lamps (world-lamps world))
         (view (view self))
         (size (view-size view)))
    (ccl:paint-rect view #(0 0) view-size)
    (dolist (v (world-vehicles world))
      (render-platform self v))
    (dolist (l (world-lamps world))
      (render-platform self l))))

(defmethd render-platform ((renderer quickdraw-renderer) (self two-wheeled-vehicle))
  (let* ((l (location self))
         (x (2d-location-x l))
         (y (2d-location-y l))
         (theta (2d-orientation-theta (orientation self)))
         (scale (scale renderer)))
    (let ((radius (/ (wheel-base self) 2)))
      (let ((lx (floor (* (- x radius) scale)))
            (rx (floor (* (+ x radius) scale)))
            (uy (floor (- y radius)))
            (ly (floor (+ y radius))))
        (ccl:with-fore-color (vcolor->qd-color (color self))
          (ccl:paint-oval (view renderer) lx uy rx ly))
        (ccl:with-fore-color *black-color*
          (ccl:frame-oval (view renderer) lx uy rx ly)))
      

  
    


(defun vcolor->qd-color (vcolor &optional (brightness 1.0))
  (ccl:make-color (floor (* (color-r vcolor) brightness 65535))
                  (floor (* (color-g vcolor) brightness 65535))
                  (floor (* (color-b vcolor) brightness 65535))))

