;;;-*- Mode: Lisp; Package: VEHICLES -*-
;;----------------------------------------------------------------------
;; 
;; Copyright:   Copyright (c) 1999 John Wiseman
;; File:        geometry.lisp
;; Created:     23 March 1997
;; Author:      John Wiseman (jjwiseman@gmail.com)
;; 
;; Description: Geometry utilities
;; 
;; Changes:     
;; 
;;----------------------------------------------------------------------

(in-package "VEHICLES")


;;----------------------------------------
;;
;; Basic math
;;
;;----------------------------------------

(defun rad->deg (rad)
  "Converts radians to degrees."
  (* rad #.(/ 180.0 pi)))

(defun deg->rad (deg)
  "Converts degrees to radians."
  (* deg #.(/ pi 180.0)))


(defun square (x)
  (* x x))

(declaim (inline square))


;;----------------------------------------
;;
;; Geometry
;;
;;----------------------------------------

(defstruct 2d-location
  (x 0.0)
  (y 0.0))

(defun 2d-location (x y)
  (make-2d-location :x x :y y))


(defstruct 2d-orientation
  (theta 0.0))

(defun 2d-orientation (theta)
  (make-2d-orientation :theta theta))


(defmethod angle ((o1 2d-orientation) (o2 2d-orientation))
  "Returns the angle between two orientations."
  ;; bogus
  (- (2d-orientation-theta o2)
     (2d-orientation-theta o1)))

(defmethod distance ((p1 2d-location) (p2 2d-location))
  (sqrt (+ (square (- (2d-location-x p1)
		      (2d-location-x p2)))
	   (square (- (2d-location-y p1)
		      (2d-location-y p2))))))

(defmethod square-distance ((p1 2d-location) (p2 2d-location))
  (+ (square (- (2d-location-x p1)
                (2d-location-x p2)))
     (square (- (2d-location-y p1)
                (2d-location-y p2)))))

(defmethod add-location ((p1 2d-location) (p2 2d-location))
  (2d-location (+ (2d-location-x p1)
		  (2d-location-x p2))
	       (+ (2d-location-y p1)
		  (2d-location-y p2))))

(defmethod add-orientation ((o1 2d-orientation) (o2 2d-orientation))
  (2d-orientation (+  (2d-orientation-theta o1)
		      (2d-orientation-theta o2))))

(defmethod orientation-between ((p1 2d-location) (p2 2d-location))
  (2d-orientation (atan (- (2d-location-y p2)
                           (2d-location-y p1))
	                (-  (2d-location-x p2)
			    (2d-location-x p1)))))


