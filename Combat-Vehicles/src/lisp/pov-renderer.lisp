;;;-*- Mode: Lisp; Package: VEHICLES -*-
;;----------------------------------------------------------------------
;; 
;; Copyright:   Copyright (c) 1999 John Wiseman
;; File:        pov-renderer.lisp
;; Created:     8 June 1998
;; Author:      John Wiseman (wiseman@neodesic.com)
;; 
;; Description: POV-Ray renderer for Braitenberg vehicles.
;; 
;; Changes:     
;; 
;;----------------------------------------------------------------------

(in-package "VEHICLES")

(export 'pov-renderer)


(defclass pov-renderer (vehicle-renderer)
  ((ambient-light :accessor ambient-light :initform (list 0.5 0.5 0.5) :initarg :ambient-light)
   (axes? :accessor axes? :initform NIL :initarg :axes?)
   (base-path :accessor base-path :initform nil :initarg :base-path)
   (includes :accessor includes :initform '("colors.inc" "shapes.inc" "textures.inc" "consts.inc"))))


(defmethod initialize-instance :around ((self pov-renderer) &key)
  (call-next-method)
  (unless (base-path self)
    (cerror "Use logical host \"home:\" as the base pathname."
            "A base pathname must be specified with :BASE-PATH.")
    (setf (base-path self) (make-pathname :host "home"))))
   


(defparameter *axes.pov* 
"

union {
    object {
    	box { <0, 0, 0> <100, .1, .1> }
	pigment { color rgb <1, 0, 0> }
    }

    object {
	box { <0, 0, 0> <.1, 100, .1> }
	pigment { color rgb <0, 1, 0> }
    }

    object {
	box { <0, 0, 0> <.1, .1, 100> }
	pigment { color rgb <0, 0, 1> }
    }
}

")

(defmethod includes-string ((self pov-renderer))
  (with-output-to-string (out)
    (format out "~&~%")
    (dolist (i (includes self))
      (format out "~&#include \"~A\"" i))))

(defmethod global-settings-string ((self pov-renderer))
  (format nil "~&~%global_settings {~&~8tambient_light color rgb ~A~&~8t assumed_gamma 1.0~&}"
          (apply #'pov-vector-string (ambient-light self))))


(defparameter *frame-prologue.pov*

"

plane {
	<0, 1, 0> 0
	texture {
		finish { Glossy }
		pigment { color rgb <0.1 0.1 0.3> }
	}
}


/* possible axes */
~A

camera {
	direction ~A
	location  ~A
	look_at   ~A
}


light_source { <0, 10, 0> color rgb <.5, .5, .5> }


"
)

(defun pov-color-string (color)
  (pov-vector-string (color-r color) (color-g color) (color-b color)))


(defmethod frame-prologue ((self pov-renderer))
  (with-output-to-string (out)
    (format out "~A" (includes-string self))
    (format out "~A" (global-settings-string self))
    (format out *frame-prologue.pov*
            (if (axes? self) *axes.pov* "")
            (apply #'pov-vector-string (camera-direction self))
            (apply #'pov-vector-string (camera-location self))
            (apply #'pov-vector-string (camera-look-at self)))))

(defmethod camera-direction ((self pov-renderer))
  (list 0 0 '|FoV_60|))

(defmethod camera-location ((self pov-renderer))
  (list 0 6 -9))

(defmethod camera-look-at ((self pov-renderer))
  (list 0 0 0.5))






(defparameter *pov-epilogue* "")



(defmethod render-frame ((self pov-renderer))
  (format T "~&Frame ~D (~S) at time ~F"
          (current-frame-number self)
          (frame-filename self)
          (world-time-seconds (world self)))
  (with-open-file (stream (frame-filename self) :direction :output)
    (format stream "~&// ~S Time: ~F  Frame: ~S"
            self
            (world-time-seconds (world self))
            (current-frame-number self))
    (format stream "~A" (frame-prologue self))
    (dolist (vehicle (world-vehicles (world self)))
      (format stream (pov-object-string self vehicle)))
    (dolist (lamp (world-lamps (world self)))
      (format stream (pov-object-string self lamp)))
    (format stream "~&~%~A" *pov-epilogue*)))


(defmethod frame-filename ((self pov-renderer))
  (merge-pathnames (make-pathname :name (format nil "~6,'0D" (absolute-frame-number self))
                                  :type "pov")
                   (base-path self)))







(defmethod pov-position-string ((self platform))
  (let ((loc (location self)))
    (pov-vector-string (2d-location-x loc) 0 (2d-location-y loc))))

(defmethod pov-orientation-string ((self platform))
  (pov-vector-string 0.0
                     (* (2d-orientation-theta (orientation self))
                        (/ -180.0 pi))
                     0.0))

(defun pov-vector-string (e1 e2 e3)
  ;; POV seems to not like excessive precision.
  (format nil "<~,5F, ~,5F, ~,5F>" e1 e2 e3))


(defmethod pov-object-string ((renderer pov-renderer) (self two-wheeled-vehicle))
  (with-output-to-string (out)
    (format out "~&~%// ~S~&~%" self)
    (format out "~&#declare car_position = ~A" (pov-position-string self))
    (format out "~&#declare car_orientation = ~A" (pov-orientation-string self))
    (let ((color (color self)))
      (format out "~&#declare car_color = color rgb ~A"
              (pov-vector-string (color-r color)
                                 (color-g color)
                                 (color-b color))))
    (format out "~&#include \"vehicle.inc\"~&")))



(defmethod pov-object-string ((renderer pov-renderer) (self lamp))
  (with-output-to-string (out)
    (format out "~&~%// ~S~&~%" self)
    (format out "~&#declare lamp_position = ~A" (pov-position-string self))
    (let ((c (color self))
          (radiators (remove-if-not
                      #'(lambda (r)
                          (eq (platform r) self))
                      (world-radiators (world self)))))
      (let ((b (if (null radiators)
                 1.0
                 (min 1.0
                      (reduce #'+ (mapcar #'output radiators))))))
        (format out "~&#declare lamp_color = color rgb ~A"
                (pov-vector-string (* b (color-r c))
                                   (* b (color-g c))
                                   (* b (color-b c))))))
    (format out "~&#include \"lamp.inc\"~&")))

