;;;-*- Mode: Lisp; Package: VEHICLES -*-
;;----------------------------------------------------------------------
;; 
;; Copyright:   Copyright (c) 1999 John Wiseman
;; File:        vehicles.lisp
;; Created:     18 July 1998
;; Author:      John Wiseman (wiseman@neodesic.com)
;; 
;; Description: Braitenberg vehicle simulator
;; 
;; Changes:     
;; 
;;----------------------------------------------------------------------

(in-package :vehicles)



(defstruct color r g b)

(defun rgb->color (r g b)
  (make-color :r r :g g :b b))


(defun load-world (world-file)
  (let ((*world* (make-world))
        (*package* (find-package "VEHICLES")))
    (reset-object-library)
    (load world-file :verbose T :print T)
    (world-stats *world*)
    *world*))


;; anything with a name

(defclass named-object ()
  ((name :accessor name-of :initform nil :initarg :name)
   (world :accessor world :initform nil :initarg :world)))

(defmethod print-object ((self named-object) stream)
  (print-unreadable-object (self stream :type T :identity T)
    (princ (name-of self) stream)))


;;----------------------------------------
;;
;; Node
;;
;; A node in a nervous system.  Correctly handles asynchronous updating.
;; 
;;
;; -- Methods
;;
;; OUTPUT
;; Returns the node's output, only recomputing if necessary.
;;
;; COMPUTE-OUTPUT
;; Forces a nodes output to be recomputed, and returns the result. Child
;; classes should specialize this method.
;;
;;----------------------------------------

;; The base node class.

(defclass node (named-object)
  ((cached-output :accessor cached-output :initform 0)
   (cache-time    :accessor cache-time :initform -1)
   (probes :accessor probes :initform nil)))

(defmethod output ((self node))
  (let ((output (cached-output self))
        (world-time (world-time (world self))))
    ;; only recompute what the output should be if we haven't
    ;; computed and cached it already
    (when (/= world-time (cache-time self))
      (setf (cache-time self) world-time)
      (setf (cached-output self) (compute-output self)))
    output))

(defvar *probes-active-p* T)

(defmethod compute-output :around ((self node))
  (let ((value (call-next-method)))
    (when (and *probes-active-p* (probes self))
      (dolist (probe (probes self))
        (funcall probe self)))
    value))


  



;; --------------------
;; Node with inputs
;; 
;; Any node that accepts inputs.
;; --------------------

(defclass node-with-inputs (node)
  ((current-inputs :accessor inputs :initform '() :initarg :inputs)
   (original-inputs :accessor original-inputs :initform '())))


;; Add inputs

(defmethod sum-inputs ((self node-with-inputs))
  (sum-node-values #'output (inputs self)))

(defun sum-node-values (function nodes)
  (reduce #'+ nodes :key function))



;; --------------------
;; Integrator
;; 
;; A node that has digital inputs and an analog output, and whose
;; output is a running average of the input.
;; --------------------

(defclass integrator (node-with-inputs)
  ((decay-factor :accessor decay-factor :initform 0.0 :initarg :decay-factor)))

(defmethod compute-output ((self integrator))
  (let ((input-sum (sum-inputs self))
	(decay-factor (expt (decay-factor self)
			    (/ 5.0 (world-ticks-per-second (world self))))))
    (+ (* (min 1.0 input-sum) (- 1.0 decay-factor))
       (* (cached-output self) decay-factor))))


;; --------------------
;; Neurode
;; 
;; Nodes that act as computational elements.
;; --------------------
	       
(defclass neurode (node-with-inputs)
  ((threshold  :accessor threshold :initform 1 :initarg :threshold)
   (inhibitors :accessor inhibitors :initform '() :initarg :inhibitors)))

(defmethod sum-inhibitors ((self neurode))
  (sum-node-values #'output (inhibitors self)))

(defmethod compute-output ((self neurode))
  (let ((input-sum (sum-inputs self))
        (inhib-sum (sum-inhibitors self)))
    (if (and (= inhib-sum 0) (>= input-sum (threshold self)))
      1
      0)))


;; --------------------
;; Radiator
;; 
;; A source of radiation.
;; --------------------

(defclass radiator (integrator)
  ((radiation-type :accessor radiation-type :initarg :radiation-type :initform nil)
   (brightness	   :accessor brightness :initarg :brightness :initform 1.0)
   (platform	   :accessor platform :initarg :platform :initform nil)))

(defmethod location ((self radiator))
  (location (platform self)))

(defmethod compute-output ((self radiator))
  (if (and (null (inputs self))
           (null (original-inputs self)))
      (brightness self)
    ;; will this work?
    (call-next-method)))


;; --------------------
;; Sensor
;; 
;; Sensors on platforms.  Sensors take an analog input corresponding
;; to the experienced intensity of the radiation to which they are
;; sensitive.  They have a digital output whose rate is proportional
;; to the radiation intensity.
;; --------------------

(defclass sensor (node)
  ((radiation-type 	 :accessor radiation-type)
   (sensitivity		 :accessor sensitivity :initform 1.0 :initarg :sensitivity)
   (directional?	 :accessor directional? :initform T :initarg :directional?)
   (relative-orientation :accessor relative-orientation :initarg :relative-orientation)
   (relative-location 	 :accessor relative-location :initarg :relative-location)
   (platform 		 :accessor platform :initarg :platform)
   (accumulator 	 :accessor accumulator :initform 0.0)))

(defmethod compute-output ((self sensor))
  (let ((rate (* (sensitivity self) (sum-inputs self))))
    (let ((accum (min 1.0 (+ rate (accumulator self)))))
      (let ((pulse (cond ((>= accum 1.0)
			  (setq accum (- accum 1.0))
			  1)
			 (T 0))))
	(setf (accumulator self) accum)
	pulse))))


;; Returns the sum of the received signal intensities.

(defmethod sum-inputs ((self sensor))
  (sum-node-values #'(lambda (r)
                       (signal-strength self r))
                   (radiators-of-type (world self) (radiation-type self))))
    
;; Calculates the radiation received by a sensor from a radiator.

(defmethod signal-strength ((sensor sensor) (radiator radiator))
  ;; Assume sensors are shielded from radiators on same platform
  (if (eq (platform sensor) (platform radiator))
    0.0
    (let ((lp (location radiator))
	  (sp (location sensor)))
      (let ((sd (square-distance sp lp))
	    (i (cos (angle (orientation sensor)
			   (orientation-between sp lp)))))
        (if (directional? sensor)
	  (/  (* (+ i 1.0) (output radiator)) (* 2.0 sd))
          (/ (output radiator) (* 2.0 sd)))))))


(defmethod location ((self sensor))
  (add-location (location (platform self))
	        (relative-location self)))

(defmethod orientation ((self sensor))
  (add-orientation (orientation (platform self))
		   (relative-orientation self)))


;; --------------------
;; Directional Sensor
;; 
;; A sensor with a limited field of fiew and non-uniform directional
;; sensitivity.
;;
;; Not implemented.
;; --------------------

(defclass directional-sensor (sensor)
  ((field-of-view :accessor field-of-view :initform (/ pi 2) :initarg :field-of-view)))


;; --------------------
;; Motor
;; 
;; A motor on a vehicle.
;; --------------------

(defclass motor (integrator)
  ((location :accessor location :initform nil :initarg :location)))


;;----------------------------------------
;;
;; Platforms
;;
;; Objects with a position and orientation in space.
;;
;;----------------------------------------

(defclass platform (named-object)
  ((location :accessor location :initarg :location :initform nil)
   (orientation :accessor orientation :initarg :orientation :initform nil)
   (old-location :accessor old-location :initform nil)
   (depiction :accessor platform-depiction :initarg :depiction :initform nil)
   (color :accessor color :initarg :color :initform (rgb->color 0 0 0))
   (bindings :accessor bindings :initarg :bindings :initform nil)))


(defmethod velocity ((self platform))
  (if (old-location self)
    (* (distance (location self) (old-location self))
       (world-ticks-per-second (world self)))
    0))

(defmethod print-object ((self platform) stream)
  (print-unreadable-object (self stream :type T :identity T)
    (format stream "[(~,2F, ~,2F) ~,2F ~,2F units/s]"
            (if (location self)
              (2d-location-x (location self))
              0)
            (if (location self)
              (2d-location-y (location self))
              0)
            (if (orientation self)
              (2d-orientation-theta (orientation self))
              0)
            (velocity self))))

(defmethod nodes ((self platform))
  (lookup :brain (bindings self)))



;; --------------------
;; Two-wheeled Vehicle
;; 
;; A vehicle with two caster wheels in front and two motorized wheels in the
;; rear.
;; --------------------

(defclass two-wheeled-vehicle (platform)
  ((max-speed :accessor max-speed :initarg :max-speed :initform 10.0)
   (wheel-radius :accessor wheel-radius :initarg :wheel-radius :initform 0.05)
   (wheel-base :accessor wheel-base :initarg :wheel-base :initform 0.3)
   (length :accessor vehicle-length :initarg :vehicle-length :initform 0.6)
   (right-motor :accessor right-motor :initarg :right-motor :initform nil)
   (left-motor :accessor left-motor :initarg :left-motor :initform nil)
   (guns :accessor guns :initarg :guns :initform '())
   (fired-flag :accessor fired-flag :initform nil)
   (last-move-time :accessor last-move-time :initform nil)
   (left-wheel-rotation-angle :accessor left-wheel-rotation-angle :initform 0.0)
   (right-wheel-rotation-angle :accessor right-wheel-rotation-angle :initform 0.0)))
   

(defmethod print-object ((self two-wheeled-vehicle) stream)
  (print-unreadable-object (self stream :type T :identity T)
    (format stream "~S [(~,2F, ~,2F) ~,2F ~,2F units/s]"
            (name-of self)
            (if (location self)
              (2d-location-x (location self))
              0)
            (if (location self)
              (2d-location-y (location self))
              0)
            (if (orientation self)
              (2d-orientation-theta (orientation self))
              0)
            (velocity self))))


;; This is a surprisingly complicated function.
;;
;; 1. Compute the distance that each wheel moves.  This is based on
;; the output of the motor driving the wheel.
;;
;; 2. These two distances are assumed to be the lengths of two arcs of
;; concentric circles.  The difference in radii of the circles is the
;; wheel base of the vehicle.
;;
;; 3. Knowing the lengths of the arcs and the difference in radii, we
;; can calculate the radius of the turn and the change in heading.
;; Now we can determine the change in location.

(defmethod move ((self two-wheeled-vehicle))
  ;; max-dist is the distance forward that the vehicle would move if
  ;; it was going at maximum speed.
  ;; x, y are the location of the vehicle.
  ;; theta is the angle the vehicle is facing.
  ;; wheel-base is the distance between the vehicle's two wheels.
  (if (and (not (null (last-move-time self)))
               (<= (world-time (world self)) (last-move-time self)))
    NIL
    (let* ((max-dist (/ (max-speed self) (world-ticks-per-second (world self))))
	   (dist-right (* (output (right-motor self)) max-dist))
	   (dist-left (* (output (left-motor self)) max-dist)))
      (let ((location (location self))
            (orientation (orientation self)))
        (let ((theta (2d-orientation-theta orientation))
              (x (2d-location-x location))
              (y (2d-location-y location))
	      (wheel-base (wheel-base self)))
          (let ((old-loc (old-location self)))
            (if (null old-loc)
              (setf (old-location self) (make-2d-location :x x :y y))
              (setf (2d-location-x old-loc) x
                    (2d-location-y old-loc) y)))
          (let ((dist-diff (- dist-right dist-left)))
            (let* ((delta-theta (/ dist-diff wheel-base))
                   (turn-radius (if (< (abs dist-diff) 0.0001)
                                  0.0
			          (- (/ dist-right delta-theta)
				     (/ wheel-base 2))))
                   (new-theta (+ theta delta-theta)))
              (let ((new-x (if (= turn-radius 0.0)
                             (+ x (* (cos theta) dist-left))
                             (+ x (* (- (sin new-theta) (sin theta))
				     turn-radius))))
                    (new-y (if (= turn-radius 0.0)
                             (+ y (* (sin theta) dist-left))
                             (- y (* (- (cos new-theta) (cos theta))
                                     turn-radius)))))
                ;; destructive modification to minimize consing
                (setf (2d-orientation-theta orientation) new-theta)
                (setf (2d-location-x location) new-x)
                (setf (2d-location-y location) new-y))))))
      (setf (last-move-time self) (world-time (world self)))
      (dolist (gun (guns self))
        (when (firing? gun)
          (setf (fired-flag gun) T)
          (let ((v (vehicle-hit-by-gun gun)))
            (when v
              (apply-gun-to-vehicle gun v)))))
      T)))


(defun vehicle-hit-by-gun (gun)
  (some #'(lambda (v)
            (and (gun-hit-vehicle-p gun v)
                 v))
        (let ((this-v (platform gun)))
          (remove this-v
                  (world-vehicles (world this-v))))))


(defun gun-hit-vehicle-p (gun target-vehicle)
  (let ((gun-loc (location gun))
        (target-loc (location target-vehicle))
        (theta (2d-orientation-theta (orientation gun))))
    (let* ((x0 (2d-location-x gun-loc))
           (y0 (2d-location-y gun-loc))
           (xr (cos theta))
           (yr (sin theta))
           (xc (2d-location-x target-loc))
           (yc (2d-location-y target-loc)))
      (let ((a (+ (square xr) (square yr)))
            (b (+ (* 2 (- x0 xc) xr) (* 2 (- y0 yc) yr)))
            (c (- (+ (square (- x0 xc)) (square (- y0 yc)))
                  (square (/ (vehicle-length target-vehicle) 2.0)))))
        (positive-root-p a b c (range gun))))))

(defun positive-root-p (a b c range)
  (if (zerop c)
    (> range (/ (- c) b) 0.0)
    (let ((disc (- (square b) (* 4 a c))))
      (if (minusp disc)
        NIL
        (let ((discrt (sqrt disc)))
          (or (> range (/ (+ (- b) discrt) (* 2 a)) 0.0)
              (> range (/ (- (- b) discrt) (* 2 a)) 0.0)))))))


;(defun gun-hit-vehicle-p (gun target-vehicle)
;  (let ((gun-loc (location gun))
;        (target-loc (location target-vehicle))
;        (theta (2d-orientation-theta (orientation gun)))
;        (range (range gun)))
;    (let ((ax (2d-location-x gun-loc))
;          (ay (2d-location-y gun-loc))
;          (vx (* range (cos theta)))
;          (vy (* range (sin theta)))
;          (cx (2d-location-x target-loc))
;          (cy (2d-location-y target-loc)))
;      (let ((r (/ (- (* (- ay cy) vy)
;                     (* (- ax cx) vx))
;                  range)))
;        (< 0 (* r range) (/ (vehicle-length target-vehicle) 2.0))))))


(defclass gun (neurode)
  ((range :accessor range :initarg :range :initform 3)
   (fired-flag :accessor fired-flag :initform NIL)
   (relative-orientation :accessor relative-orientation :initarg :relative-orientation
                         :initform (2d-orientation 0))
   (relative-location :accessor relative-location :initarg :relative-location
                      :initform (2d-location 0 0))
   (platform :accessor platform :initarg :platform :initform nil)))
   

(defmethod firing? ((self gun))
  (> (output self) 0))


(defmethod orientation ((self gun))
  (add-orientation (orientation (platform self))
		   (relative-orientation self)))

(defmethod location ((self gun))
  (add-location (location (platform self))
                (relative-location self)))



(defmethod apply-gun-to-vehicle ((self gun) vehicle)
;  (format T "~&* ~30S -> ~S  " self vehicle)
  (damage-node (random-node vehicle) vehicle)
  (with-slots (r g b) (color vehicle)
    (setf r (* r .9) g (* g .9) b (* b .9))))



(defstruct effect
  name
  type
  prob
  proc)


(defun damage-node (node vehicle)
  (let ((effects '())
        (world (world vehicle)))
    (labels ((defeffect (name type prob-factor proc)
               (setf effects (acons name (make-effect :name name
                                                      :type type
                                                      :prob prob-factor
                                                      :proc proc)
                                    effects)))
             (vrand (n) (vrandom n world))
	     (applicable-effects (node)
	       (remove-if #'(lambda (e)
			      (not (typep node (effect-type (cdr e)))))
			  effects))
	     (apply-effect (effect node)
               (if (< (/ (vrand 100) 100) (effect-prob (cdr effect)))
                 (progn 
;                   (format T "Applying effect ~20S to ~S."
;                           (effect-name (cdr effect)) node)
                   (funcall (effect-proc (cdr effect)) node)
                   T)
                 NIL)))

      (defeffect :REMOVE-INPUT 'node-with-inputs .9
	#'(lambda (node)
	    (when (inputs node)
	      (setf (inputs node)
		    (remove (elt (inputs node)
                                 (vrand (length (inputs node))))
			    (inputs node))))))

      (defeffect :CHANGE-THRESHOLD 'neurode .9
	#'(lambda (node)
	    (setf (threshold node)
		  (+ (threshold node) (- (vrand 3) 1)))))

      #+(or :mcl :allegro)
      (defeffect :RANDOM-OUTPUT T .01
	#'(lambda (node)
            (if (typep node 'extremely-random-node-mixin)
              (setf (average node) 0)
	      (push-mixin 'extremely-random-node-mixin node))))

      (defeffect :NEW-INPUT 'node-with-inputs .5
        #'(lambda (node)
            (let ((other-node (random-node vehicle)))
              (unless (null other-node)
                (setf (inputs node)
                      (cons other-node (inputs node)))))))

      (defeffect :DESTROY-NODE T .2
        #'(lambda (node)
            (dolist (n (nodes vehicle))
              (when (and (typep n 'node-with-inputs)
                         (member node (inputs n) :test #'eq))
                (setf (inputs n) (remove node (inputs n)))))
            (unless (typep node 'extremely-random-node-mixin)
              (push-mixin 'extremely-random-node-mixin node))
            (setf (average node) 0)))

      (unless (null node)
        (dotimes (i 5)
          (let ((applicable-effects (applicable-effects node)))
            (unless (null applicable-effects)
	      (when (apply-effect (elt applicable-effects
			               (vrand (length applicable-effects)))
		                  node)
                (return-from damage-node)))))))))


(defun random-node (vehicle)
  (let ((nodes (nodes vehicle)))
    (and nodes
         (cdr (elt nodes (vrandom (length nodes) (world vehicle)))))))

#+(or :allegro :mcl)
(defclass extremely-random-node-mixin ()
  ((world :accessor world :initform nil)
   (average :accessor average :initform 0.5)))

#+(or :allegro :mcl)
(defmethod output ((self extremely-random-node-mixin))
  (if (< (/ (vrandom 100 (world self)) 100) (average self))
    1
    0))



;; The real reason I'm doing this is because I just saw it on Usenet.
;; (See http://x3.dejanews.com/getdoc.xp?AN=365608470.4)

#+allegro
(use-package "CLOS")

#+allegro
(defun make-new-class (name superclasses)
  (ensure-class name :direct-superclasses superclasses))


#+mcl
(defun make-new-class (name superclasses)
  (ccl::%defclass name superclasses nil nil nil nil 'nil))

#+(or :mcl :allegro)
(defun push-mixin (mixin object)
  (let* ((name (concatenate 'string
			    (symbol-name mixin) "+" (symbol-name (type-of object))))
	 (symbol (intern name #.*package*))
	 (class (or (find-class symbol nil)
		    (make-new-class symbol
                                    (list mixin (class-of object))))))
    (change-class object class)))






;; --------------------
;; Lamp
;; 
;; A non-mobile platform.
;; --------------------

(defclass lamp (platform)
  ()
  (:default-initargs :color (rgb->color 0.9 0.9 1)))


(defmethod print-object ((self lamp) stream)
  (print-unreadable-object (self stream :type T :identity T)
    (format stream "[(~,2F, ~,2F) Brightness: ~,2F]"
            (if (location self)
              (2d-location-x (location self))
              0)
            (if (location self)
              (2d-location-y (location self))
              0)
            (if (world self)
              (brightness self)
              nil))))



(defmethod brightness ((self lamp))
  (let ((radiators (remove-if-not
                    #'(lambda (r)
                        (eq (platform r) self))
                    (world-radiators (world self)))))
    (sum-node-values #'output radiators)))






(defun fill-radiation-isa-table (table types)
  (clrhash table)
  (dolist (type types)
    (setf (gethash (first type) table)
	  (remove-duplicates
	   (append (second type)
		   (reduce #'append
			   (mapcar #'(lambda (ty)
				       (gethash ty table))
				   (second type)))))))
  (values))

(defun radiation-isa? (world abst-rad-type spec-rad-type)
  (or (eq abst-rad-type spec-rad-type)
      (member abst-rad-type (gethash spec-rad-type (world-radiation-hierarchy world))
	      :test #'eq)))
    

(defun radiators-of-type (world rad-type)
  (remove-if-not #'(lambda (r)
                     (radiation-isa? world rad-type (radiation-type r)))
                 (world-radiators world)))
