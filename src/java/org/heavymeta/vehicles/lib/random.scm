;; General Utilities

(define (vector-add v e) (invoke v 'addElement e))


;; Vehicle Utilities

(define (attach-node a b)
  (let ((current-inputs (peek b 'current_inputs))
        (original-inputs (peek b 'original_inputs)))
    (jvector-add current-inputs a)
    (jvector-add original-inputs a)))

(define (make-radiation-type type)
  type)
  
(define (make-radiator world platform type)
  (let ((r (new 'org.heavymeta.vehicles.Radiator)))
    (poke r 'radiation_type (make-radiation-type type))
    (poke r 'platform platform)
    (jvector-add (peek world 'radiators) r)
    (poke r 'world world)
    r))

(define (make-sensor world platform type theta)
  (let ((s (new 'org.heavymeta.vehicles.Sensor)))
    (poke s 'radiation_type (make-radiation-type type))
    (poke s 'platform platform)
    (poke (peek s 'relative_orientation) 'theta (* (/ 3.1415 180) theta))
    (jvector-add (peek world 'sensors) s)
    (poke s 'world world)
    (poke s 'sensitivity 3.0)
    s))

(define (make-motor world)
  (let ((m (new 'org.heavymeta.vehicles.Motor)))
    (poke m 'world world)
    (poke m 'decay_factor 0.00001)
    m))


(define (make-simple-vehicle world x y color motor-rad-type seek-rad-type flee-rad-type)
  (let* ((v (new 'org.heavymeta.vehicles.TwoWheeledVehicle))
         (left-motor (make-motor world))
         (right-motor (make-motor world))
         (left-sensor (make-sensor world v seek-rad-type 30))
         (right-sensor (make-sensor world v seek-rad-type -30)))
    (when flee-rad-type
      (let ((left-flee-sensor (make-sensor world v flee-rad-type 30))
	    (right-flee-sensor (make-sensor world v flee-rad-type 30)))
	(let ((s (* 1.25 (peek left-flee-sensor 'sensitivity))))
	  (poke left-flee-sensor 'sensitivity s)
	  (poke right-flee-sensor 'sensitivity s))
	(attach-node left-flee-sensor left-motor)
	(attach-node right-flee-sensor right-motor)))
    (attach-node right-sensor left-motor)
    (attach-node left-sensor right-motor)
    (poke v 'left_motor left-motor)
    (poke v 'right_motor right-motor)
    (when motor-rad-type
      (let ((r (make-radiator world v motor-rad-type)))
	(attach-node left-motor r)
	(attach-node right-motor r)))
    (let ((loc (peek v 'm_location)))
      (poke loc 'x x)
      (poke loc 'y y))
    (jvector-add (peek world 'vehicles) v)
    (poke v 'world world)
    (poke v 'color color)
    (poke (peek v 'm_orientation) 'theta
	  (random-range (* 2 pi)))
    v))

(defun make-predator (world x y color)
  (make-simple-vehicle world x y color "low-infrared" "high-infrared" #f))

(defun make-dumb-prey (world x y color)
  (make-simple-vehicle world x y color "high-infrared" "light" #f))

(defun make-alert-prey (world x y color)
  (make-simple-vehicle world x y color "high-infrared" "light" "low-infrared"))


(define (make-lamp world x y r-type)
  (let ((l (new 'org.heavymeta.vehicles.Lamp)))
    (let ((loc (peek l 'm_location)))
      (poke loc 'x x)
      (poke loc 'y y))
    (let ((r (make-radiator world l r-type))))
    (poke l 'world world)
    (jvector-add (peek world 'lamps) l)
    l))


(defun random-color ()
  (new 'java.awt.Color (random 256) (random 256) (random 256)))

(defun random-coord ()
  (- 7 (random-range 14)))

(defun random-speed ()
  (+ 20 (/ (random 20) (+ (random 3) 1))))


(defun make-random-vehicle (world type)
  (let ((fn (cond ((eq? type 'dumb)
		   make-dumb-prey)
		  ((eq? type 'alert)
		   make-alert-prey)
		  ((eq? type 'predator)
		   make-predator))))
    (let ((v (funcall fn world (random-coord) (random-coord)
		      (random-color))))
      (poke v 'max_speed (random-speed))
      v)))

(defun make-random-lamp (world)
  (let ((l (make-lamp world
		      (random-coord)
		      (random-coord)
		      "light")))
    l))

(defun random-world (n-lamps n-dumb n-alert n-pred)
  (let ((world (new 'org.heavymeta.vehicles.World)))
    (dotimes (i n-lamps)
      (make-random-lamp world))
    (dotimes (i n-dumb)
      (make-random-vehicle world 'dumb))
    (dotimes (i n-alert)
      (make-random-vehicle world 'alert))
    (dotimes (i n-pred)
      (make-random-vehicle world 'predator))
    (invoke (peek world 'radiation_hierarchy)
	    'addChild
	    "light" "visible-light")
    world))



