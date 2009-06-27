(define-radiation-types
  (light (electromagnetic))
  (visible-light (light))
  (infrared (light))
  (low-infrared (infrared))
  (high-infrared (infrared)))


(define-lamp lamp-1
  (:position 0 -1.5)
  (:color 1.0 1.0 0.0)
  (:sensor s1
           (:radiation-type infrared)
           (:non-directional? T)
           (:sensitivity 10.0))
  (:radiator r1
             (:radiation-type visible-light)
             (:decay-factor 0.05)
             (:brightness 2.0))
  (:brain
   (n1 (:inputs ())
       (:threshold 0)
       (:inhibitors (s1)))
   (r1 (:inputs (n1)))))

(define-lamp lamp-2
  (:position 1.5 1.5)
  (:color 1.0 1.0 0.0)
  (:sensor s1
           (:radiation-type infrared)
           (:non-directional? T)
           (:sensitivity 10.0))
  (:radiator r1
             (:radiation-type visible-light)
             (:brightness 2.0)
             (:decay-factor 0.05))
  (:brain
   (n1 (:inputs ())
       (:threshold 0)
       (:inhibitors (s1)))
   (r1 (:inputs (n1)))))

(define-lamp lamp-3
  (:position -1.5 1.5)
  (:color 1.0 1.0 0.0)
  (:sensor s1
           (:radiation-type infrared)
           (:non-directional? T)
           (:sensitivity 10.0))
  (:radiator r1
             (:radiation-type visible-light)
             (:brightness 2.0)
             (:decay-factor 0.05))
  (:brain
   (n1 (:inputs ())
       (:threshold 0)
       (:inhibitors (s1)))
   (r1 (:inputs (n1)))))


(define-vehicle simple-light-seeker-1
  (:position 3.0 3.0)
  (:orientation 180)
  (:max-speed 15)
  (:color 0 1.0 0)
  (:sensor s1
           (:orientation -30)
           (:radiation-type visible-light)
           (:sensitivity 3.0))
  (:sensor s2
           (:orientation 30)
           (:radiation-type visible-light)
           (:sensitivity 3.0))
  (:motor m1
          (:position right)
          (:decay-factor 0.18))
  (:motor m2
          (:position left)
          (:decay-factor 0.18))
  (:radiator r1
             (:radiation-type low-infrared)
             (:decay-factor 1.0))
  (:brain
   (r1 (:inputs (m1 m2)))
   (m1 (:inputs (s2)))
   (m2 (:inputs (s1)))))

(define-vehicle simple-light-seeker-2
  (:position 2.0 3.0)
  (:orientation 180)
  (:max-speed 15)
  (:color 1.0 0 0)
  (:sensor s1
           (:orientation -30)
           (:radiation-type visible-light)
           (:sensitivity 3.0))
  (:sensor s2
           (:orientation 30)
           (:radiation-type visible-light)
           (:sensitivity 3.0))
  (:motor m1
          (:position right)
          (:decay-factor 0.18))
  (:motor m2
          (:position left)
          (:decay-factor 0.18))
  (:radiator r1
             (:radiation-type low-infrared)
             (:decay-factor 1.0))
  (:brain
   (r1 (:inputs (m1 m2)))
   (m1 (:inputs (s2)))
   (m2 (:inputs (s1)))))

