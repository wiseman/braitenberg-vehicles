<html>
<head>
<title>Braitenberg Vehicle Simulator: World Definition 3</title>
</head>

<body bgcolor="#FFFFFF" text="#000000">

<h2>Definitions</h2>

<hr>

<pre>

<a name="radiation"</a>
(define-radiation-types
  (light (electromagnetic))
  (visible-light (light))
  (infrared (light))
  (low-infrared (infrared))
  (high-infrared (infrared)))


(define-lamp <def>lamp-1</def>
  (:position -2.0 1.0)
  (:color 1.0 1.0 0.0)
  (:sensor s1
           (:radiation-type infrared)
           (:non-directional? T)
           (:sensitivity 2.0))
  (:radiator r1
             (:radiation-type visible-light)
             (:decay-factor 0.05)
             (:brightness 2.0))
  (:brain
   (n1 (:inputs ())
       (:threshold 0)
       (:inhibitors (s1)))
   (r1 (:inputs (n1)))))

(define-lamp <def>lamp-2</def>
  (:position -4.0 -4.0)
  (:color 1.0 1.0 0.0)
  (:sensor s1
           (:radiation-type infrared)
           (:non-directional? T)
           (:sensitivity 10.0))
  (:radiator r1
             (:radiation-type visible-light)
             (:decay-factor 0.01)
             (:brightness 8.0))
  (:brain
   (n1 (:inputs ())
       (:threshold 0)
       (:inhibitors (s1)))
   (r1 (:inputs (n1)))))

(define-lamp <def>lamp-3</def>
  (:position 4.0 0.0)
  (:color 1.0 1.0 0.0)
  (:radiator r1
             (:radiation-type visible-light)
             (:decay-factor 0.05)))

(define-lamp <def>lamp-4</def>
  (:position 4.0 -4.0)
  (:color 1.0 1.0 0.0)
  (:radiator r1
             (:radiation-type visible-light)
             (:decay-factor 0.05)))

(define-vehicle <def>simple-light-seeker</def>
  (:position 5.0 5.0)
  (:orientation 0)
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
          (:decay-factor 0.2))
  (:motor m2
          (:position left)
          (:decay-factor 0.2))
  (:radiator r1
             (:radiation-type low-infrared)
             (:decay-factor 1.0))
  (:brain
   (r1 (:inputs (m1 m2)))
   (m1 (:inputs (s2)))
   (m2 (:inputs (s1)))))

(define-vehicle <def>simple-light-seeker-2</def>
  (:position -5.0 -2.0)
  (:orientation 0)
  (:max-speed 10)
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
          (:decay-factor 0.2))
  (:motor m2
          (:position left)
          (:decay-factor 0.2))
  (:radiator r1
             (:radiation-type low-infrared)
             (:decay-factor 1.0))
  (:brain
   (r1 (:inputs (m1 m2)))
   (m1 (:inputs (s2)))
   (m2 (:inputs (s1)))))


(define-vehicle <def>heat-seeker</def>
  (:position -7.0 6.0)
  (:orientation 0.0)
  (:max-speed 15.0)
  (:color 1.0 0 0)
  (:sensor s1
           (:orientation -40)
           (:radiation-type low-infrared)
           (:sensitivity 10.0))
  (:sensor s2
           (:orientation 40)
           (:radiation-type low-infrared)
           (:sensitivity 10.0))
  (:motor m1
          (:position right)
          (:decay-factor 0.2))
  (:motor m2
          (:position left)
          (:decay-factor 0.2))
  (:radiator r1
             (:radiation-type high-infrared)
             (:decay-factor 1))
  (:brain
   (m2 (:inputs (s1)))
   (m1 (:inputs (s2)))
   (r1 (:inputs (m1 m2)))))

(define-vehicle <def>heat-seeker-2</def>
  (:position 4.0 -7.0)
  (:orientation 0.0)
  (:max-speed 8.0)
  (:color 1.0 0 0)
  (:sensor s1
           (:orientation -40)
           (:radiation-type low-infrared)
           (:sensitivity 10.0))
  (:sensor s2
           (:orientation 40)
           (:radiation-type low-infrared)
           (:sensitivity 10.0))
  (:motor m1
          (:position right)
          (:decay-factor 0.2))
  (:motor m2
          (:position left)
          (:decay-factor 0.2))
  (:radiator r1
             (:radiation-type high-infrared)
             (:decay-factor 1))
  (:brain
   (m2 (:inputs (s1)))
   (m1 (:inputs (s2)))
   (r1 (:inputs (m1 m2)))))

(define-vehicle <def>non-linear-shy-seeker</def>
  (:position -2.0 4.0)
  (:orientation 294)
  (:color 0 0 1.0)

  (:sensor s1
           (:orientation 30)
           (:radiation-type visible-light)
           (:sensitivity 4.0))
  (:sensor s2
           (:orientation -30)
           (:radiation-type visible-light)
           (:sensitivity 4.0))

  (:sensor s3
           (:orientation 50)
           (:radiation-type high-infrared)
           (:sensitivity 2.0))
  (:sensor s4
           (:orientation -50)
           (:radiation-type high-infrared)
           (:sensitivity 2.0))


  (:motor m1
          (:position left)
          (:decay-factor 0.1))
  (:motor m2
          (:position right)
          (:decay-factor 0.1))

  (:radiator r1
             (:radiation-type low-infrared)
             (:decay-factor 1.0))

  (:brain

   <com>;; Light-seeking behavior</com>
   (n1 (:inputs (s1))
       (:inhibitors (n2))
       (:threshold 1))
   (n1.5 (:inputs (s1))
         (:threshold 1))
   (n2 (:inputs (n1.5))
       (:threshold 1))
   (m2 (:inputs (n1 s4)))

   <com>;; Predator avoidance behavior</com>
   (n3 (:inputs (s2))
       (:threshold 1))
   (n3.5 (:inputs (s2))
         (:threshold 1))
   (n4 (:inputs (n3.5))
       (:inhibitors (n3))
       (:threshold 1))
   (m1 (:inputs (n4 s3)))
   
   (r1 (:inputs (m1 m2)))))

(define-vehicle <def>non-linear-shy-seeker-2</def>
  (:position 6.0 4.0)
  (:orientation 294)
  (:color 0 0 1.0)

  (:sensor s1
           (:orientation 30)
           (:radiation-type visible-light)
           (:sensitivity 4.0))
  (:sensor s2
           (:orientation -30)
           (:radiation-type visible-light)
           (:sensitivity 4.0))

  (:sensor s3
           (:orientation 50)
           (:radiation-type high-infrared)
           (:sensitivity 2.0))
  (:sensor s4
           (:orientation -50)
           (:radiation-type high-infrared)
           (:sensitivity 2.0))


  (:motor m1
          (:position left)
          (:decay-factor 0.1))
  (:motor m2
          (:position right)
          (:decay-factor 0.1))

  (:radiator r1
             (:radiation-type low-infrared)
             (:decay-factor 1.0))

  (:brain

   <com>;; Light-seeking behavior</com>
   (n1 (:inputs (s1))
       (:inhibitors (n2))
       (:threshold 1))
   (n1.5 (:inputs (s1))
         (:threshold 1))
   (n2 (:inputs (n1.5))
       (:threshold 1))
   (m2 (:inputs (n1 s4)))

   <com>;; Predator avoidance behavior</com>
   (n3 (:inputs (s2))
       (:threshold 1))
   (n3.5 (:inputs (s2))
         (:threshold 1))
   (n4 (:inputs (n3.5))
       (:inhibitors (n3))
       (:threshold 1))
   (m1 (:inputs (n4 s3)))
   
   (r1 (:inputs (m1 m2)))))




