;;; I don't care what the extension is, this is really -*-Lisp-*-

;; *lamp-size* .3
;; *window-scale* 15




(vehicle vehicle-1
  (:position 0.0 0.0)
  (:orientation 0)
  (:color 0 1.0 0)
  (:gun g1
        (:range 1.0))
  (:gun g2
        (:range 3.0)
        (:orientation 30))
  (:motor m1
          (:position left))
  (:motor m2
          (:position right))
  (:brain
   (g1 (:threshold 0))
   (g2 (:threshold 0))))


(vehicle vehicle-4
  (:position 1.0 1.0)
  (:orientation 90)
  (:color 0 1.0 0)
  (:motor m2
          (:position right))
  (:motor m1
          (:position left)))


(vehicle vehicle-3
  (:position 1.0 0.0)
  (:orientation 0)
  (:color 0 1.0 0)
  (:motor m2
          (:position right))
  (:motor m1
          (:position left)))


(vehicle vehicle-2
  (:position -1.0 0.0)
  (:orientation 0)
  (:color 0 1.0 0)
  (:motor m2
          (:position right))
  (:motor m1
          (:position left)))


#|
(vehicle vehicle-4
  (:position 0.1 1.0)
  (:orientation 0)
  (:color 0 1.0 0)
  (:motor m2
          (:position right))
  (:motor m1
          (:position left)))
|#


#|
(vehicle vehicle-5
  (:position 1.0 1.0)
  (:orientation 0)
  (:color 0 1.0 0)
  (:motor m2
          (:position right))
  (:motor m1
          (:position left)))
|#

