;;;-*- Mode: Lisp; Package: VEHICLES -*-
;;----------------------------------------------------------------------
;; 
;; Copyright:   Copyright (c) 1999 John Wiseman
;; File:        world.lisp
;; Created:     2 December 1997
;; Author:      John Wiseman (wiseman@neodesic.com)
;; 
;; Description: Braitenberg vehicle simulator world
;; 
;; Changes:     
;; 
;;----------------------------------------------------------------------

(in-package "VEHICLES")

(defparameter *world* nil)


(defstruct vrandom-state
  (a 4096)
  (c 150889)
  (m 714025)
  (seed 0))


(defstruct world
  (time 0)
  (ticks-per-second 100)
  vehicles
  radiators
  lamps
  sensors
  (radiation-hierarchy (make-hash-table :test #'eq))
  (rand-state (make-vrandom-state)))


(defmethod print-object ((self world) stream)
  (if *print-readably*
    (call-next-method)
    (print-unreadable-object (self stream :type T :identity T)
      (format stream "time: ~S, TPS: ~s"
              (world-time self)
              (world-ticks-per-second self)))))


(defun step-world (world)
  (dolist (v (world-vehicles world))
    (move v))
  (incf (world-time world))
  world)


(defun world-stats (world)
  (let ((platforms 0)
        (nodes 0))
  (format T "~&World ~S has ~&  ~S vehicles, with ~S nodes;~&  ~S lamps, with ~S nodes;"
          world
          (let ((p (length (world-vehicles world))))
            (incf platforms p)
            p)
          (let ((n (reduce #'+ (mapcar #'length
                                       (mapcar #'nodes
                                               (world-vehicles world))))))
            (incf nodes n)
            n)
          (let ((p (length (world-lamps world))))
            (incf platforms p)
            p)
          (let ((n (reduce #'+ (mapcar #'length
                                       (mapcar #'nodes
                                               (world-lamps world))))))
            (incf nodes n)
            n))
  (format T "~&  ~S platforms total, with ~S nodes total."
          platforms nodes)))



(defun vrandom (n world)
  (let* ((state (world-rand-state world))
         (m (vrandom-state-m state)))
    (let ((new-seed (mod (+ (* (vrandom-state-seed state)
                               (vrandom-state-a state))
                            (vrandom-state-c state))
                         m)))
      (setf (vrandom-state-seed state) new-seed)
      (values (floor (* n (/ (float new-seed) (float m))))))))


          
