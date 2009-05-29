;;; -------------------------------------------------------------------
;;; -------------------------------------------------------------------
;;; Common Graphics: Graphics the right way!
;;;
;;; Copyright (c) R. James Firby 1989, 1991
;;; -------------------------------------------------------------------
;;; -------------------------------------------------------------------

(in-package "CG")

; --------------------------------------------------------------------
; Primitives
; ----------

(defconstant *tg-closeness-constant* 2)

(defclass primitive
  ()
  ((name :accessor primitive-name :initarg :name)))

(defun primitive? (x) (typep x 'primitive))

; --- Points ---

(defclass cg-point
  (primitive)
  ((x     :accessor cg-point-x     :initarg :x)
   (y     :accessor cg-point-y     :initarg :y)
   (color :accessor cg-point-color :initarg :color)))

(defmethod draw-object ((self cg-point) x0 y0 scale)
  (i.set-cg-current-transform x0 y0 scale)
  (draw-point (cg-point-x self) (cg-point-y self) (cg-point-color self)))

(defmethod extent ((self cg-point))
  (values (cg-point-x self) (cg-point-y self) 
          (cg-point-x self) (cg-point-y self)))

(defmethod hold-point? ((self cg-point) x y)
  (and (<= (abs (- x (cg-point-x self))) *tg-closeness-constant*)
       (<= (abs (- y (cg-point-y self))) *tg-closeness-constant*)))

(defmethod obscures-area? ((self cg-point) x1 y1 x2 y2)
  (and (= x1 (cg-point-x self)) (= y1 (cg-point-y self))
       (= x2 (cg-point-y self)) (= y2 (cg-point-y self))))

(defun point (x y &key (color color/foreground))
  (assert (numberp x)     (x))
  (assert (numberp y)     (y))
  (assert (color?  color) (color))
  (let ( (primitive (make-instance 'cg-point 
                                   :name  :POINT
                                   :x     x
                                   :y     y
                                   :color color)) )
    (insert! (make-empty-picture) primitive)))

; --- Segments ---

(defclass cg-segment
  (primitive)
  ((x1    :accessor cg-segment-x1    :initarg :x1)
   (y1    :accessor cg-segment-y1    :initarg :y1)
   (x2    :accessor cg-segment-x2    :initarg :x2)
   (y2    :accessor cg-segment-y2    :initarg :y2)
   (color :accessor cg-segment-color :initarg :color)))

(defmethod draw-object ((self cg-segment) x0 y0 scale)
  (i.set-cg-current-transform x0 y0 scale)
  (draw-segment (cg-segment-x1 self) (cg-segment-y1 self)
                (cg-segment-x2 self) (cg-segment-y2 self)
                (cg-segment-color self)))

(defmethod extent ((self cg-segment))
  (let ( (x1 (cg-segment-x1 self)) (y1 (cg-segment-y1 self))
         (x2 (cg-segment-x2 self)) (y2 (cg-segment-y2 self)) )
    (values (min x1 x2) (min y1 y2)
            (max x1 x2) (max y1 y2))))

(defmethod holds-point? ((self cg-segment) x y)
  (and (point-along-line? x y 
                          (cg-segment-x1 self) (cg-segment-y1 self)
                          (cg-segment-x2 self) (cg-segment-y2 self))
       (<= (abs (point-line-distance x y 
                                     (cg-segment-x1 self) (cg-segment-y1 self)
                                     (cg-segment-x2 self) (cg-segment-y2 self)))
           *tg-closeness-constant*)))

(defmethod obscures-area? ((self cg-segment) x1 y1 x2 y2)
  (declare (ignore self x1 y1 x2 y2))
  NIL)

(defun segment (x1 y1 x2 y2 &key (color color/foreground))
  (assert (numberp x1)    (x1))
  (assert (numberp y1)    (y1))
  (assert (numberp x2)    (x2))
  (assert (numberp y2)    (y2))
  (assert (color?  color) (color))
  (let ( (primitive (make-instance 'cg-segment
                                   :name  :SEGMENT
                                   :x1    x1
                                   :y1    y1
                                   :x2    x2
                                   :y2    y2
                                   :color color)) )
    (insert! (make-empty-picture) primitive)))
  
; --- Rectangles ---

(defclass cg-rectangle
  (primitive)
  ((x1      :accessor cg-rectangle-x1      :initarg :x1)
   (y1      :accessor cg-rectangle-y1      :initarg :y1)
   (x2      :accessor cg-rectangle-x2      :initarg :x2)
   (y2      :accessor cg-rectangle-y2      :initarg :y2)
   (color   :accessor cg-rectangle-color   :initarg :color)
   (texture :accessor cg-rectangle-texture :initarg :texture)
   (filled? :accessor cg-rectangle-filled? :initarg :filled?)))

(defmethod draw-object ((self cg-rectangle) x0 y0 scale)
  (i.set-cg-current-transform x0 y0 scale)
  (draw-rectangle (cg-rectangle-x1 self) (cg-rectangle-y1 self)
                  (cg-rectangle-x2 self) (cg-rectangle-y2 self)
                  (cg-rectangle-filled? self)
                  (cg-rectangle-color   self)
                  (cg-rectangle-texture self)))

(defmethod extent ((self cg-rectangle))
  (values (cg-rectangle-x1 self) (cg-rectangle-y1 self)
          (cg-rectangle-x2 self) (cg-rectangle-y2 self)))

(defmethod holds-point? ((self cg-rectangle) x y)
  (and (<= (cg-rectangle-x1 self) x) (<= x (cg-rectangle-x2 self))
       (<= (cg-rectangle-y1 self) y) (<= y (cg-rectangle-y2 self))))

(defmethod obscures-area? ((self cg-rectangle) x1 y1 x2 y2)
  (and (cg-rectangle-filled? self)
       (<= (cg-rectangle-x1 self) x1) (<= (cg-rectangle-y1 self) y1)
       (>= (cg-rectangle-x2 self) x2) (>= (cg-rectangle-y2 self) y2)))

(defun rectangle (x1 y1 x2 y2 &key (color  color/foreground)
                                   (filled NIL)
                                   (fill   texture/100% fillp))
  (assert (numberp  x1)    (x1))
  (assert (numberp  y1)    (y1))
  (assert (numberp  x2)    (x2))
  (assert (numberp  y2)    (y2))
  (assert (color?   color) (color))
  (assert (texture? fill)  (fill))
  (let ( (primitive (make-instance 'cg-rectangle
                                   :name    :RECTANGLE
                                   :x1      (min x1 x2)
                                   :y1      (min y1 y2)
                                   :x2      (max x1 x2)
                                   :y2      (max y1 y2)
                                   :color   color
                                   :filled? (or filled fillp)
                                   :texture fill)) )
    (insert! (make-empty-picture) primitive)))

; --- Ovals ---

(defclass cg-oval
  (primitive)
  ((x1      :accessor cg-oval-x1      :initarg :x1)
   (y1      :accessor cg-oval-y1      :initarg :y1)
   (x2      :accessor cg-oval-x2      :initarg :x2)
   (y2      :accessor cg-oval-y2      :initarg :y2)
   (color   :accessor cg-oval-color   :initarg :color)
   (texture :accessor cg-oval-texture :initarg :texture)
   (filled? :accessor cg-oval-filled? :initarg :filled?)
   (a**2    :accessor cg-oval-a**2    :initarg :a**2)
   (b**2    :accessor cg-oval-b**2    :initarg :b**2)
   (cx      :accessor cg-oval-cx      :initarg :cx)
   (cy      :accessor cg-oval-cy      :initarg :cy)))

(defmethod draw-object ((self cg-oval) x0 y0 scale)
  (i.set-cg-current-transform x0 y0 scale)
  (draw-oval (cg-oval-x1 self) (cg-oval-y1 self)
             (cg-oval-x2 self) (cg-oval-y2 self)
             (cg-oval-filled? self)
             (cg-oval-color   self)
             (cg-oval-texture self)))

(defmethod extent ((self cg-oval))
  (values (cg-oval-x1 self) (cg-oval-y1 self)
          (cg-oval-x2 self) (cg-oval-y2 self)))

(defmethod holds-point? ((self cg-oval) x y)
  (let ( (x (- x (cg-oval-cx self))) (y (- y (cg-oval-cy self))) )
    (<= (+ (/ (* x x) (cg-oval-a**2 self)) 
           (/ (* y y) (cg-oval-b**2 self)))
        1.0)))

(defmethod obscures-area? ((self cg-oval) x1 y1 x2 y2)
  (and (cg-oval-filled? self)
       (holds-point? self x1 y1)
       (holds-point? self x2 y2)))

(defun oval (x1 y1 x2 y2 &key (color  color/foreground)
                              (filled NIL)
                              (fill   texture/100% fillp)
                              (name   :oval))
  (assert (numberp  x1)    (x1))
  (assert (numberp  y1)    (y1))
  (assert (numberp  x2)    (x2))
  (assert (numberp  y2)    (y2))
  (assert (color?   color) (color))
  (assert (texture? fill)  (fill))
  (let ( (a (/ (- x2 x1) 2))  (b (/ (- y2 y1) 2)) )
    (let ( (primitive (make-instance 'cg-oval
                                     :name    name
                                     :x1      (min x1 x2)
                                     :y1      (min y1 y2)
                                     :x2      (max x1 x2)
                                     :y2      (max y1 y2)
                                     :color   color
                                     :filled? (or filled fillp)
                                     :texture fill
                                     :a**2    (* a a)
                                     :b**2    (* b b)
                                     :cx      (/ (+ x2 x1) 2)
                                     :cy      (/ (+ y2 y1) 2))) )
      (insert! (make-empty-picture) primitive))))


(defun circle (x y r &rest options) 
  (apply #'oval (- x r) (- y r) (+ x r) (+ y r) :name :circle options))

; --- Polygons ---

(defclass cg-polygon
  (primitive)
  ((xs      :accessor cg-polygon-xs      :initarg :xs)
   (ys      :accessor cg-polygon-ys      :initarg :ys)
   (len     :accessor cg-polygon-len     :initarg :len)
   (color   :accessor cg-polygon-color   :initarg :color)
   (texture :accessor cg-polygon-texture :initarg :texture)
   (filled? :accessor cg-polygon-filled? :initarg :filled?)
   (x1      :accessor cg-polygon-x1      :initarg :x1)
   (y1      :accessor cg-polygon-y1      :initarg :y1)
   (x2      :accessor cg-polygon-x2      :initarg :x2)
   (y2      :accessor cg-polygon-y2      :initarg :y2)
   (x-far   :accessor cg-polygon-x-far   :initarg :x-far)
   (y-far   :accessor cg-polygon-y-far   :initarg :y-far)))

(defmethod draw-object ((self cg-polygon) x0 y0 scale)
  (i.set-cg-current-transform x0 y0 scale)
  (draw-polygon (cg-polygon-xs self) (cg-polygon-ys self)
                (cg-polygon-len  self)
                (cg-polygon-filled? self)
                (cg-polygon-color   self)
                (cg-polygon-texture self)))

(defmethod extent ((self cg-polygon))
  (values (cg-polygon-x1 self) (cg-polygon-y1 self)
          (cg-polygon-x2 self) (cg-polygon-y2 self)))

(defmethod holds-point? ((self cg-polygon) x y)
  (let ( (xs (cg-polygon-xs self))  (ys (cg-polygon-ys self))
         (len (cg-polygon-len self))
         (far-x (cg-polygon-x-far self)) (far-y (cg-polygon-y-far self)) )
    (do ( (index 0 (+ index 1))
          (crossings 0) )
        ((= index len) (oddp crossings))
      (let ( (x1 (if (= index 0) (aref xs (- len 1)) (aref xs (- index 1))))
             (y1 (if (= index 0) (aref ys (- len 1)) (aref ys (- index 1))))
             (x2 (aref xs index))
             (y2 (aref ys index))
             (x3 (if (= index (- len 1)) (aref xs 0) (aref xs (+ index 1))))
             (y3 (if (= index (- len 1)) (aref ys 0) (aref ys (+ index 1)))) )
        (multiple-value-bind (intersection xi yi)
                             (line-line-intersection x y far-x far-y x1 y1 x2 y2)
          (if (and intersection
                   (point-along-line? xi yi x1 y1 x2 y2)
                   (point-along-line? xi yi x y far-x far-y))
            (cond ((approximately= 0.0
                                   (point-line-distance x y x1 y1 x2 y2))
                   (incf crossings))
                  ((and (approximately= x1 xi)
                        (approximately= y1 yi))
                   nil)
                  ((and (approximately= x2 xi)
                        (approximately= y2 yi))
                   (let ( (d1 (point-line-distance x1 y1 x y far-x far-y))
                          (d2 (point-line-distance x3 y3 x y far-x far-y)) )
                     (if (or (and (> d1 0.0) (> d2 0.0))
                             (and (< d1 0.0) (< d2 0.0)))
                       nil
                       (incf crossings))))
                  (T
                   (incf crossings)))
            nil))))))

(defmethod obscures-area? ((self cg-polygon) x1 y1 x2 y2)
  (declare (ignore self x1 y1 x2 y2))
  NIL)

(defun polygon (coords &key (color  color/foreground)
                            (filled NIL)
                            (fill   texture/100% fillp))
  (assert (color?   color)             (color))
  (assert (texture? fill)              (fill))
  (assert (and (evenp (length coords))
               (>= (length coords) 6)) (coords))
  (let* ( (len (truncate (length coords) 2))
          (primitive (make-instance 'cg-polygon
                                    :name    :POLYGON
                                    :len     len
                                    :color   color
                                    :filled? (or filled fillp)
                                    :texture fill)) )
    (let ( (x1 (car coords))     (y1 (cadr coords))
           (x2 (car coords))     (y2 (cadr coords))
           (xs (make-array len)) (ys (make-array len)) )
      (do ( (i 0  (+ i 1))
            (coords coords (cddr coords)) )
          ((>= i len) (values))
        (setf (aref xs i) (car coords))
        (setf (aref ys i) (cadr coords))
        (setf x1 (min x1 (aref xs i)))
        (setf x2 (max x2 (aref xs i)))
        (setf y1 (min y1 (aref ys i)))
        (setf y2 (max y2 (aref ys i))))
      (setf (cg-polygon-xs    primitive) xs)
      (setf (cg-polygon-ys    primitive) ys)
      (setf (cg-polygon-x1    primitive) x1)
      (setf (cg-polygon-y1    primitive) y1)
      (setf (cg-polygon-x2    primitive) x2)
      (setf (cg-polygon-y2    primitive) y2)
      (setf (cg-polygon-x-far primitive) (+ 10000.0 x2))
      (setf (cg-polygon-y-far primitive) (+ 10000.0 y2)))
    (insert! (make-empty-picture) primitive)))

(defun regular-polygon (nsides radius angle &key (color  color/foreground)
                               (filled NIL)
                               (fill   texture/100% fillp))
  (assert (and (integerp nsides)
               (> nsides 3)) (nsides))
  (assert (numberp  radius)  (radius))
  (assert (numberp  angle)   (angle))
  (assert (color?   color)   (color))
  (assert (texture? fill)    (fill))
  (assert (>= nsides 3)      (nsides))
  (let ( (angle-inc (/ (* 2.0 pi) nsides))
         (primitive (make-instance 'cg-polygon
                                   :name    :POLYGON
                                   :len     nsides
                                   :color   color
                                   :filled? (or filled fillp)
                                   :texture fill)) )
    (let ( (x1 (* radius (cos angle)))
           (y1 (* radius (sin angle))) )
      (let ( (x2 x1) (y2 y1)
             (xs (make-array nsides))
             (ys (make-array nsides)) )
        (setf (aref xs 0) x1)
        (setf (aref ys 0) y1)
        (do ( (i 1  (+ i 1))
              (angle (+ angle angle-inc) (+ angle angle-inc)) )
            ((>= i nsides) (values))
          (setf (aref xs i) (* radius (cos angle)))
          (setf (aref ys i) (* radius (sin angle)))
          (setf x1 (min x1 (aref xs i)))
          (setf y1 (min y1 (aref ys i)))
          (setf x2 (max x2 (aref xs i)))
          (setf y2 (max y2 (aref ys i))))
        (setf (cg-polygon-xs    primitive) xs)
        (setf (cg-polygon-ys    primitive) ys)
        (setf (cg-polygon-x1    primitive) x1)
        (setf (cg-polygon-y1    primitive) y1)
        (setf (cg-polygon-x2    primitive) x2)
        (setf (cg-polygon-y2    primitive) y2)
        (setf (cg-polygon-x-far primitive) (+ 10000.0 x2))
        (setf (cg-polygon-y-far primitive) (+ 10000.0 y2))))
    (insert! (make-empty-picture) primitive)))

; --- Texts ---

(defclass cg-text
  (primitive)
  ((x      :accessor cg-text-x      :initarg :x)
   (y      :accessor cg-text-y      :initarg :y)
   (string :accessor cg-text-string :initarg :string)
   (font   :accessor cg-text-font   :initarg :font)
   (color  :accessor cg-text-color  :initarg :color)
   (x1     :accessor cg-text-x1     :initarg :x1)
   (y1     :accessor cg-text-y1     :initarg :y1)
   (x2     :accessor cg-text-x2     :initarg :x2)
   (y2     :accessor cg-text-y2     :initarg :y2)))

(defmethod draw-object ((self cg-text) x0 y0 scale)
  (i.set-cg-current-transform x0 y0 scale)
  (draw-text (cg-text-x self) (cg-text-y self)
             (cg-text-string self)
             (cg-text-font   self)
             (cg-text-color  self)))

(defmethod extent ((self cg-text))
  (values (cg-text-x1 self) (cg-text-y1 self)
          (cg-text-x2 self) (cg-text-y2 self)))

(defmethod holds-point?  ((self cg-text) x y)
  (and (<= (cg-text-x1 self) x) (<= x (cg-text-x2 self))
       (<= (cg-text-y1 self) y) (<= y (cg-text-y2 self))))

(defmethod obscures-area? ((self cg-text) x1 y1 x2 y2)
  (declare (ignore self x1 y1 x2 y2))
  NIL)

(defun text (x y string &rest options)
  (let ( (font  (member :font  options :test #'eq))
         (color (member :color options :test #'eq)) )
    (assert (numberp x)      (x))
    (assert (numberp y)      (y))
    (assert (stringp string) (string))
    (if (null color)
        (setf color color/foreground)
        (setf color (second color)))
    (assert (color? color) (color))
    (if (null font)
        (setf font font/standard)
        (setf font (second font)))
    (assert (font? font) (font))
    (let ( (primitive (make-instance 'cg-text
                                     :name   :TEXT
                                     :string string
                                     :font   font
                                     :color  color)) )
      (multiple-value-bind (x1 y1 x2 y2)
                           (i.text-extent string font)
        (cond ((member :left options :test #'eq)
               (setf (cg-text-x  primitive) (- x x1))
               (setf (cg-text-x1 primitive) x)
               (setf (cg-text-x2 primitive) (+ x (- x2 x1))))
              ((member :right options :test #'eq)
               (setf (cg-text-x  primitive) (- x x2))
               (setf (cg-text-x1 primitive) (- x (- x2 x1)))
               (setf (cg-text-x2 primitive) x))
              (T
               (let ( (half (truncate (- x2 x1) 2)) )
                 (setf (cg-text-x  primitive) (- (- x half) x1))
                 (setf (cg-text-x1 primitive) (- x half))
                 (setf (cg-text-x2 primitive) (+ x half)))))
        (cond ((member :top options :test #'eq)
               (setf (cg-text-y  primitive) (- y y2))
               (setf (cg-text-y1 primitive) (- y (- y2 y1)))
               (setf (cg-text-y2 primitive) y))
              ((member :bottom options :test #'eq)
               (setf (cg-text-y  primitive) (- y y1))
               (setf (cg-text-y1 primitive) y)
               (setf (cg-text-y2 primitive) (+ y (- y2 y1))))
              ((member :baseline options :test #'eq)
               (setf (cg-text-y  primitive) y)
               (setf (cg-text-y1 primitive) (+ y y1))
               (setf (cg-text-y2 primitive) (+ y y2)))
              (T
               (let ( (half (truncate (- y2 y1) 2)) )
                 (setf (cg-text-y  primitive) (- (- y half) y1))
                 (setf (cg-text-y1 primitive) (- y half))
                 (setf (cg-text-y2 primitive) (+ y half))))))
      (insert! (make-empty-picture) primitive))))

; --------------------------------------------------------------------

(defun approximately= (a b) (< (abs (- a b)) 0.00005))

(defun point-line-distance (x y x1 y1 x2 y2)
  (cond ((approximately= x1 x2) (- x2 x))
        ((approximately= y1 y2) (- y2 y))
        (T
         (let ( (m (/ (- y2 y1) (- x2 x1))) )
           (/ (- (- y y2) (* m (- x x2)))
              (sqrt (+ (* m m) 1.0)))))))

(defun line-line-intersection (x11 y11 x12 y12 x21 y21 x22 y22)
  (cond ((and (approximately= x11 x12)
              (approximately= x21 x22))
         (values NIL 0 0))
        ((approximately= x11 x12)
         (let ( (m2 (/ (- y22 y21) (- x22 x21))) )
           (let ( (b2 (- y22 (* m2 x22))) )
             (values T x11 (+ (* m2 x11) b2)))))
        ((approximately= x21 x22)
         (let ( (m1 (/ (- y12 y11) (- x12 x11))) )
           (let ( (b1 (- y11 (* m1 x11))) )
             (values T x22 (+ (* m1 x22) b1)))))
        (T
         (let ( (m1 (/ (- y12 y11) (- x12 x11)))
                (m2 (/ (- y22 y21) (- x22 x21))) )
           (cond ((approximately= m1 m2)
                  (values NIL 0 0))
                 (T
                  (let ( (b1 (- y11 (* m1 x11)))
                         (b2 (- y22 (* m2 x22))) )
                    (let ( (xi (/ (- b2 b1) (- m1 m2))) )
                      (values T xi (+ (* m1 xi) b1))))))))))

(defun point-along-line? (x y x1 y1 x2 y2)
   (and (if (< x1 x2)
            (and (<= x1 (+ x *tg-closeness-constant*)) 
                 (<= (- x *tg-closeness-constant*) x2))
            (and (<= x2 (+ x *tg-closeness-constant*))
                 (<= (- x *tg-closeness-constant*) x1)))
        (if (< y1 y2)
            (and (<= y1 (+ y *tg-closeness-constant*)) 
                 (<= (- y *tg-closeness-constant*) y2))
            (and (<= y2 (+ y *tg-closeness-constant*)) 
                 (<= (- y *tg-closeness-constant*) y1)))))

