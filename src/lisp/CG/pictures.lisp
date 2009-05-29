;;; -------------------------------------------------------------------
;;; -------------------------------------------------------------------
;;; Common Graphics: Graphics the right way!
;;;
;;; Copyright (c) R. James Firby 1989,1991
;;; -------------------------------------------------------------------
;;; -------------------------------------------------------------------

(in-package "CG")

(defmethod depiction (self)
  (text 0 0 (format nil "~s" self) :left :baseline))

(defmethod extent (item) 
  (extent (depiction item)))

(defmethod holds-point? (item x y) 
  (holds-point? (depiction item) x y))

; --------------------------------------------------------------------
; The Main Drawing Routine
; ------------------------

(defun draw (item view x y scale cache?)
  (cond ((null item)
         (values))
        ((and cache? (cachable? item))
         (cache item view x y scale))
        ((picture? item)
         (picture-draw item view x y scale cache?))
        ((primitive? item)
         (draw-object item x y scale))
        ((depiction? item)
         (depiction-draw item view x y scale cache?))
        (T
         (draw (depiction item) view x y scale cache?))))

; --------------------------------------------------------------------
; Pictures
; --------

(defclass picture
  ()
  ((x        :accessor picture-x        :initarg  :x)
   (y        :accessor picture-y        :initarg  :y)
   (scale    :accessor picture-scale    :initarg  :scale)
   (members  :accessor picture-members  :initform '())
   (backward :accessor picture-backward :initform '())
   (users    :accessor picture-users    :initform '())
   (drawing? :accessor picture-drawing? :initform NIL)
   (events?  :accessor picture-events?  :initform NIL)
   (x1       :accessor picture-x1       :initform nil)
   (y1       :accessor picture-y1       :initform nil)
   (x2       :accessor picture-x2       :initform nil)
   (y2       :accessor picture-y2       :initform nil)
   ))

(defmethod print-object ((self picture) stream)
  (print-unreadable-object (self stream :type NIL :identity T)
     (format stream "CG Picture ~s" (picture-class self))))

(defun point<-picture-coords (picture x1 y1)
  (let ( (x0    (picture-x     picture))
         (y0    (picture-y     picture))
         (scale (picture-scale picture)) )
    (values (+ x0 (* scale x1)) (+ y0 (* scale y1)))))

(defun point->picture-coords (picture x1 y1)
  (let ( (x0    (picture-x     picture))
         (y0    (picture-y     picture))
         (scale (picture-scale picture)) )
    (values (/ (- x1 x0) scale) (/ (- y1 y0) scale))))

(defmethod propogate-change ((self picture) kind cause x1 y1 x2 y2)
  (cond ((or (eq kind 'ERASE)
             (null (picture-x1 self))
             (eq kind 'REGION))
         (multiple-value-bind (nx1 ny1 nx2 ny2 events?)
                              (recalculate-picture-stuff self)
           (set-picture-region! self nx1 ny1 nx2 ny2)
           (setf (picture-events? self) events?)))
        ((eq kind 'ADD)
         (set-picture-region! self 
                              (min x1 (picture-x1 self))
                              (min y1 (picture-y1 self))
                              (max x2 (picture-x2 self))
                              (max y2 (picture-y2 self)))
         (setf (picture-events? self)
               (or (wants-events? cause) 
                   (picture-events? self))))
        (T
         (error "Unknown change type! Internal bogosity!")))
  (multiple-value-bind (x1 y1 x2 y2)
                       (if (not (eq self cause))
                           (region<-picture-coords self x1 y1 x2 y2)
                           (values x1 y1 x2 y2))
    (do ( (users (picture-users self) (cdr users)) )
        ((null users) (values))
      (propogate-change (car users) kind cause x1 y1 x2 y2))))

(defun picture-draw (self view x y scale cache?)
  (if (picture-drawing? self)
      (error "Circular picture structure: ~s." self)
      (unwind-protect
        (let ( (x     (+ x (* scale (picture-x self))))
               (y     (+ y (* scale (picture-y self))))
               (scale (* scale (picture-scale self))) )
          (setf (picture-drawing? self) T)
          (do ( (things (picture-members self) (cdr things)) )
              ((null things) (values))
            (draw (car things) view x y scale cache?)))
        (setf (picture-drawing? self) NIL))))

(defmethod extent ((self picture))
  (if (null (picture-x1 self))
      (multiple-value-bind (x1 y1 x2 y2 events?)
                           (recalculate-picture-stuff self)
        (set-picture-region! self x1 y1 x2 y2)
        (setf (picture-events? self) events?)
        (values x1 y1 x2 y2))
      (picture-region self)))
   
(defun may-hold-point? (picture x y)
  (multiple-value-bind (x1 y1 x2 y2)
                       (extent picture)
    (or (null x1)
        (and (<= (- x1 *tg-closeness-constant*) x)
             (<= x (+ x2 *tg-closeness-constant*))
             (<= (- y1 *tg-closeness-constant*) y)
             (<= y (+ y2 *tg-closeness-constant*))))))

(defmethod holds-point? ((self picture) x y)
  (if (may-hold-point? self x y)
    (multiple-value-bind (x0 y0)
                         (point->picture-coords self x y)
      (do ( (members (picture-members self) (cdr members)) )
          ((or (null members) 
               (holds-point? (car members) x0 y0))
           (not (null members)))))
    nil))

(defmethod obscures-area? ((self picture) x1 y1 x2 y2)
  (multiple-value-bind (x1 y1 x2 y2)
                       (region->picture-coords self x1 y1 x2 y2)
    (multiple-value-bind (px1 py1 px2 py2)
                         (picture-region self)
      (if (or (null px1)
              (< x1 px1) (< y1 py1)
              (> x2 px2) (> y2 py2))
          NIL
          (piece-obscures-area? (picture-members self)
                                   x1 y1 x2 y2)))))

(defun piece-obscures-area? (pieces x1 y1 x2 y2)
  (cond ((null pieces) 
         NIL)
        ((obscures-area? (car pieces) x1 y1 x2 y2)
         T)
        (T (piece-obscures-area? (cdr pieces) x1 y1 x2 y2))))

; --------------------------
; Region Functions

(defun picture-region (picture)
  (values (picture-x1 picture) (picture-y1 picture)
          (picture-x2 picture) (picture-y2 picture)))

(defun region->picture-coords (picture x1 y1 x2 y2)
  (multiple-value-bind (nx1 ny1)
                       (point->picture-coords picture x1 y1)
    (multiple-value-bind (nx2 ny2)
                         (point->picture-coords picture x2 y2)
      (values nx1 ny1 nx2 ny2))))

(defun region<-picture-coords (picture x1 y1 x2 y2)
  (multiple-value-bind (nx1 ny1)
                       (point<-picture-coords picture x1 y1)
    (multiple-value-bind (nx2 ny2)
                         (point<-picture-coords picture x2 y2)
      (values nx1 ny1 nx2 ny2))))

(defun set-picture-region! (picture x1 y1 x2 y2)
  (setf (picture-x1 picture) x1)
  (setf (picture-y1 picture) y1)
  (setf (picture-x2 picture) x2)
  (setf (picture-y2 picture) y2)
  (values))

(defun recalculate-picture-stuff (picture)
  (let ( (members (picture-members picture)) )
    (let ( (x1 nil) (y1 nil) (x2 nil) (y2 nil) (events? NIL) )
      (do ( (members members (cdr members)) )
          ((null members)
           (if (null x1)
               (values nil nil nil nil nil)
               (multiple-value-bind (nx1 ny1 nx2 ny2)
                                    (region<-picture-coords picture 
                                                            x1 y1 x2 y2)
                 (values nx1 ny1 nx2 ny2 events?))))
        (multiple-value-bind (x-min y-min x-max y-max)
                             (extent (car members))
          (cond ((null x1)
                 (setf x1 x-min) (setf y1 y-min)
                 (setf x2 x-max) (setf y2 y-max))
                ((not (null x-min))
                 (setf x1 (min x1 x-min))  
                 (setf y1 (min y1 y-min))
                 (setf x2 (max x2 x-max))  
                 (setf y2 (max y2 y-max))))
          (if (wants-events? (car members))
              (setf events? T)))))))

; --------------------------
; Internal Picture Functions

(defun picture-class (picture)
  (let ( (members (picture-members picture)) )
    (cond ((null members) :EMPTY)
          ((null (cdr members))
           (if (primitive? (car members))
               (primitive-name (car members))
               :COMPOSITE))
          (T :COMPOSITE))))

(defun add-picture-user! (picture user)
  (let ( (end (last (picture-users picture))) )
    (if (null end)
        (setf (picture-users picture) (list user))
        (setf (cdr end) (list user))))
  picture)

(defun add-picture-member! (picture member)
  (let ( (end (last (picture-members picture))) )
    (if (null end)
        (setf (picture-members picture) (list member))
        (setf (cdr end) (list member))))
  (setf (picture-backward picture)
        (cons member (picture-backward picture)))
  picture)

(defun remove-picture-user! (picture user)
  (setf (picture-users picture)
        (delete user (picture-users picture) :test #'eq))
  picture)

(defun remove-picture-member! (picture member)
  (setf (picture-members picture)
        (delete member (picture-members picture) :test #'eq))
  (setf (picture-backward picture)
        (delete member (picture-backward picture) :test #'eq))
  picture)

; -----------------------
; Basic Picture Functions

(defun make-empty-picture ()
  (make-instance 'picture :x 0.0 :y 0.0 :scale 1.0))

(defparameter the-empty-picture (make-empty-picture))

(defun picture? (obj)
  (typep obj 'picture))

(defun copy-picture (picture)
  (assert (picture? picture) (picture))
  (let ( (new-picture (make-empty-picture)) )
    (setf (picture-x     new-picture) (picture-x     picture))
    (setf (picture-y     new-picture) (picture-y     picture))
    (setf (picture-scale new-picture) (picture-scale picture))
    (do ( (members (picture-members picture) (cdr members)) )
        ((null members) new-picture)
      (if (depiction? (car members))
          (insert! new-picture (depiction-thing (car members)))
          (insert! new-picture (car members))))))

; ---------------------------
; Pictures As Data Structures

(defun picture-components (picture)
  (assert (picture? picture) (picture))
  (let ( (result  '()) )
    (do ( (members (picture-backward picture) (cdr members)) )
        ((null members) result)
      (cond ((picture? (car members))
             (setf result (cons (car members) result)))
            ((depiction? (car members))
             (setf result (cons (depiction-thing (car members))
                                result)))))))

(defun picture-components-include-point? (picture x y)
  (assert (picture? picture) (picture))
  (do ( (members (picture-members picture) (cdr members)) )
      ((or (null members)
           (includes-point? (car members) x y))
       (not (null members)))))

(defun picture-components-including-point (picture x y)
  (assert (picture? picture) (picture))
  (let ( (result  '()) )
    (do ( (members (picture-backward picture) (cdr members)) )
        ((null members) result)
      (let ( (member (car members)) )
        (cond ((and (picture? member)
                    (includes-point? member x y))
               (setf result (cons member result)))
              ((and (depiction? member)
                    (includes-point? member x y))
               (setf result (cons (depiction-thing member) result))))))))

(defun picture-components-within-region (picture x1 y1 x2 y2)
  (assert (picture? picture) (picture))
  (let ( (result  '()) )
    (do ( (members (picture-backward picture) (cdr members)) )
        ((null members) result)
      (let ( (member (car members)) )
        (if (or (picture? member)
                (depiction? member))
            (multiple-value-bind (x-min y-min x-max y-max)
                                 (depiction-region member)
               (if (and (<= x-min x1) (<= x2 x-max)
                        (<= y-min y1) (<= y2 y-max))
                   (setf result (cons (if (depiction? member)
                                          (depiction-thing member)
                                          member)
                                      result)))))))))

(defun empty-picture? (thing)
  (and (picture? thing) (eq ':EMPTY (picture-class thing))))
(defun composite-picture? (thing)
  (and (picture? thing) (eq ':COMPOSITE (picture-class thing))))
(defun point? (thing)
  (and (picture? thing) (eq ':POINT (picture-class thing))))
(defun segment? (thing)
  (and (picture? thing) (eq ':LINE (picture-class thing))))
(defun oval? (thing)
  (and (picture? thing) (eq ':OVAL (picture-class thing))))
(defun rectangle? (thing)
  (and (picture? thing) (eq ':RECTANGLE (picture-class thing))))
(defun polygon? (thing)
  (and (picture? thing) (eq ':POLYGON (picture-class thing))))
(defun text? (thing)
  (and (picture? thing) (eq ':TEXT (picture-class thing))))

; --------------------------------------------------------------------
; Depictions
; ----------

(defclass depiction
  ()
  ((thing   :accessor depiction-thing   :initarg :thing)
   (picture :accessor depiction-picture :initarg :picture)
   (events? :accessor depiction-events? :initarg :events?)
   (user    :accessor depiction-user    :initarg :user)))

(defmethod propogate-change ((self depiction) kind cause x1 y1 x2 y2)
  (propogate-change (depiction-user self) kind cause x1 y1 x2 y2))

(defun depiction-draw (self view x y scale cache?)
  (cond ((null (depiction-picture self))
         (let ( (picture (extract-picture (depiction-thing self))) )
           (setf (depiction-picture self) picture)
           (add-picture-user! picture self)
           (draw picture view x y scale cache?)))
        ((picture-drawing? (depiction-picture self))
         (error "Circular picture structure ~s." self))
        (T
         (draw (depiction-picture self) view x y scale cache?))))

(defmethod extent ((self depiction))
  (extent (depiction-picture self)))

(defmethod holds-point? ((self depiction) x y)
  (holds-point? (depiction-picture self) x y))

(defmethod obscures-area? ((self depiction) x1 y1 x2 y2)
  (obscures-area? (depiction-picture self) x1 y1 x2 y2))

(defun depiction? (x) (typep x 'depiction))

; ----------------------------
; Internal Depiction Functions

(defun extract-picture (thing)
  (if (or (picture? thing)
          (primitive? thing))
      thing
      (extract-picture (depiction thing))))

(defun wants-events? (thing)
  (or (and (picture? thing)
           (picture-events? thing))
      (and (depiction? thing)
           (depiction-events? thing))))

(defun make-depiction (thing picture)
  (let ( (depict (make-instance 'depiction
                                :thing   thing
                                :picture picture
                                :events? (or (handles-events? thing)
                                             (wants-events? picture))
                                :user    nil)) )
    (add-picture-user! picture depict)
    depict))

(defun nullify-depiction (depict)
  (if (not (null (depiction-picture depict)))
      (remove-picture-user! (depiction-picture depict) depict))
  (setf (depiction-picture depict) nil)
  (setf (depiction-thing   depict) nil)
  (values))

; --------------------------------------------------------------------
; Insert and Remove
; -----------------

(defun insert! (picture1 thing)
  (assert (picture? picture1) (picture1))
  (let ( (picture2 (extract-picture thing)) )
    (if (eq picture1 the-empty-picture)
        (error "Cannot alter the-empty-picture."))
    (if (second-picture-in-first? picture2 picture1)
        (error "Picture ~s is a subpicture of ~s." thing picture1))
    (let ( (new (if (eq picture2 thing)
                    thing
                    (make-depiction thing picture2))) )
      (add-picture-member! picture1 new)
      (cond ((picture? new)
             (add-picture-user! new picture1))
            ((depiction? new)
             (setf (depiction-user new) picture1))
            ((primitive? new)
             (values))
            (T
             (error "Bogus insertion occurring: ~s into ~s." 
                    thing picture1)))
      (signal-a-change picture1 'ADD new)
      picture1)))

(defun remove! (picture thing)
  (assert (picture? picture) (picture) )
  (cond ((picture? thing)
         (remove-picture-user! thing picture)
         (remove-picture-member! picture thing))
        (T
         (do ( (members (picture-members picture) (cdr members)) )
             ((null members) (values))
           (let ( (member (car members)) )
             (if (and (depiction? member)
                      (eq thing (depiction-thing member)))
                 (nullify-depiction member))))
         (setf (picture-members picture)
               (delete picture (picture-members picture) :test #'null-depiction?))
         (setf (picture-backward picture)
               (delete picture (picture-backward picture) :test #'null-depiction?))))
  (signal-a-change picture 'ERASE thing)
  picture)

(defun empty-picture! (picture)
  (assert (picture? picture) (picture))
  (if (composite-picture? picture)
    (with-delayed-update
      (empty-picture-helper! picture))
    picture))

(defun empty-picture-helper! (picture)
  (let ( (thing (car (picture-backward picture))) )
    (remove! picture (if (depiction? thing)
                       (depiction-thing thing)
                       thing)))
  (if (composite-picture? picture)
    (empty-picture-helper! picture)
    picture))

(defun null-depiction? (thing depict)
  (and (depiction? depict)
       (eq (depiction-user depict) thing)
       (null (depiction-thing depict))))

(defun second-picture-in-first? (picture1 picture2)
  (cond ((eq picture1 picture2)
         T)
        ((picture? picture1)
         (do ( (members (picture-members picture1) (cdr members)) )
             ((or (null members)
                  (let ( (thing (car members)) )
                    (and (picture? thing)
                         (or (eq picture2 thing)
                             (second-picture-in-first? thing picture2)))))
              (not (null members)))))
        ((depiction? picture1)
         (second-picture-in-first? (depiction-picture picture1) picture2))
        (T NIL)))

(defun compose-pictures (&rest things)
  (let ( (new-picture (make-empty-picture)) )
    (do ( (things things (cdr things)) )
        ((null things) new-picture)
      (insert! new-picture (car things)))))

;(define (remove picture thing)   ;;; Name conflict
;  (remove! (copy-picture picture) thing))

; --------------------------------------------------------------------
; Translation and dilation
; ----------------------------------

(defun translate-to (thing x y)
  (assert (numberp x) (x))
  (assert (numberp y) (y))
  (let ( (new-pic (make-empty-picture)) )
    (setf (picture-x new-pic) x)
    (setf (picture-y new-pic) y)
    (insert! new-pic thing)
    new-pic))

(defun translate-to! (picture x y)
  (assert (picture? picture) (picture))
  (assert (numberp x) (x))
  (assert (numberp y) (y))
  (cond ((highlight? picture)
         (with-change-to-highlight picture
           (setf (picture-x picture) x)
           (setf (picture-y picture) y)
           (signal-a-change picture 'REGION picture)))
        (T
         (with-delayed-update
           (signal-a-change picture 'ERASE picture)
           (setf (picture-x picture) x)
           (setf (picture-y picture) y)
           (signal-a-change picture 'REGION picture))))
  picture)

(defun dilate-to (thing scale)
  (assert (numberp scale) (scale))
  (let ( (new-pic (make-empty-picture)) )
    (setf (picture-scale new-pic) scale)
    (insert! new-pic thing)
    new-pic))

(defun dilate-to! (picture scale)
  (assert (picture? picture) (picture))
  (assert (numberp  scale)   (scale))
  (cond ((highlight? picture)
         (with-change-to-highlight picture
           (setf (picture-scale picture) scale)
           (signal-a-change picture 'REGION picture)))
        (T
         (with-delayed-update
           (signal-a-change picture 'ERASE picture)
           (setf (picture-scale picture) scale)
           (signal-a-change picture 'REGION picture))))
  picture)


(defun translate (thing x y) (translate-to thing x y))

(defun translate! (picture x y)
  (assert (picture? picture) (picture))
  (assert (numberp  x)       (x))
  (assert (numberp  y)       (y))
  (translate-to! picture (+ x (picture-x picture))
                         (+ y (picture-y picture))))

(defun dilate (thing scale) (dilate-to thing scale))

(defun dilate! (picture scale)
  (assert (picture? picture) (picture))
  (assert (numberp  scale)   (scale))
  (dilate-to! picture (* scale (picture-scale picture))))

; --------------------------------------------------------------------
; Regions and points
; ------------------

(defun depiction-region (thing)
  (multiple-value-bind (x1 y1 x2 y2)
                       (extent thing)
    (if (null x1)
        (values 0.0 0.0 0.0 0.0) ;; The region of an empty picture
        (values x1 y1 x2 y2))))  ;;     should be undefined.

(defun includes-point? (thing x y)
  (assert (numberp x) (x))
  (assert (numberp y) (y))
  (holds-point? thing x y))

(defun relocate! (picture x y &rest options)
  (assert (picture? picture) (picture))
  (assert (numberp  x)       (x))
  (assert (numberp  y)       (y))
  (multiple-value-bind (x1 y1 x2 y2)
                       (depiction-region picture)
    (with-delayed-update
      (signal-a-change picture 'ERASE picture)
      (setf (picture-x picture)
            (cond ((member :right options :test #'eq)
                   (- (picture-x picture) (truncate (- x2 x))))
                  ((or (null options) 
                       (member :left options :test #'eq))
                   (- (picture-x picture) (truncate (- x1 x))))
                  (T
                   (- (picture-x picture) (truncate (- (/ (+ x1 x2) 2) x))))))
      (setf (picture-y picture)
            (cond ((member :top options :test #'eq)
                   (- (picture-y picture) (truncate (- y2 y))))
                  ((or (null options) 
                       (member :bottom options :test #'eq))
                   (- (picture-y picture) (truncate (- y1 y))))
                  (T
                   (- (picture-y picture) (truncate (- (/ (+ y1 y2) 2) y))))))
      (signal-a-change picture 'REGION picture))
    picture))

(defun relocate (object x y &rest options)
  (apply #'relocate! (insert! (make-empty-picture) object) x y options))
                    



