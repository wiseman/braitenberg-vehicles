;;; -------------------------------------------------------------------
;;; -------------------------------------------------------------------
;;; Common Graphics: Graphics the right way!
;;;
;;; Copyright (c) R. James Firby 1989,1991
;;; -------------------------------------------------------------------
;;; -------------------------------------------------------------------

(in-package "CG")

; --------------------------------------------------------------------
; Fonts
; -----

(defstruct (cg-font (:print-function print-cg-font))
  family
  size
  options
  internal)

(defun print-cg-font (self stream level)
  (declare (ignore level))
  (print-unreadable-object (self stream :type NIL :identity T)
     (format stream "Font ~s ~d ~s" 
             (cg-font-family self)  (cg-font-size self)
             (cg-font-options self))))

(defun cg-font-constant (family size &rest options)
  (make-cg-font :family family :size size :options options 
                :internal nil))

(defparameter font/standard (cg-font-constant "Geneva" 12 :plain))
(defparameter font/system   font/standard)

;(defparameter *font-list* '("Geneva"))

(defun font? (x) (typep x 'cg-font))

(defun ->font (&rest options)
  (let ( (options (copy-list (if (listp (car options)) 
                                 (car options) 
                                 options))) )
    (multiple-value-bind (family size options)
                         (parse-font options)
      (make-cg-font 
       :family   (if (null family)
                     (cg-font-family font/standard)
                     family)
       :size     (if (null size) (cg-font-size font/standard) (truncate size))
       :options  (if (null options) (list :plain) options)
       :internal nil))))

(defun parse-font (font)
  (let ( (family (extract-type #'stringp font))
         (size   (extract-type #'numberp font)) )
    (let* ( (options (if (null family) 
                         font 
                         (remove family font :test #'eq)))
            (options (if (null size) 
                         options 
                         (remove size options :test #'equal))) )
      (values family size options))))

(defun extract-type (type list)
  (cond ((null list) nil)
        ((funcall type (car list)) (car list))
        (T (extract-type type (cdr list)))))


; --------------------------------------------------------------------
; Colors
; ------

(defstruct (cg-color (:print-function print-cg-color))
  red
  green
  blue
  primary
  internal)

(defun print-cg-color (self stream level)
  (declare (ignore level))
  (print-unreadable-object (self stream :type NIL :identity T)
    (if (or (eq (cg-color-primary self) :foreground)
            (eq (cg-color-primary self) :background))
        (format stream "Color ~s" (cg-color-primary self))
        (format stream "Color ~d ~d ~d" 
                (cg-color-red self)  (cg-color-green self)
                (cg-color-blue self)))))

(defun cg-color-constant (r g b primary)
  (make-cg-color :red r :green g :blue b :primary primary
                 :internal nil))
             
(defparameter color/foreground (cg-color-constant 1.0 1.0 1.0 :foreground))
(defparameter color/background (cg-color-constant 0.0 0.0 0.0 :background))

(defparameter color/white      (cg-color-constant 1.0 1.0 1.0 :white))
(defparameter color/black      (cg-color-constant 0.0 0.0 0.0 :black))
(defparameter color/red        (cg-color-constant 1.0 0.0 0.0 :red))
(defparameter color/green      (cg-color-constant 0.0 1.0 0.0 :green))
(defparameter color/blue       (cg-color-constant 0.0 0.0 1.0 :blue))
(defparameter color/cyan       (cg-color-constant 1.0 1.0 0.0 :cyan))
(defparameter color/magenta    (cg-color-constant 1.0 0.0 1.0 :magenta))
(defparameter color/yellow     (cg-color-constant 0.0 1.0 1.0 :yellow))

(defun color? (color) (typep color 'cg-color))

(defun ->color (r g b)
  (cond ((or (not (numberp r)) (< r 0.0) (> r 1.0)
             (not (numberp g)) (< g 0.0) (> g 1.0)
             (not (numberp b)) (< b 0.0) (> b 1.0))
         (cerror "Use the foreground color instead."
                 "Invalid color definition (->color ~d ~d ~d)"
                 r g b)
         color/foreground)
        (T
         (let ( (color (make-cg-color :red r :green g :blue b)) )
           (setf (cg-color-internal color) nil)
           (setf (cg-color-primary  color) nil)
;           (setf (cg-color-primary  color)
;                 (cond ((and (= r 0) (= g 0) (= b 0))   :black)
;                       ((and (= r g) (= r b) (> r 0.5)) :white)
;                       ((and (> r g) (> r b))           :red)
;                       ((and (> g r) (> g b))           :green)
;                       ((and (> b g) (> b r))           :blue)
;                       ((and (> r b) (> g b))           :cyan)
;                       ((and (> r g) (> b g))           :magenta)
;                       ((and (> g r) (> b r))           :yellow)
;                       (T                               :black)))
           color))))

(defun color-brighten (color percent)
  (cond ((not (color? color))
         (cerror "Use the foreground color instead."
                 "Invalid color ~d" color)
         color/foreground)
        (T
         (->color (max 0.0 (min 1.0  (* percent (cg-color-red   color))))
                  (max 0.0 (min 1.0  (* percent (cg-color-green color))))
                  (max 0.0 (min 1.0  (* percent (cg-color-blue  color))))))))

; --------------------------------------------------------------------
; Textures
; --------

(defstruct (cg-texture (:print-function print-cg-texture))
  name
  internal)

(defun print-cg-texture (self stream level)
  (declare (ignore level))
  (print-unreadable-object (self stream :type NIL :identity T)
     (format stream "Texture ~s" (cg-texture-name self))))

(defun cg-texture-constant (name)
  (make-cg-texture :name name
                   :internal nil))

(defparameter texture/0%   (cg-texture-constant :stipple-0))
(defparameter texture/25%  (cg-texture-constant :stipple-25))
(defparameter texture/50%  (cg-texture-constant :stipple-50))
(defparameter texture/75%  (cg-texture-constant :stipple-75))
(defparameter texture/100% (cg-texture-constant :stipple-100))

(defun texture? (x) (typep x 'cg-texture))


;(lset texture/0%   *white-pattern*)
;(lset texture/100% *black-pattern*)
;(lset texture/25%  *light-gray-pattern*)
;(lset texture/50%  *gray-pattern*)
;(lset texture/75%  *dark-gray-pattern*)
;
;(lset |TG Textures| (list texture/0% texture/100%
;                                     texture/25% texture/50% texture/75%))
;
;(define (texture? texture) (memq? texture |TG Textures|))

