;;;-*- Mode: Lisp; Package: VEHICLES -*-
;;----------------------------------------------------------------------
;; 
;; Copyright:   Copyright (c) 1999 John Wiseman
;; File:        qd3d-renderer.lisp
;; Created:     10 June 1998
;; Author:      John Wiseman (jjwiseman@gmail.com)
;; 
;; Description: MCL QuickDraw 3D renderer for Braitenberg vehicles.
;;		See <http://nd1.neodesic.com/lisp/QD3D/> for the latest
;;		QD3D interface code.
;; 
;; Changes:     
;; 
;;----------------------------------------------------------------------

(in-package "VEHICLES")

(export 'qd3d-renderer)

(ccl:require :quickdraw-3d)


(load "ccl:library;quickdraw-3d;extras;flyby")


(defstruct platform-group
  group
  xform
  attribs
  geometry
  label
  laser)



(defclass qd3d-renderer (vehicle-renderer)
  ((window :accessor window :initform nil)
   (window-size :accessor window-size :initform 300 :initarg :window-size)
   (vehicle-groups :accessor vehicle-groups :initform (make-hash-table :test #'eq))
   (lamp-groups :accessor lamp-groups :initform (make-hash-table :test #'eq))
   (lights :accessor lights :initform (make-hash-table :test #'eq))
   (laser :accessor laser :initform (create-laser))
   (woo-wooer :accessor woo-wooer :initform T :initarg :woo-woo)
   (labels? :accessor labels? :initform NIL :initarg :labels?)
   (font :accessor font :initform '("Geneva" 12) :initarg :font)
   (renderer :accessor renderer :initform :interactive :initarg :renderer)))



(defun create-laser ()
  (q3:create-display-group
   (q3:create-illumination-shader :null)
   (q3:create-attribute-set :diffuse-color 1 1 0)
   (q3:create-cylinder 0 0 0 .03 1 nil :ribs 4)))
   


(defmethod render-animation ((self qd3d-renderer))
  (let ((model (q3:create-display-group
                (create-plane -10 0 -10 20 20
                              (q3:create-attribute-set :diffuse-color .4 .4 .8)
                              :ribs 10)))
        (light-group (q3:create-light-group
                      (q3:create-ambient-light 0.5 1.0 1.0 1.0))))
    (dolist (v (world-vehicles (world self)))
      (let ((g (create-platform-group self v)))
        (setf (gethash v (vehicle-groups self)) g)
        (q3:group-add model (platform-group-group g))))
    (dolist (l (world-lamps (world self)))
      (let ((g (create-platform-group self l)))
        (setf (gethash l (lamp-groups self)) g)
        (q3:group-add model (platform-group-group g)))
      (let ((q3-light (q3:create-point-light (2d-location-x (location l))
                                             0.4
                                             (- (2d-location-y (location l)))
                                             0.5
                                             (color-r (color l))
                                             (color-g (color l))
                                             (color-b (color l)))))
        (setf (gethash l (lights self)) q3-light)
        (q3:group-add light-group q3-light)))
    (setf (window self) (make-instance 'qd3d-renderer-window
                          :light-group light-group
                          :update-light-group-p NIL
                          :renderer (q3:create-renderer (renderer self))
                          :model model
                          :view-size (let ((size (window-size self)))
                                       (if (<= size 65535)
                                         (ccl:make-point size size)
                                         size))
                          :clear-color ccl:*black-color*))
    (q3:point-camera-at (q3:view-camera (window self))
                        0.0 0.0 0.0)
    (let ((woo-wooer
           (make-woo-wooer (window self)
                           :min-tilt -.05
                           :max-tilt 1.0
                           :frames 300
                           :radius 11
                           :poi '(-2 0 2))))
      (funcall woo-wooer 0)
      (when (woo-wooer self)
        (setf (woo-wooer self) woo-wooer)))
    (call-next-method)))



(defmethod render-animation ((self qd3d-renderer))
  (let ((model (q3:create-display-group
                (create-plane -10 0 -10 20 20
                              (q3:create-attribute-set :diffuse-color .4 .4 .8)
                              :ribs 10)))
        (light-group (q3:create-light-group
                      (q3:create-ambient-light 0.5 1.0 1.0 1.0))))
    (dolist (v (world-vehicles (world self)))
      (let ((g (create-platform-group self v)))
        (setf (gethash v (vehicle-groups self)) g)
        (q3:group-add model (platform-group-group g))))
    (dolist (l (world-lamps (world self)))
      (let ((g (create-platform-group self l)))
        (setf (gethash l (lamp-groups self)) g)
        (q3:group-add model (platform-group-group g)))
      (let ((q3-light (q3:create-point-light (2d-location-x (location l))
                                             0.4
                                             (- (2d-location-y (location l)))
                                             0.5
                                             (color-r (color l))
                                             (color-g (color l))
                                             (color-b (color l)))))
        (setf (gethash l (lights self)) q3-light)
        (q3:group-add light-group q3-light)))
    (setf (window self) (make-instance 'qd3d-renderer-window
                          :light-group light-group
                          :update-light-group-p NIL
                          :renderer (q3:create-renderer (renderer self))
                          :model model
                          :view-size (let ((size (window-size self)))
                                       (if (<= size 65535)
                                         (ccl:make-point size size)
                                         size))
                          :clear-color ccl:*black-color*))
    (q3:point-camera-at (q3:view-camera (window self))
                        0.0 0.0 0.0)
    (let ((woo-wooer
           (make-woo-wooer (window self)
                           :min-tilt 0.5
                           :max-tilt 0.5
                           :frames 300
                           :radius 11
                           :poi '(-2 0 2))))
      (funcall woo-wooer 0)
      (when (woo-wooer self)
        (setf (woo-wooer self) woo-wooer)))
    (call-next-method)))


(defmethod render-frame ((self qd3d-renderer))
  (let ((world (world self))
        (v-groups (vehicle-groups self))
        (l-groups (lamp-groups self)))
    (dolist (v (world-vehicles world))
      (update-platform-group self v (gethash v v-groups)))
    (dolist (l (world-lamps world))
      (update-platform-group self l (gethash l l-groups)))
    (when (woo-wooer self)
      (funcall (woo-wooer self) (+ 221
                                   (floor (* (world-time-seconds (world self))
                                             30)))))
    (q3:render-view (window self))))


(defun create-platform-group (renderer platform)
  (let ((xform (update-transform (q3:create-transform) platform))
        (attribs (q3:create-attribute-set
                  :diffuse-color (color-r (color platform))
                  (color-g (color platform))
                  (color-b (color platform))))
        (geometry (typecase platform
                    (two-wheeled-vehicle
                     (q3:create-display-group
                      (q3:translate-transform (q3:create-transform)
                                              0 .12 0)
                      (q3:create-box -0.3 0 -0.15
                                     (vehicle-length platform) 0.1 0.3)
                      (q3:rotate-transform-xyz (q3:create-transform)
                                               (/ pi 2) 0.0 0.0)
                      (q3:create-attribute-set :diffuse-color 0 0 0)
                      (q3:create-cylinder -0.15 0.08 -0.05 .12 .1 nil :ribs 8)
                      (q3:create-cylinder -0.15 -0.16 -0.05 .12 .1 nil :ribs 8)))
                    (lamp
                      (q3:create-sphere 0 0.1 0 0.2 nil :ribs 5))
                    (otherwise
                     (q3:create-box 0 0 0 .4 .4 .4))))
        (laser (if (and (typep platform 'two-wheeled-vehicle)
                        (guns platform))
                 (q3:create-display-group)
                 nil))
        (label (if (labels? renderer)
                 (let ((name (string (name-of platform)))
                       (font (font renderer)))
                 (q3:create-text-marker 0.0 1.0 0.0
                                        name
                                        font
                                        (- (/ (ccl:string-width name font)
                                           2))
                                        0.0))
                 nil)))
    (let ((elts (remove-if #'null (list xform attribs geometry laser label))))
      (make-platform-group :group (apply #'q3:create-display-group elts)
                           :xform xform
                           :attribs attribs
                           :geometry geometry
                           :label label
                           :laser laser))))

(defmethod update-platform-group ((renderer qd3d-renderer) (self two-wheeled-vehicle) group)
  (update-transform (platform-group-xform group) self)
  (let ((lg (platform-group-laser group)))
    (when lg
      (dolist (elt (q3:map-group #'identity lg))
        (q3:group-delete lg elt))
      (when (some #'fired-flag (guns self))
        (dolist (gun (guns self))
          (when (fired-flag gun)
            (let ((gun-theta (2d-orientation-theta (relative-orientation gun))))
              (setf (fired-flag gun) nil)
              (let ((xform (q3:create-transform)))
                (q3:rotate-transform-xyz xform 0.0 0.0  #.(/ pi -2))
                (q3:scale-transform xform (range gun) 1.0 1.0)
                (q3:rotate-transform-xyz xform 0.0 gun-theta 0.0)
                (q3:translate-transform xform 0.0 0.17 0.0)
                (q3:group-add lg
                              (q3:create-display-group
                               xform
                               (laser renderer)))))))))))


(defmethod update-platform-group ((renderer qd3d-renderer) (self lamp) group)
  (let ((brightness (brightness self)))
    (update-attribute-set (platform-group-attribs group) self brightness)
    (update-lamp-light renderer self brightness)))


(defun update-transform (xform platform)
  (let* ((theta (2d-orientation-theta (orientation platform)))
         (location (location platform))
         (x (2d-location-x location))
         (y (2d-location-y location)))
    (q3:reset-transform xform)
    (q3:rotate-transform-xyz xform 0.0 theta 0.0)
    (q3:translate-transform xform x 0 (- y))))

(defun update-attribute-set (attribute-set platform brightness)
  (let ((color (color platform)))
    (q3:attribute-set-add attribute-set :diffuse-color
                          (* (color-r color) brightness)
                          (* (color-g color) brightness)
                          (* (color-b color) brightness))))



(defun update-lamp-light (renderer lamp brightness)
  (let ((q3-light (gethash lamp (lights renderer))))
    (q3:set-light-brightness q3-light brightness)))



(defun create-plane (x y z width height &optional attribute-set &key (ribs 20))
  (when (null attribute-set)
    (setq attribute-set (ccl:%null-ptr)))
  (flet ((make-odd (n)
	   (+ n (- 1 (mod n 2)))))
    (let* ((mesh (q3::q3-mesh-new))
	   (dx (/ width ribs))
	   (dz (/ height ribs))
	   (vertices (make-array (list (+ ribs 1) (+ ribs 1)))))
      (unless (ccl:%null-ptr-p attribute-set)
        (q3::q3-geometry-set-attribute-set mesh attribute-set))
      ;; Create vertices.
      (q3:with-mesh-updates-delayed mesh
	(dotimes (i (+ ribs 1))
	  (dotimes (j (+ ribs 1))
	    (let* ((cx (+ x (* i dx)))
		   (cy y)
		   (cz (+ z (* j dz)))
		   (u (/ i ribs))
		   (v (/ j ribs)))
	      (q3:with-q3-objects ((attribs (q3:create-attribute-set :surface-uv u v)))
		(setf (aref vertices i j)
		      (q3:add-mesh-vertex mesh cx cy cz attribs))))))
        ;; Create faces
        (dotimes (i ribs)
	  (dotimes (j ribs)
	    (q3:add-mesh-face mesh (list (aref vertices (+ i 1) j)
					 (aref vertices (+ i 1) (+ j 1))
					 (aref vertices i (+ j 1))
					 (aref vertices i j))))))
      mesh)))






#|

(defclass snapshot-rendering-mixin ()
  ((filename :accessor filename :initform nil :initarg :filename)
   (filename-pattern :accessor filename-pattern :initform nil :initarg :filename-pattern)
   (file-format :accessor file-format :initform :pics :initarg :file-format)
   (snapshot-function :accessor snapshot-function :initform nil)
   (resnum :accessor resnum :initform nil)))




(defun get-cg-window-as-pict (window)
  (unless (null (cg::cg-window-change window))
    (cg::window-draw window))
  (let ((mac-window (cg::cg-window-real window)))
    (let ((new-image (progn
                       (ccl:start-picture mac-window)
                       (cg::window-refresh window)
                       (ccl:get-picture mac-window))))
      (if (not (ccl:handlep new-image))
        NIL
        new-image))))
  


(defmethod render-animation :around ((self snapshot-rendering-mixin))
  (cond ((find-package "CG")
         (setf (snapshot-function self)
               #'get-cg-window-as-pict))
        ((find-package "Q3")
         (setf (snapshot-function self)
               (symbol-function (find-symbol "GET-WINDOW-PIXMAP-AS-PICT"
                                             (find-package "Q3")))))
        (T
         (error "Can't figure out how to take a snapshot.")))
  (check-type (file-format self) (member :pict :pics))
  (ecase (file-format self)
    ((:pict)
     (assert (not (null (filename-pattern self))))
     (call-next-method))
    ((:pics)
     (assert (not (null (filename self))))
     (ccl:with-open-resource-file (resnum (filename self) :if-does-not-exist :create)
       (setf (resnum self) resnum)
       (call-next-method)))))

(defmethod render-frame :around ((self snapshot-rendering-mixin))
  (call-next-method)
  (ecase (file-format self)
    ((:pict)
     (let ((filename (format nil (filename self) (absolute-frame-number self))))
       (q3:write-window-pixmap-as-pict (window self) filename)))
    ((:pics)
     (let ((pict (funcall (snapshot-function self) (window self))))
       (unwind-protect
         (ccl:using-resource-file (resnum self)
           (ccl:add-resource pict :PICT (+ 128 (absolute-frame-number self)))
           (ccl::write-resource pict)
           (ccl:detach-resource pict))
         (when pict
           (ccl::kill-picture pict)))))))



(defclass qd3d-renderer-ss (snapshot-rendering-mixin qd3d-renderer)
  ())

|#


(defclass qt-qd3d-renderer (qd3d-renderer)
  ((filename :accessor filename :initform "frame.~5,'0D" :initarg :filename)))


(defmethod render-frame :around ((self qt-qd3d-renderer))
  (call-next-method)
  (let ((filename (format nil (filename self) (absolute-frame-number self))))
    (q3:synchronize-view (window self))
    (q3:write-window-pixmap-as-pict (window self) filename)))


(defmethod ccl:window-draw-grow-icon ((self q3:qd3d-view))
  )


(defclass qd3d-renderer-window (q3:qd3d-window)
  ())

(defmethod ccl:window-draw-grow-icon ((self qd3d-renderer-window))
  )


#|

(defmethod ccl:view-click-event-handler ((self qd3d-renderer-window) where)
  (let ((hit-data (q3:pick-view self
                                where
                                (q3:pick-detail-mask :pick-id :object :path)
                                :near-to-far)))
    
    (unwind-protect
      (let ((h (car hit-data)))
        (q3::with-fields (:TQ3HitData object) h
          (print (q3::q3-type-of object T))))
      (dolist (h hit-data)
        (q3::q3-pick-empty-data h)))))

|#
