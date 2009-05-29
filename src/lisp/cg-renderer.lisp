;;;-*- Mode: Lisp; Package: VEHICLES -*-
;;----------------------------------------------------------------------
;; 
;; Copyright:   Copyright (c) 1999 John Wiseman
;; File:        cg-renderer.lisp
;; Created:     19 July 1998
;; Author:      John Wiseman (wiseman@neodesic.com)
;; 
;; Description: Common Graphics renderer for Braitenberg vehicles.
;; 
;; Changes:     
;; 
;;----------------------------------------------------------------------

(in-package "VEHICLES")

(export 'common-graphics-renderer)


(unless (find-package "COMMON-GRAPHICS")
  (let ((file (merge-pathnames (make-pathname :directory '(:relative "CG")
                                              ;; This is gross.
					      :name #+mcl "CG" #-mcl "CG-clx"
                                              :type "lisp")
			       (asd:this-files-pathname))))
    (if (probe-file (print file))
      (load file)
      (asd:define-system "CG" :directory (asd:this-files-directory)))
    (asd:load-system 'cg)))


(defclass common-graphics-renderer (vehicle-renderer)
  ((window :accessor window :initform nil)
   (view :accessor view :initform nil)
   (window-size :accessor window-size :initform 300 :initarg :window-size)
   (scale :accessor scale :initform 20 :initarg :scale)
   (labels? :accessor labels? :initform NIL :initarg :labels?)
   (world-view :accessor world-view :initform nil)
   (depictions :accessor depictions :initform (make-hash-table))))
  

   
(defmethod window-translation ((self common-graphics-renderer))
  (/ (window-size self) 2))

(defmethod window-scale ((self common-graphics-renderer))
  (/ (window-size self) (scale self)))

(defvar *cg-renderer* nil)


;; This is gross.

#+mcl
(defun open-cg-window (size)
  (cg:open-window :size size size :type :document))

#-mcl
(defun open-cg-window (size)
  (let* ((display (system:getenv "DISPLAY"))
	 (host (if (null display)
		   nil
		 (let ((end (position #\: display)))
		   (if end
		       (subseq display 0 end)
		     display)))))
  (cg:open-window :size size size :type host)))


(defmethod render-animation ((self common-graphics-renderer))
  (let ((size (window-size self)))
    (setf (window self) (open-cg-window size))
    (setf *cg-renderer* self)
    (compute-initial-depictions self)
    (cg:open-view (window self) (cg:rectangle 0 0 size size
                                              :filled T
                                              :color (cg:->color .4 .4 .8)))
    (cg:open-view (window self) (world self))
    (call-next-method)))

(defmethod render-frame ((self common-graphics-renderer))
  (let* ((world (world self))
         (vehicles (world-vehicles world))
         (lamps (world-lamps world)))
    (cg:with-delayed-update 
      (dolist (l lamps)
        (update-depiction self l))
      (dolist (v vehicles)
        (update-depiction self v)))))



        
                                      
(defun ->cg-color (color &optional (bright 1.0))
  (cg:->color (* (color-r color) bright)
              (* (color-g color) bright)
              (* (color-b color) bright)))


(defmethod cg:depiction ((self lamp))
  (gethash self (depictions *cg-renderer*)))

(defmethod cg:depiction ((self two-wheeled-vehicle))
  (gethash self (depictions *cg-renderer*)))


(defmethod compute-depiction ((renderer common-graphics-renderer) (self lamp) container)
  (let ((l (location self)))
    (let ((x (2d-location-x l))
          (y (2d-location-y l))
          (radiators (remove-if-not
                      #'(lambda (r)
                          (eq (platform r) self))
                      (world-radiators (world self)))))
      (let* ((brightness (if (null radiators)
                           1.0
                           (min 1.0
                                (reduce #'+ (mapcar #'output radiators)))))
             (new-c (->cg-color (color self) brightness)))
        (cg:insert! container (cg:circle x y 0.2 :filled T :color new-c))
        (cg:insert! container (cg:circle x y 0.2 :filled NIL :color cg:color/white))
        (when (labels? renderer)
          (cg:insert! container
                      (cg:translate
                       (cg:dilate (cg:text 0 0 (string (name-of self)))
                                  (/ 1.0 (window-scale renderer)))
                       x
                       (+ y 0.6))))
        container))))


(defun update-depiction (renderer object)
  (let ((d (cg:depiction object)))
    (cg:empty-picture! d)
    (compute-depiction renderer object d)))

    
(defun compute-initial-depictions (renderer)
  (let ((objects (append (world-vehicles (world renderer))
                         (world-lamps (world renderer)))))
    (dolist (o objects)
      (setf (gethash o (depictions renderer))
            (compute-depiction renderer o (cg:compose-pictures))))))



(defmethod compute-depiction ((renderer common-graphics-renderer) (self two-wheeled-vehicle) container)
  (let ((l (location self))
        (o (orientation self)))
    (let ((x (2d-location-x l))
          (y (2d-location-y l))
          (theta (2d-orientation-theta o)))
      (cg:insert! container
                  (cg:circle x y (/ (wheel-base self) 2)
                             :color (->cg-color (color self))
                             :filled T))
      (cg:insert! container
                  (cg:circle x y (/ (wheel-base self) 2)
                             :color cg:color/black
                             :filled nil))
      (cg:insert! container
                  (cg:segment x y
                              (+ x (* (cos theta) (vehicle-length self)))
                              (+ y (* (sin theta) (vehicle-length self)))
                              :color cg:color/black))
      (when (labels? renderer)
        (cg:insert! container
                    (cg:translate
                     (cg:dilate (cg:text 0 0 (string (name-of self)))
                                (/ 1.0 (window-scale renderer)))
                     x
                     (+ y (/ (wheel-base self) .66667)))))
      (dolist (gun (guns self))
        (when (fired-flag gun)
          (let ((gun-theta (2d-orientation-theta (orientation gun))))
            (setf (fired-flag gun) nil)
            (cg:insert! container
                        (cg:segment x y
                                    (+ x (* (cos gun-theta) (range gun)))
                                    (+ y (* (sin gun-theta) (range gun)))
                                    :color cg:color/yellow)))))
      container)))

(defmethod cg:depiction ((self world))
  (let ((translation (window-translation *cg-renderer*))
        (scale (window-scale *cg-renderer*)))
    (let ((picture (apply #'cg:compose-pictures 
                          (append (mapcar #'cg:depiction
                                          (world-vehicles self))
                                  (mapcar #'cg:depiction
                                          (world-lamps self))))))
    (cg:translate! (cg:dilate! picture scale)
                   translation
                   translation))))



(defclass qt-cg-renderer (common-graphics-renderer)
  ((filename :accessor filename :initform "frame.~5,'0D" :initarg :filename)))


(defmethod render-frame :around ((self qt-cg-renderer))
  (call-next-method)
  (let ((filename (format nil (filename self) (absolute-frame-number self))))
    (cg::copy-window-to-file (window self) filename)))


#|

(defmethod cg:event-handler ((self two-wheeled-vehicle) event x y)
  (print self))

|#
