;;;-*- Mode: Lisp; Package: VEHICLES -*-
;;----------------------------------------------------------------------
;; 
;; Copyright:   Copyright (c) 1999 John Wiseman
;; File:        parsing.lisp
;; Created:     23 March 1997
;; Author:      John Wiseman (wiseman@neodesic.com)
;; 
;; Description: World description parser for Braitenberg vehicles.
;; 
;; Changes:     
;; 
;;----------------------------------------------------------------------

(in-package "VEHICLES")


(defvar *object-library* nil)

(defun reset-object-library ()
  (setq *object-library*
        (list (cons :vehicle (make-hash-table :test #'eq))
              (cons :lamp (make-hash-table :test #'eq)))))

(defun add-object-to-library (lib type definition)
  (setf (gethash type (cdr (assoc lib *object-library*))) definition)
  type)

(defun get-object-from-library (lib type)
  (copy-tree (gethash type (cdr (assoc lib *object-library*)))))


(defun instantiate-vehicle (type world name body)
  (let ((def (get-object-from-library :vehicle type)))
    (create-vehicle world name (append def body))))

(defun instantiate-lamp (type world name body)
  (let ((def (get-object-from-library :lamp type)))
    (create-lamp world name (append def body))))


(defmacro ticks-per-second (ticks)
  `(setf (world-ticks-per-second *world*) ,ticks))

(defmacro seed (n)
  (let ((n-var (gensym "N-")))
    `(let ((,n-var ,n))
       (setf (vrandom-state-seed (world-rand-state *world*))
             (mod ,n-var (vrandom-state-m (world-rand-state *world*)))))))



(defmacro define-radiation-types (&rest rad-types)
  `(fill-radiation-isa-table (world-radiation-hierarchy *world*)
                             ',rad-types))

(defmacro vehicle (name &body b)
  (if (symbolp (car b))
    `(instantiate-vehicle ',(car b) *world* ',name ',(cdr b))
    `(create-vehicle *world* ',name ',b)))

(defmacro lamp (name &body b)
  (if (symbolp (car b))
    `(instantiate-lamp ',(car b) *world* ',name ',(cdr b))
    `(create-lamp *world* ',name ',b)))


(defmacro define-vehicle (type &body b)
  `(add-object-to-library :vehicle ',type ',b))

(defmacro define-lamp (type &body b)
  `(add-object-to-library :lamp ',type ',b))



(defparameter *include-path* nil)

(eval-when (:load-toplevel)
  (setq *include-path*
        (merge-pathnames (make-pathname :directory '(:relative "Includes"))
                         *load-pathname*)))

                   
(defun vinclude (filename)
  (let ((global-include (merge-pathnames filename
                                         *include-path*)))
    (if (probe-file global-include)
      (load global-include)
      (let ((local-include (merge-pathnames filename
                                            *load-pathname*)))
        (if (probe-file local-include)
          (load local-include))))))



(defvar *network-macros* (make-hash-table :test #'eq))

(defmacro define-network (name vars &body body)
  `(setf (gethash ',name *network-macros*)
         #'(lambda ,vars
             ,@body)))

(defun network-macro? (name)
  (and (gethash name *network-macros*) T))

(defun network-expand (name args)
  (let ((network-expander (gethash name *network-macros*)))
    (if (null network-expander)
      (error "Network ~S is undefined in ~S." name (cons name args))
      (apply network-expander args))))




;; Parameter list definitions for various object definitions

;;   Parameter       Function             Required?   Default
(defparameter *vehicle-params* 
  '((:position       create-position	  T)
    (:orientation    create-orientation	  T)
    (:max-speed      one-arg		  NIL	      10.0)
    (:sensor         create-sensor	  NIL)
    (:motor 	     create-motor 	  T)
    (:radiator       create-radiator 	  NIL)
    (:gun	     create-gun           NIL)
    (:brain          create-brain 	  NIL)
    (:color	     create-color	  NIL)))

(defparameter *lamp-params* 
  `((:position       create-position	  T)
    (:orientation    create-orientation   NIL	      (2d-orientation 0.0))
    (:radiator       create-radiator	  T)
    (:color	     create-color	  NIL)
    (:sensor         create-sensor	  NIL)
    (:brain	     create-brain	  NIL)))

(defparameter *radiator-params* 
  '((:radiation-type one-arg		  T)
    (:decay-factor   one-arg		  T)
    (:brightness     one-arg		  NIL	      1.0)))

(defparameter *sensor-params*
  `((:radiation-type one-arg		  T)
    (:field-of-view  one-arg-eval	  NIL	      150.0)
    (:non-directional? one-arg		  NIL)
    (:sensitivity    one-arg		  NIL         1.0)
    (:orientation    create-orientation   NIL         (2d-orientation 0.0))))

;(defparameter *gun-params*
;  `((:orientation    create-orientation   NIL         ,(2d-orientation 0.0))))

(defparameter *motor-params*
  '((:position       one-arg		  T)
    (:decay-factor   one-arg		  NIL         0.005)))

(defparameter *neurode-params*
  '((:inputs         one-arg		  NIL)
    (:threshold      one-arg		  NIL         nil)
    (:inhibitors     one-arg		  NIL         nil)))



(defun create-vehicle (world name definition)
  (let ((v (make-instance 'two-wheeled-vehicle
             :name name
             :world world)))
    (let ((bindings (slot-values *vehicle-params*
				 (make-symbol-table)
                                 world
			         (list (cons name v))
                                 definition)))
      ;; Should try to check that nodes are connected in a way that
      ;; has at least the possibility of causing the vehicle to move.
      (setf (location v) (lookup :position bindings)
	    (orientation v) (lookup :orientation bindings)
	    (max-speed v) (lookup :max-speed bindings))
      (when (lookup :color bindings)
        (setf (color v) (lookup :color bindings)))
      (let ((motors (lookup :motor bindings)))
	(flet ((sym= (s1 s2) (string= (symbol-name s1) (symbol-name s2))))
          (setf (right-motor v) (find 'RIGHT motors :key #'location :test #'sym=)
                (left-motor v) (find 'LEFT motors :key #'location :test #'sym=))))
      (let ((gun-bindings (lookup :gun bindings)))
        (when gun-bindings
          (setf (guns v) (if (listp gun-bindings)
                           gun-bindings
                           (list gun-bindings)))))
      (setf (bindings v) bindings)
      (push v (world-vehicles world))
      v)))


(defun create-lamp (world name body)
  (let ((l (make-instance 'lamp :name name :world world)))
    (let ((bindings (slot-values *lamp-params*
				 (make-symbol-table)
                                 world
		                 (list (cons name l))
		                 body)))
      (setf (location l) (lookup :position bindings)
            (orientation l) (lookup :orientation bindings))
      (when (lookup :color bindings)
        (setf (color l) (lookup :color bindings)))
      (setf (bindings l) bindings)
      (push l (world-lamps world))
      l)))


(defun create-radiator (symbols world containers body)
  (let* ((name (car body))
         (r (make-instance 'radiator
              :name (new-name name containers)
              :world world)))
    (let ((bindings (slot-values *radiator-params*
                                 symbols
                                 world
                                 (cons (cons name r) containers)
                                 (cdr body))))
      (setf (radiation-type r) (lookup :radiation-type bindings)
            (brightness r) (lookup :brightness bindings)
            (decay-factor r) (lookup :decay-factor bindings)
            ;; most recent container should be something like
            ;; (lamp-a . #S(LAMP ...))
            (platform r) (cdar containers))
      (symbol-table-add symbols name r)
      (push r (world-radiators world))
      r)))

(defun create-gun (symbols world containers body)
  (let* ((name (car body))
         (orientation (lookup :orientation (cdr body)))
         (position (lookup :position (cdr body)))
         (range (lookup :range (cdr body)))
         (g (make-instance 'gun
              :name (new-name name containers)
              :relative-orientation (if orientation
                                      (2d-orientation (deg->rad (car orientation)))
                                      (2d-orientation 0))
              :relative-location (if position
                                   (2d-location (eval (car position))
                                                (eval (cadr position)))
                                   (2d-location 0 0))
              :range (if range (car range) 3.0)
              :platform (cdar containers)
              :world world)))
    (symbol-table-add symbols name g)
    g))


(defun create-sensor (symbols world containers body)
  (let* ((name (car body))
         (s (make-instance 'directional-sensor
              :name (new-name name containers)
              :world world)))
    (let ((bindings (slot-values *sensor-params*
				 symbols
                                 world
				 (cons (cons name s) containers)
				 (cdr body))))
      (setf (radiation-type s) (lookup :radiation-type bindings)
	    (sensitivity s) (lookup :sensitivity bindings)
            (relative-location s) (2d-location 0.0 0.0)
            (relative-orientation s) (lookup :orientation bindings)
	    (field-of-view s) (deg->rad (lookup :field-of-view bindings))
            (platform s) (cdar containers)
            (directional? s) (null (lookup :non-directional? bindings)))
      (symbol-table-add symbols name s)
      (push s (world-sensors world))
      s)))


(defun create-motor (symbols world containers body)
  (let* ((name (car body))
         (m (make-instance 'motor
              :name (new-name name containers)
              :world world)))
    (let ((bindings (slot-values *motor-params*	
			         symbols
                                 world
			         (cons (cons name m) containers)
				 (cdr body))))
      (setf (location m) (lookup :position bindings)
            (decay-factor m) (lookup :decay-factor bindings))
      (symbol-table-add symbols name m)
      m)))


(defun create-brain (symbols world containers body)
  (flet ((get-node-by-name (name)
           (if (null name)
             nil
             (let ((n (symbol-table-get symbols name)))
               (if (null n)
                 (symbol-table-add symbols
                                   name
                                   (make-instance 'neurode
                                     :name (new-name name containers)
                                     :world world))
                 n)))))
    ;; Expand network macros:
    (let ((expanded-body (mapcan #'(lambda (node)
                                     (let ((name (car node)))
                                       (if (network-macro? name)
                                         (network-expand name (cdr node))
                                         (list node))))
                                 body)))
      (dolist (node expanded-body)
        (let* ((name (car node))
               (containers (cons (cons name 'brain) containers))
               (n (get-node-by-name name)))
          (if (not (typep n 'node-with-inputs))
            (error "Node ~s cannot be used in ~s, it has no inputs"
                   name containers)
            ;; :threshold is not required, but we want the default value to
	    ;; be nil here, and set the real default later
	    (let ((bindings (slot-values *neurode-params*
				         symbols
                                         world
                                         containers
			                 (cdr node))))
              (let ((inputs (mapcar #'get-node-by-name
                                    (lookup :inputs bindings))))
                (setf (inputs n) inputs)
                (setf (original-inputs n) inputs))
              (cond ((typep n 'neurode)
		     (let ((threshold (lookup :threshold bindings))
		           (inhibitors (mapcar #'get-node-by-name
					       (lookup :inhibitors bindings))))
		       (when threshold
	                 (setf (threshold n) threshold))
	               (when inhibitors
                         (setf (inhibitors n) inhibitors))
                       (when (and threshold (= threshold 0) (not (null (inputs n))))
                         (warn "~s in ~s has a threshold of 0 and has inputs."
                               name containers))
                       (when (and (> (threshold n) 0) (null (inputs n)))
                         (warn "~s in ~s has a non-zero threshold but has no inputs."
                               name containers))))
	            (T
                     (when (lookup :threshold bindings)
                       (error ":threshold cannot be specified for ~s in ~s, it isn't a neurode"
                              name containers))
		     (when (lookup :inhibitors bindings)
	               (error ":inhibitors cannot be specified for ~s in ~s, it isn't in a neurode"
                              name containers))))))))
      ;; I'm not sure we really have anything useful to return(?)
      (symbol-table-to-bindings symbols))))



(defun create-position (symbols world containers body)
  (declare (ignore symbols world containers))
  (assert (= (length body) 2))
  (2d-location (eval (car body)) (eval (cadr body))))



(defun create-orientation (symbols world containers body)
  (declare (ignore symbols world containers))
  (assert (= (length body) 1))
  (2d-orientation (deg->rad (car body))))


(defun create-color (symbols world containers body)
  (declare (ignore symbols world containers))
  (assert (= (length body) 3))
  (apply #'rgb->color body))


;;
;; generic slot functions
;;

(defstruct (slot-pat (:type list))
  name
  constructor
  required?
  default-value)


(defun one-arg (symbols world containers body)
  (declare (ignore symbols world containers))
  (assert (= (length body) 1))
  (car body))


(defun one-arg-eval (symbols world containers body)
  (declare (ignore symbols world containers))
  (assert (= (length body) 1))
  (eval (car body)))



(defun list-arg (symbols world containers body)
  (declare (ignore symbols world containers))
  body)



;;
;; slot-filling functions
;;


(defun slot-values (slot-pats symbols world containers l)
  (assert (every #'(lambda (p) (not (and (slot-pat-required? p)
                                         (slot-pat-default-value p))))
		 slot-pats))
  (labels ((fill-slots (l bindings)
	     (cond ((null l) bindings)
                   ((atom l) (error "Bad syntax ~s in ~s" l containers))  ; bad syntax
		   (T 
                    (let ((slot-name (caar l))
                          (slot-body (cdar l)))
	              (let ((slot (get-slot slot-pats slot-name)))
                        (if (null slot)
		          ;; unrecognized slot tag
		          (error "Unrecognized slot tag ~s in ~s" slot-name containers) 
                          (fill-slots (cdr l)
				      (extend-bindings slot-name
                                                       (funcall (slot-pat-constructor slot)
							        symbols
                                                                world
                                                                containers
                                                                slot-body)
                                                       bindings)))))))))
    (let ((bindings (fill-slots l nil)))
      (let ((s (check-required-slots slot-pats bindings)))
	(when s (error "Slot ~s is required in ~s" s containers))
	(add-default-values slot-pats bindings)))))



(defun check-required-slots (slot-pats bindings)
  (if (null slot-pats) 
    nil
    (let ((p (car slot-pats)))
      (if (and (slot-pat-required? p)
               (not (lookup (slot-pat-name p) bindings)))
        (slot-pat-name p)
	(check-required-slots (cdr slot-pats) bindings)))))



(defun add-default-values (slot-pats bindings)
  (cond ((null slot-pats) bindings)
	(T
         (let ((slot (car slot-pats)))
           (if (and (slot-pat-default-value slot)
		    (not (lookup (slot-pat-name slot) bindings)))
             (add-default-values (cdr slot-pats)
                                 (extend-bindings (slot-pat-name slot)
                                                  (eval (slot-pat-default-value slot))
                                                  bindings))
	     (add-default-values (cdr slot-pats) bindings))))))




(defun get-slot (slot-pats slot-name)
  (let ((slot (member slot-name slot-pats :key #'slot-pat-name)))
    (if slot
      (car slot)
      nil)))



(defun allowed-slot? (slots-pat slot-name)
  (or (member slot-name slots-pat)
      (assoc slot-name slots-pat)))





;;
;; bindings functions
;;


(defun lookup (var bindings)
  "Looks up the value of a variable in a binding environment."
  (cdr (assoc var bindings)))

(defun extend-bindings (var val bindings)
  "Extends a binding environment by binding var to val. If var already has a
   binding, then val is added to the existing binding."
  (let ((binding (assoc var bindings)))
    (cond ((null binding)
           (push (cons var val) bindings))
	  ((not (listp (cdr binding)))
           (setf (cdr binding) (list val (cdr binding)))
	   bindings)
          (T
           (push val (cdr binding))
           bindings))))



;;
;; Symbol table functions
;;


;(defun make-symbol-table (&rest hash-table-args)
;  "Creates a new symbol table."
;  (apply #'make-hash-table hash-table-args))
;
;
;(defun symbol-table-add (table key value)
;  "Adds an entry to a symbol table."
;  (let ((old-value (gethash key table)))
;    (if (not (null old-value))
;      (if (not (eq value old-value))
;        (error "symbol redeclared")
;        (values))
;      (setf (gethash key table) value))))
;
;
;
;(defun symbol-table-get (table key)
;  "Looks up an entry in a symbol table."
;  (gethash key table))
;
;(defun symbol-table-to-bindings (table)
;  (let ((bindings '()))
;    (maphash #'(lambda (k v)
;                 (push (cons k v) bindings))
;             table)
;    bindings))



(defun make-symbol-table ()
  "Creates a new symbol table."
  (list '()))

(defun symbol-table-add (table key value)
  "Adds an entry to a symbol table."
  (let ((assoc (assoc key (car table))))
    (if (not (null assoc))
      (if (not (eq value (cdr assoc)))
        (error "symbol redeclared")
        (values))
      (progn (setf (car table) (acons key value (car table)))
             value))))

(defun symbol-table-get (table key)
  "Looks up an entry in a symbol table."
  (cdr (assoc key (car table))))

(defun symbol-table-to-bindings (table)
  (let ((bindings '()))
    (dolist (assoc (car table))
      (push (cons (car assoc) (cdr assoc)) bindings))
    bindings))




(defun new-name (name containers)
  (intern (concatenate 'string
                       (symbol-name name)
                       "@"
                       (symbol-name (caar containers)))
          "CL-USER"))

