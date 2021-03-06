

(defparameter *v-command-table* (make-hash-table))

(defmacro (def-v-cmd name-and-args lambda)
  `(setf (gethash ',(car name-and-args) *v-command-table*)
         (list ',name-and-args
               ,lambda)))

(defun get-v-cmd (name)
  (gethash name *v-command-table*))

(defun execute-v-cmd (cmd world args)
  (newline)
  (display (stringify (list ";; Executing " (caar cmd))))
  (apply (second cmd) (cons world args)))



(define *world* '())

(defun load-world (world-file)
  (call-with-input-file world-file
    read-world))

(defun read-world (port)
  (let ((world (make-world)))
    (reset-object-library)
    (do ((form (read port) (read port)))
        ((eof-object? form) world)
      (let* ((name (car form))
             (command (get-v-cmd name)))
        (if (null command)
          (lerror "Unknown directive " name " in " form)
          (execute-v-cmd command world (cdr form)))))))


(defvar *object-library* '())

(defun reset-object-library ()
  (set! *object-library*
        (list (cons ':vehicle (make-hash-table))
              (cons ':lamp (make-hash-table)))))

(defun add-object-to-library (lib type definition)
  (setf (gethash type (cdr (assoc lib *object-library*))) definition)
  type)

(defun get-object-from-library (lib type)
  (copy-tree (gethash type (cdr (assoc lib *object-library*)))))


(defun instantiate-vehicle (type world name body)
  (let ((def (get-object-from-library ':vehicle type)))
    (create-vehicle world name (append def body))))

(defun instantiate-lamp (type world name body)
  (let ((def (get-object-from-library ':lamp type)))
    (create-lamp world name (append def body))))


(def-v-cmd (ticks-per-second ticks)
  (lambda (world ticks)
    (setf (ticks_per_second world) (eval ticks))))

(def-v-cmd (seed n)
  (lambda (world n)
    (setf (vrandom-state-seed (world-rand-state world))
          (mod (eval n) (vrandom-state-m (world-rand-state world))))))

(def-v-cmd (define-radiation-types . rad-types)
  (lambda (world . types)
    (let ((table (radiation_hierarchy world)))
      (invoke table 'clear)
      (dolist (type types)
        (let ((child (symbol-name (first type)))
              (parents (second type)))
          (dolist (parent parents)
            (invoke table 'addChild (symbol-name parent) child)))))))


(def-v-cmd (vehicle name . body)
  (lambda (world name . body)
    (if (symbolp (car body))
      (instantiate-vehicle (car body) world name (cdr body))
      (create-vehicle world name body))))

(def-v-cmd (lamp name . body)
  (lambda (world name . body)
    (if (symbolp (car body))
      (instantiate-lamp (car b) world name (cdr body))
      (create-lamp world name body))))


(def-v-cmd (define-vehicle type . body)
  (lambda (type . body)
    (add-object-to-library ':vehicle type body)))

(def-v-cmd (define-lamp type . body)
  (lambda (type . body)
    (add-object-to-library ':lamp type body)))



;(defparameter *include-path* nil)
;
;(eval-when (:load-toplevel)
;  (setq *include-path*
;        (merge-pathnames (make-pathname :directory '(:relative "Includes"))
;                         *load-pathname*)))
;
;                   
;(defun vinclude (filename)
;  (let ((global-include (merge-pathnames filename
;                                         *include-path*)))
;    (if (probe-file global-include)
;      (load global-include)
;      (let ((local-include (merge-pathnames filename
;                                            *load-pathname*)))
;        (if (probe-file local-include)
;          (load local-include))))))



(defvar *network-macros* (make-hash-table))

(def-v-cmd (define-network name vars . body)
  (lambda (name vars . body)
    (setf (gethash name *network-macros*)
          (eval `(lambda ,vars
                   ,@body)))))

(defun network-macro? (name)
  (and (gethash name *network-macros*) #t))

(defun network-expand (name args)
  (let ((network-expander (gethash name *network-macros*)))
    (if (null network-expander)
      (error "Network " name " is undefined in " args ".")
      (apply network-expander args))))




;; Parameter list definitions for various object definitions

;;   Parameter       Function             Required?   Default
(defparameter *vehicle-params* 
  `((:position       create-position	  #t)
    (:orientation    create-orientation   #t)
    (:max-speed      one-arg		  #f	      10.0)
    (:sensor         create-sensor	  #f)
    (:motor 	     create-motor 	  #t)
    (:radiator       create-radiator 	  #f)
    (:gun	     create-gun           #f)
    (:brain          create-brain 	  #f)
    (:color	     create-color	  #f)))

(defparameter *lamp-params* 
  `((:position       create-position	  #t)
    (:orientation    create-orientation   #f	      (2d-orientation 0.0))
    (:radiator       create-radiator	  #t)
    (:color	     create-color	  #f)
    (:sensor         create-sensor	  #f)
    (:brain	     create-brain	  #f)))

(defparameter *radiator-params* 
  `((:radiation-type one-arg		  #t)
    (:decay-factor   one-arg		  #t)
    (:brightness     one-arg		  #f	      1.0)))

(defparameter *sensor-params*
  `((:radiation-type one-arg		  #t)
    (:field-of-view  one-arg-eval	  #f	      150.0)
    (:non-directional? one-arg		  #f)
    (:sensitivity    one-arg		  #f         1.0)
    (:orientation    create-orientation   #f         (2d-orientation 0.0))))

;(defparameter *gun-params*
;  `((:orientation    create-orientation   NIL         ,(2d-orientation 0.0))))

(defparameter *motor-params*
  `((:position       one-arg		  #t)
    (:decay-factor   one-arg		  #f         0.005)))

(defparameter *neurode-params*
  '((:inputs         one-arg		  #f)
    (:threshold      one-arg		  #f         #f)
    (:inhibitors     one-arg		  #f         '())))



(defun create-vehicle (world name definition)
  (let ((v (make-instance '#,(vehicles-class 'TwoWheeledVehicle)
             'name (symbol-name name)
             'world world)))
    (let ((bindings (slot-values *vehicle-params*
				 (make-symbol-table)
                                 world
			         (list (cons name v))
                                 definition)))
      ;; Should try to check that nodes are connected in a way that
      ;; has at least the possibility of causing the vehicle to move.
      (setf (m_location v) (lookup ':position bindings)
	    (m_orientation v) (lookup ':orientation bindings)
	    (max_speed v) (lookup ':max-speed bindings))
      (when (lookup ':color bindings)
        (setf (color v) (lookup ':color bindings)))
      (let ((motors (lookup ':motor bindings)))
        (flet ((sym= (s1 s2) (string=? s1 s2)))
          (setf (right_motor v) (find-key-test "RIGHT" motors m_location sym=)
                (left_motor v) (find-key-test "LEFT" motors m_location sym=))))
      (let ((gun-bindings (lookup ':gun bindings)))
        (when gun-bindings
          (setf (guns v) (if (listp gun-bindings)
                           gun-bindings
                           (list gun-bindings)))))
      ;(setf (bindings v) bindings)
      (jvector-add (.world-vehicles world) v)
      v)))


(defun create-lamp (world name body)
  (let ((l (make-instance '#,(vehicles-class 'Lamp)
			  'name (symbol-name name) 'world world)))
    (let ((bindings (slot-values *lamp-params*
				 (make-symbol-table)
                                 world
		                 (list (cons name l))
		                 body)))
      (setf (m_location l) (lookup ':position bindings)
            (m_orientation l) (lookup ':orientation bindings))
      (when (lookup ':color bindings)
        (setf (color l) (lookup ':color bindings)))
      ;(setf (bindings l) bindings)
      (jvector-add (.world-lamps world) l)
      l)))


(defun create-radiator (symbols world containers body)
  (let* ((name (car body))
         (r (make-instance '#,(vehicles-class 'Radiator)
              'name (symbol-name (new-name name containers))
              'world world)))
    (let ((bindings (slot-values *radiator-params*
                                 symbols
                                 world
                                 (cons (cons name r) containers)
                                 (cdr body))))
      (setf (radiation_type r) (symbol-name (lookup ':radiation-type bindings))
            (brightness r) (lookup ':brightness bindings)
            (decay_factor r) (lookup ':decay-factor bindings)
            ;; most recent container should be something like
            ;; (lamp-a . #S(LAMP ...))
            (platform r) (cdar containers))
      (symbol-table-add symbols name r)
      (jvector-add (.world-radiators world) r)
      r)))

(defun create-gun (symbols world containers body)
  (let* ((name (car body))
         (orientation (lookup ':orientation (cdr body)))
         (position (lookup ':position (cdr body)))
         (range (lookup ':range (cdr body)))
         (g (make-instance '#,(vehicles-class 'Gun)
              'name (symbol-name (new-name name containers))
              'relative-orientation (if orientation
                                      (2d-orientation (deg->rad (car orientation)))
                                      (2d-orientation 0))
              'relative-location (if position
                                   (2d-location (eval (car position))
                                                (eval (cadr position)))
                                   (2d-location 0 0))
              'range (if range (car range) 3.0)
              'platform (cdar containers)
              'world world)))
    (symbol-table-add symbols name g)
    g))


(defun create-sensor (symbols world containers body)
  (let* ((name (car body))
         (s (make-instance '#,(vehicles-class 'Sensor)
              'name (symbol-name (new-name name containers))
              'world world)))
    (let ((bindings (slot-values *sensor-params*
				 symbols
                                 world
				 (cons (cons name s) containers)
				 (cdr body))))
      (setf (radiation_type s) (symbol-name (lookup ':radiation-type bindings))
	    (sensitivity s) (lookup ':sensitivity bindings)
            (relative_location s) (2d-location 0.0 0.0)
            (relative_orientation s) (lookup ':orientation bindings)
	    ;(field_of_view s) (deg->rad (lookup ':field-of-view bindings))
            (platform s) (cdar containers)
            ;(directionalp s) (null (lookup ':non-directional? bindings))
            )
      (symbol-table-add symbols name s)
      (jvector-add (.world-sensors world) s)
      s)))


(defun create-motor (symbols world containers body)
  (let* ((name (car body))
         (m (make-instance '#,(vehicles-class 'Motor)
              'name (symbol-name (new-name name containers))
              'world world)))
    (let ((bindings (slot-values *motor-params*	
			         symbols
                                 world
			         (cons (cons name m) containers)
				 (cdr body))))
      (setf (m_location m) (string-upcase (symbol-name (lookup ':position bindings)))
            (decay_factor m) (lookup ':decay-factor bindings))
      (symbol-table-add symbols name m)
      m)))


(defun create-brain (symbols world containers body)
  (flet ((get-node-by-name (name)
           (if (null name)
             #f
             (let ((n (symbol-table-get symbols name)))
               (if (null n)
                 (symbol-table-add symbols
                                   name
                                   (make-instance '#,(vehicles-class 'Neurode)
                                     'name (symbol-name (new-name name containers))
                                     'world world))
                 n)))))
    ;; Expand network macros:
    (let ((expanded-body (mapcan (lambda (node)
                                   (let ((name (car node)))
                                     (if (network-macro? name)
                                       (network-expand name (cdr node))
                                       (list node))))
                                 body)))
      (dolist (node expanded-body)
        (let* ((name (car node))
               (containers (cons (cons name 'brain) containers))
               (n (get-node-by-name name)))
          (if (not (typep n '#,(vehicles-class 'NodeWithInputs)))
            (lerror "Node " name " cannot be used in " containers ", it has no inputs.")
            ;; :threshold is not required, but we want the default value to
	    ;; be nil here, and set the real default later
	    (let ((bindings (slot-values *neurode-params*
				         symbols
                                         world
                                         containers
			                 (cdr node))))
              (let ((inputs (mapcar get-node-by-name
                                    (lookup ':inputs bindings))))
                (setf (current_inputs n) inputs)
                (setf (original_inputs n) inputs))
              (cond ((typep n '#,(vehicles-class 'Neurode))
		     (let ((the-threshold (lookup ':threshold bindings))
		           (the-inhibitors (mapcar get-node-by-name
					           (lookup ':inhibitors bindings))))
		       (when the-threshold
	                 (setf (threshold n) the-threshold))
	               (when the-inhibitors
                         (setf (inhibitors n) the-inhibitors))
                       (when (and the-threshold (= the-threshold 0) (not (null (inputs n))))
                         (warn name " in " containers
                               " has a threshold of 0 and has no inputs."))
                       (when (and (> (threshold n) 0) (null (inputs n)))
                         (warn name " in " containers
                               " has a non-zero threshold but has no inputs."))))
	            (else
                     (when (lookup ':threshold bindings)
                       (lerror ":threshold cannot be specified for " name
                               " in " containers ", it isn't a neurode"))
		     (when (let ((i (lookup ':inhibitors bindings)))
			     (and (not (null? i)) (eq i '())))
	               (lerror ":inhibitors cannot be specified for " name
                               " in " containers ", it isn't in a neurode"))))))))
      ;; I'm not sure we really have anything useful to return(?)
      (symbol-table-to-bindings symbols))))



(defun create-position (symbols world containers body)
;  (declare (ignore symbols world containers))
  (assert (= (length body) 2))
  (2d-location (eval (car body)) (eval (cadr body))))



(defun create-orientation (symbols world containers body)
  (declare (ignore symbols world containers))
  (assert (= (length body) 1))
  (2d-orientation (deg->rad (car body))))


(defun create-color (symbols world containers body)
  (declare (ignore symbols world containers))
  (assert (= (length body) 3))
  (apply rgb->color body))


;;
;; generic slot functions
;;

(defun %or-f (x) (if (%%null? x) #f x))

(defun slot-pat-name (x) (%or-f (list-ref x 0)))
(defun slot-pat-constructor (x) (%or-f (list-ref x 1)))
(defun slot-pat-required? (x) (%or-f (list-ref x 2)))
(defun slot-pat-default-value (x) (%or-f (list-ref x 3)))


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
  (dolist (p slot-pats)
    (when (not (not (and (slot-pat-required? p)
                         (slot-pat-default-value p))))
      (print p)))
  (assert (every (lambda (p) (not (and (slot-pat-required? p)
                                       (slot-pat-default-value p))))
		 slot-pats))
  (labels ((fill-slots (l bindings)
	     (cond ((null l) bindings)
                   ((atom l) (lerror "Bad syntax " l " in "
                                     containers))
		   (else
                    (let ((slot-name (caar l))
                          (slot-body (cdar l)))
	              (let ((slot (get-slot slot-pats slot-name)))
                        (if (null slot)
		          ;; unrecognized slot tag
		          (lerror "Unrecognized slot tag " slot-name
                                  " in " containers)
                          (fill-slots (cdr l)
				      (extend-bindings slot-name
                                                       (funcall (eval (slot-pat-constructor slot))
							        symbols
                                                                world
                                                                containers
                                                                slot-body)
                                                       bindings)))))))))
    (let ((bindings (fill-slots l '())))
      (let ((s (check-required-slots slot-pats bindings)))
	(when s
          (lerror "Slot " s " is required in " containers))
	(add-default-values slot-pats bindings)))))



(defun check-required-slots (slot-pats bindings)
  (if (null slot-pats) 
    #f
    (let ((p (car slot-pats)))
      (if (and (slot-pat-required? p)
               (not (lookup (slot-pat-name p) bindings)))
        (slot-pat-name p)
	(check-required-slots (cdr slot-pats) bindings)))))



(defun add-default-values (slot-pats bindings)
  (cond ((null slot-pats) bindings)
	(else
         (let ((slot (car slot-pats)))
           (if (and (slot-pat-default-value slot)
		    (not (lookup (slot-pat-name slot) bindings)))
             (add-default-values (cdr slot-pats)
                                 (extend-bindings (slot-pat-name slot)
                                                  (eval (slot-pat-default-value slot))
                                                  bindings))
	     (add-default-values (cdr slot-pats) bindings))))))




(defun get-slot (slot-pats slot-name)
  (let ((slot (member-key-test slot-name slot-pats slot-pat-name eqv?)))
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
  (let ((b (assoc var bindings)))
    (if (null b)
      #f
      (cdr b))))

(defun extend-bindings (var val bindings)
  "Extends a binding environment by binding var to val. If var already has a binding, then val is added to the existing binding."
  (let ((binding (assoc var bindings)))
    (cond ((null binding)
           (push (cons var val) bindings))
	  ((not (listp (cdr binding)))
           (setf (cdr binding) (list val (cdr binding)))
	   bindings)
          (else
           (begin (push val (cdr binding))
                  bindings)))))



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
      (if (not (eqv? value (cdr assoc)))
        (lerror "symbol redeclared"))
      (progn (setf (car table) (acons key value (car table)))
             value))))

(defun symbol-table-get (table key)
  "Looks up an entry in a symbol table."
  (let ((v (assoc key (car table))))
    (if (null v)
      #f
      (cdr v))))

(defun symbol-table-to-bindings (table)
  (let ((bindings '()))
    (dolist (assoc (car table))
      (push (cons (car assoc) (cdr assoc)) bindings))
    bindings))




(defun new-name (name containers)
  (string->symbol
   (string-append (symbol-name name)
                  "@"
                  (symbol-name (caar containers)))))





;(ttrace create-vehicle)
;(ttrace create-lamp)
;(ttrace create-position)
;(ttrace create-orientation)
;(ttrace create-brain)
;(ttrace create-radiator)
;(ttrace create-sensor)
;(ttrace create-motor)
;(ttrace m_location)


;(ttrace symbol-table-get)
;(ttrace symbol-table-add)

;(ttrace slot-values)
;(ttrace find-key-test)


;(ttrace get-slot)
;(ttrace lookup)
;(ttrace extend-bindings)


