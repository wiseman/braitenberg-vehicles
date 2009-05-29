                  




(defun rgb->color (r g b)
  (define (->255 x) (integer (* x 255)))
  (new 'java.awt.Color (->255 r) (->255 g) (->255 b)))

(defun deg->rad (d)
  (* d (/ pi 180.0)))

(defun 2d-orientation (theta)
  (new 'org.heavymeta.vehicles.Orientation (double theta)))

(defun 2d-location (x y)
  (new 'org.heavymeta.vehicles.Location (double x) (double y)))

(defun make-world ()
  (new 'org.heavymeta.vehicles.World))


(defun jvector-add (v e)
  (invoke v 'addElement e))

(defun list->jvector (list)
  (let ((v (new 'java.util.Vector)))
    (dolist (e list)
      (invoke v 'addElement e))
    v))

(defun jvector->list (v)
  (enumeration->list (invoke v 'elements)))




(define *field-types* '())

(defun define-field-type (type ->java ->scheme)
  (set! *field-types* (acons type (list ->java ->scheme) *field-types*)))

(defun get-field-type (type)
  (if type
    (let ((types (assoc type *field-types*)))
      (if types
        (cdr types)
        (list #f #f)))
    (list #f #f)))

(defun add-converter (cvtr form)
  (if cvtr
    (list cvtr form)
    form))

(define-field-type 'jvector 'jvector->list 'list->jvector)
(define-field-type 'double #f 'double)


;(ttrace add-converter)

(defmacro (def-field . spec)
  (let ((name (car spec))
        (type (if (not (null (cdr spec)))
                (cadr spec)
                #f))
        (dot (lambda (sym)
               (string->symbol (string-append "." (symbol->string sym))))))
    (let ((converters (get-field-type type)))
      `(begin (defun ,name (obj)
                ,(add-converter (car converters) `(peek obj ',name)))
              (defun ,(dot name) (obj)
                (peek obj ',name))
              (def-setf (,name obj) value
                (list 'poke obj '',name ,(list 'add-converter (list 'quote (cadr converters)) 'value)))
              (def-setf (,(dot name) obj) value
                (list 'poke obj '',name value))))))
          

(def-field current_inputs jvector)
(def-field original_inputs jvector)
(def-field threshold double)
(def-field inhibitors jvector)
(def-field m_location)
(def-field m_orientation)
(def-field decay_factor double)
(def-field radiation_type)
(def-field sensitivity double)
(def-field relative_location)
(def-field relative_orientation)
(def-field field_of_view double)
(def-field platform)
(def-field directionalp)
(def-field brightness double)
(def-field color)
(def-field bindings)
(def-field max_speed double)
(def-field guns jvector)
(def-field ticks_per_second)
(def-field radiation_hierarchy)
(def-field right_motor)
(def-field left_motor)


(defun inputs (node)
  (invoke node 'inputs))

(defun .world-radiators (world)
  (peek world 'radiators))

(defun .world-lamps (world)
  (peek world 'lamps))

(defun .world-vehicles (world)
  (peek world 'vehicles))

(defun .world-sensors (world)
  (peek world 'sensors))





















;;; This file is part of Skij.
;;; Author: Michael Travers (mt@watson.ibm.com)

;;; Licensed Materials - See the file license.txt.
;;; (c) Copyright IBM Corp. 1997, 1998. All rights reserved.

(define *trace-level* 0)

(define (traceprint msg)
  (display
   (with-string-output-port
     (lambda (port)
       (newline port)
       (let loop ((i (dynamic *trace-level*)))
         (unless (zero? i)
           (display "  " port)
           (loop (- i 1))))
       (write msg port)))))

(defmacro (with-traceprint msg . body)
  `(begin (traceprint ,msg)
	  (let ((*trace-level* (+ (dynamic *trace-level*) 1)))
	    ,@body)))

(define (proc-name proc)
  (%or-null (peek proc 'name)
	    proc))

(define (trace-encapsulate procedure)
  (lambda args
    (traceprint`(Entering ,(proc-name procedure) with args ,@args))
    (define result #f)
    (let ((*trace-level* (+ (dynamic *trace-level*) 1)))
      (set! result (apply procedure args)))
    (traceprint `(Exit ,(proc-name procedure) with ,result))
    result))

(define *ttraced-procs* (make-hash-table))

(defmacro (ttrace procname)
  `(begin
    (setf (gethash ',procname *ttraced-procs*) ,procname)
    (set! ,procname
	  (trace-encapsulate ,procname))))

(defmacro (unttrace procname)
  (let ((old-proc-var (gentemp)))
  `(let ((,old-proc-var (gethash ',procname *ttraced-procs*)))
     (when ,old-proc-var
       (set! ,procname ,old-proc-var)
       (remhash ',procname *ttraced-procs*)))))


