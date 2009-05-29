;;; This file is part of Skij.
;;; Author: Michael Travers (mt@watson.ibm.com)

;;; Licensed Materials - See the file license.txt.
;;; (c) Copyright IBM Corp. 1997, 1998. All rights reserved.

(define setf-ht (make-hashtable))

(defmacro (def-setf form val body)
  `(hashtable-put setf-ht ',(car form) 
		  (lambda ,(cons val form)
		    ,body)))

(defmacro (setf place value)
  (cond ((symbol? place)
	 `(set! ,place ,value))
	((pair? place)
	 (let ((proc (hashtable-get setf-ht (car place) #f)))
	   (if proc (apply proc value place)
	       (error `(cant setf ,place)))))
	(#t (error `(cant setf ,place)))))

(def-setf (vector-ref var index) val
  `(vector-set! ,var ,index ,val))

;;; needs a once-only
(defmacro (push thing place)
  `(setf ,place (cons ,thing ,place)))

(defmacro (pushnew thing place)
  `(if (memq ,thing ,place) #f
       (setf ,place (cons ,thing ,place))))

(defmacro (pop place)
  `(let ((result (car ,place)))
     (setf ,place (cdr ,place))
     result))

(defmacro (incf place . amt)
  (set! amt (if (null? amt) 1 (car amt)))
  `(setf ,place (+ ,amt ,place)))

(defmacro (decf place . amt)
  (set! amt (if (null? amt) 1 (car amt)))
  `(setf ,place (- ,amt ,place)))

(def-setf (hashtable-get ht key default) val
  `(hashtable-put ,ht ,key ,val))

(def-setf (car place) val
  `(set-car! ,place ,val))

(def-setf (cdr place) val
  `(set-cdr! ,place ,val))
