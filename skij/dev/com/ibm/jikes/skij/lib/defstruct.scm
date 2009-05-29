;;; This file is part of Skij.
;;; Author: Michael Travers (mt@watson.ibm.com)

;;; Licensed Materials - See the file license.txt.
;;; (c) Copyright IBM Corp. 1997, 1998. All rights reserved.

; Elementary form of defstruct. 
; note: it would be useful to use adaptors to give reasonable printed representations (at cost of extra references)
; structures are implemented using vectors

(defmacro (defstruct name . fields)
  (let ((nfields (length fields))
	(i -1))
    `(begin
       (define (,(symbol-conc 'make- name) ,@fields)
	 (list->vector (list ,@fields)))
       ,@(apply nconc
		(map (lambda (field)
		       (set! i (+ i 1))
		       (define getter-name (symbol-conc name '- field))
		       (define setter-name (symbol-conc 'set- name '- field '!))
		       (list
			`(define (,setter-name thing new-val)
			   (vector-set! thing ,i new-val))
			`(define (,getter-name thing)
			   (vector-ref thing ,i))
			`(def-setf (,getter-name thing) new-val
			   `(,',setter-name ,thing ,new-val))
			))
		     fields)))))