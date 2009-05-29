;;; This file is part of Skij.
;;; Author: Michael Travers (mt@watson.ibm.com)

;;; Licensed Materials - See the file license.txt.
;;; (c) Copyright IBM Corp. 1997, 1998. All rights reserved.

;;; Class loader stuff

(define (make-class-loader . path-entries)
  (let ((loader (new 'com.ibm.jikes.skij.util.ExtendableClassLoader)))
    (map (lambda (path-entry) (invoke loader 'addClassPath path-entry))
	 path-entries)
    loader))

(define (cl-class-named cl class-name)
  (invoke cl 'loadClass (string class-name)))

(define (class-from-paths class-name . path-entries)
  (cl-class-named (apply make-class-loader path-entries)
		  class-name))

(define (new-cl cl name . args)
  (apply new (cl-class-named cl name) args))
    
; +++ invoke-static and others that can take a class name.

