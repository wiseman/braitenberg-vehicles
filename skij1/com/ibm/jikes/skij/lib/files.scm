;;; This file is part of Skij.
;;; Author: Michael Travers (mt@watson.ibm.com)

;;; Licensed Materials - See the file license.txt.
;;; (c) Copyright IBM Corp. 1997, 1998. All rights reserved.

(define (open-input-file filename)
  (define file-obj (new 'java.io.File filename))
  (define reader (new 'java.io.BufferedReader
		      (new 'java.io.InputStreamReader
			   (new 'java.io.FileInputStream file-obj))))
  (new 'com.ibm.jikes.skij.InputPort reader))

(define (open-output-file filename)
  (define file-obj (new 'java.io.File filename))
  (define stream (new 'java.io.FileOutputStream file-obj))
  (new 'com.ibm.jikes.skij.OutputPort stream))

(define (call-with-input-file filename func)
  (define port #f)
  (dynamic-wind (lambda () (set! port (open-input-file filename)))
		(lambda () (func port))
		(lambda () (close-input-port port))))

(define (call-with-output-file filename func)
  (define port #f)
  (dynamic-wind (lambda () (set! port (open-output-file filename)))
		(lambda () (func port))
		(lambda () (close-output-port port))))

(define (directory dir)
  (define fileobj (new 'java.io.File dir))
  (define filarray (invoke fileobj 'list))
  (vector->list filarray))

(define (with-open-for-append file proc)
  (let ((stream #f)
	(port #f))
    (dynamic-wind
     (lambda ()
       (set! stream
	     (new 'java.io.FileOutputStream (invoke file 'getPath) #t)) ;open for append
       (set! port (new 'com.ibm.jikes.skij.OutputPort stream)))
     (lambda () (proc port))
     (lambda ()
       (invoke stream 'close)))))

;;; apply procedure w recursive descent
(define (for-all-files proc dir)
  (define (process fileobj)
    (if (invoke fileobj 'isDirectory)
	(for-vector (lambda (name)
		      (process
		       (new 'java.io.File fileobj name)))
		    (invoke fileobj 'list))
	(proc fileobj)))
  (process (new 'java.io.File dir)))
  
  