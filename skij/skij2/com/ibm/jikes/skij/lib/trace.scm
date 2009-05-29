;;; This file is part of Skij.
;;; Author: Michael Travers (mt@watson.ibm.com)

;;; Licensed Materials - See the file license.txt.
;;; (c) Copyright IBM Corp. 1997, 1998. All rights reserved.

;;; Note: this doesn't take any account of threads. Probably a tracer should be
;;; specific to the thread it was created in, or something.
;;; also, it was written before I had macros. probably could use a redesign.
;;; see also dynamic-wind and dynamic variables...
;;; updating trees on the fly is kind of slow. perhaps we want a record and show mode...sigh.
;;; also, link to inspector from tree

(define tracer #f)

; a tracer is a list: (tree current-node)
(define (make-tracer name)
  (define tree (make-tree-window name #f)) 
  (set! tracer
	(list tree (set-root tree name)))
  tracer)

(make-tracer 'tracer)

(define (set-current-node tracer nnode)
  (set-car! (cdr tracer) nnode))

(define (traceprint tracer msg)
  (add-child (car tracer) (cadr tracer) (write-to-string msg)))

(define (tracein tracer msg)
  (define nnode (traceprint tracer msg))
  (set-current-node tracer nnode)
  nnode)

(define (traceout node msg)
  (if node '() (set! node (cadr tracer)))
  (set-current-node tracer (node-parent node))
  (traceprint tracer msg))

(define (traced-proc procedure name)
  (define encapsulated (trace-encapsulate procedure))
  (lambda args
    (if (eq? (car args) 'magic-restore-argument)
	(eval `(set! ,name procedure))
	(apply encapsulated args))))

(define (trace-encapsulate procedure)
  (lambda args
    (define node (tracein tracer `(Entering ,procedure with ,args)))
    (define result (apply procedure args))
    (traceout node `(Exit with ,result))
    result))

(define (settrace procname)
  (set! saved-procs (cons (list procname (eval procname)) saved-procs))
  (eval `(set! ,procname
	       (traced-proc (eval procname) procname))))


;;; new, for use with internal procedures in code
(defmacro (ptrace procname)
  `(set! ,procname
	 (trace-encapsulate ,procname)))

(define (untrace procname)
  (restore-original procname))

; +++ this should be a hashtable

(define saved-procs '())

(define (save-original procname)
  (set! saved-procs
	(cons `(,procname ,(eval procname))
	      saved-procs)))

(define (restore-original procname)
  (eval `(set! ,procname
	       (cadr (assq procname saved-procs)))))

(define (untrace-all)
  (for-each (lambda (entry)
	      (restore-original (car entry)))
	    saved-procs))