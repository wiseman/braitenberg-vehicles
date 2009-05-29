;;; This file is part of Skij.
;;; Author: Michael Travers (mt@watson.ibm.com)

;;; Licensed Materials - See the file license.txt.
;;; (c) Copyright IBM Corp. 1997, 1998. All rights reserved.

;;; Network servers.

;;; Make a server. Handler is a 1-arg procedure, called with the socket
;;; when a connection is made. Note that listener and connections are
;;; each in their own thread.
(define (make-server port handler)
  (define server-socket (new 'java.net.ServerSocket port))
  (in-own-thread
   (let loop ()
     (define socket (invoke server-socket 'accept))
     (in-own-thread (handler socket))
     (loop))))

(define (local-host)
  (invoke-static 'java.net.InetAddress 'getLocalHost))
  
; warning: running this creates a big fat security hole
(define (make-eval-server port)
  (make-server 
   port
   (lambda (socket)
     (define in (new 'com.ibm.jikes.skij.InputPort (invoke socket 'getInputStream)))
     (define form (read in))
     (print (string-append '"EVAL server: " (to-string form)))
     (define out (new 'com.ibm.jikes.skij.OutputPort (invoke socket 'getOutputStream)))
     (define result (eval form))
     (print (string-append '"--> " (to-string result)))
     (write result out)
     (newline out)
     (invoke socket 'close))))

(define (remote-eval host port form)
  (define socket (new 'java.net.Socket host port))
  (define out (new 'com.ibm.jikes.skij.OutputPort (invoke socket 'getOutputStream)))
  (write form out)
  (newline out)
  (define in (new 'com.ibm.jikes.skij.InputPort (invoke socket 'getInputStream)))
  (read in))

; a client for simple, 1-stroke put/get protocols
(define (simple-client host port argstring)
  (define socket (new 'java.net.Socket host port))
  (define out (new 'com.ibm.jikes.skij.OutputPort (invoke socket 'getOutputStream)))
  (write argstring out)
  (newline out)
  (define in (new 'com.ibm.jikes.skij.InputPort (invoke socket 'getInputStream)))
  (with-string-output-port 
   (lambda (stringp)
     (copy-until-eof in stringp))))

;;; start a minimal http server.
(define (start-http-service port proc)
  (make-server 
   port
   (lambda (socket)
     (define in (new 'com.ibm.jikes.skij.InputPort (invoke socket 'getInputStream)))
     (define command-line (read-line in))
     (define url-start (+ 1 (invoke command-line 'indexOf 32)))
     (define url-end (invoke command-line 'indexOf 32 url-start))
     (define url (substring command-line url-start url-end))
     (define command (substring command-line 0 url-start))
     (define headers '())
     (let loop ((line (read-line in)))
       (unless (= 0 (string-length line))
         (push line headers)
	 (loop (read-line in))))
     (define out (new 'com.ibm.jikes.skij.OutputPort (invoke socket 'getOutputStream)))
     (proc out command url headers socket))))
