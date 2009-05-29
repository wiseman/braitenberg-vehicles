;;; This file is part of Skij.
;;; Author: Michael Travers (mt@watson.ibm.com)

;;; Licensed Materials - See the file license.txt.
;;; (c) Copyright IBM Corp. 1997, 1998. All rights reserved.

(define (open-tcp-conn host port)
  (define sock (new 'java.net.Socket host port))
  (define in (new 'com.ibm.jikes.skij.InputPort (invoke sock 'getInputStream)))
  (define out (new 'com.ibm.jikes.skij.OutputPort (invoke sock 'getOutputStream)))
  (lambda args
    (define op (car args))
    (case op
      ((in) in)
      ((out) out)
      (#t (error "foo")))))

(define (set-proxy-host host)
  (invoke (invoke-static 'java.lang.System 'getProperties)
	  'put
	  "socksProxyHost" host))

;(set-proxy-host "socks.watson.ibm.com")

