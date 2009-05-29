;;; This file is part of Skij.
;;; Author: Michael Travers (mt@watson.ibm.com)

;;; Licensed Materials - See the file license.txt.
;;; (c) Copyright IBM Corp. 1997, 1998. All rights reserved.

; interface to unix rsh protocol

; host, user, password are strings
; out is an output port (ie, (current-output-port))
(define (rsh cmd host user password out)
  (define conn (open-tcp-conn host 512)) ;exec protocl
  (define stream (conn 'out))
  (define (net-out string)
    (invoke stream 'write string)
    (write-char #,(integer->char 0) stream))
  (net-out "")
  (net-out user)
  (net-out password)
  (net-out cmd)
  (read-char (conn 'in))
  (copy-until-eof (conn 'in) out))





