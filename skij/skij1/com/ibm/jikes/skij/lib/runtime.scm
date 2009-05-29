;;; This file is part of Skij.
;;; Author: Michael Travers (mt@watson.ibm.com)

;;; Licensed Materials - See the file license.txt.
;;; (c) Copyright IBM Corp. 1997, 1998. All rights reserved.

; I assume there is always exactly one of these?
(define *runtime* (invoke-static 'java.lang.Runtime 'getRuntime))

; execute a shell command and dump output to current-output-port.
; Note: error output will be ignored.
(define (shell command)
  (shell-exec command (current-output-port)))

(define (shell-to-string command)
  (with-string-output-port
   (lambda (out)
     (shell-exec command out))))

(define (shell-exec command out)
  (define p (invoke *runtime* 'exec command))
  (define s (invoke-static 'com.ibm.jikes.skij.misc.Kludge 'processInputStream p))
  (define ss (new 'com.ibm.jikes.skij.InputPort s))
  (define e (invoke-static 'com.ibm.jikes.skij.misc.Kludge 'processErrorStream p))
  (define es (new 'com.ibm.jikes.skij.InputPort e))
  (copy-until-eof ss out)
  (define errstring
    (with-string-output-port 
     (lambda (error)
       (copy-until-eof es error))))
  (if (> (string-length errstring) 0)
      (error errstring)))

(define (gc)
  (invoke *runtime* 'gc))

(define (room)
  (invoke *runtime* 'freeMemory))