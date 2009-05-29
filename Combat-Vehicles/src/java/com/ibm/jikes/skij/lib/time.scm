;;; This file is part of Skij.
;;; Author: Michael Travers (mt@watson.ibm.com)

;;; Licensed Materials - See the file license.txt.
;;; (c) Copyright IBM Corp. 1997, 1998. All rights reserved.

;;; this truncates the longword time value, but it seems to work anyway.
;;; thunk has to return a value, not sure what to do about that.
(define (now)
  (invoke-static 'java.lang.System 'currentTimeMillis))  

(define (time thunk)
  (define start (now))
  (define result (thunk))
  (define end (now))
  (display '"Execution time (msec): " )
  (display (- end start))
  (newline)
  result)

(defmacro (with-timeout time . body)
  `(let* ((result ':timed-out)
	  (work-thread (in-own-thread (set! result (catch ,@body))))
	  (timer-thread (in-own-thread
			 (catch 
			  (invoke work-thread 'join ,time)
			  (if (invoke work-thread 'isAlive)
			      (begin (print `(,work-thread timed out))
				     (invoke work-thread 'stop))))))) 
     (catch (invoke work-thread 'join))
     (invoke timer-thread 'stop)
     (if (instanceof result 'java.lang.Throwable)
	 (throw result))
     result))

; +++ timezone screwup
(define date-format (invoke-static 'java.text.DateFormat 'getInstance))
(invoke date-format 'setTimeZone (invoke-static 'java.util.TimeZone 'getDefault))

(define (now-string)
  (invoke date-format 'format (now)))  

     
     
   
     

