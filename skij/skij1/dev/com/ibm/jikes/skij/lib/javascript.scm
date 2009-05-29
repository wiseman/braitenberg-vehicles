;;; This file is part of Skij.
;;; Author: Michael Travers (mt@watson.ibm.com)

;;; Licensed Materials - See the file license.txt.
;;; (c) Copyright IBM Corp. 1997, 1998. All rights reserved.

;;; this code only works while running as applet under Netscape.
;;; +++ is there an IE equivalent?

(define *javascript-window*
  (ignore-errors			;needed for autoload machinations...
   (invoke-static 'netscape.javascript.JSObject 'getWindow *applet*)))

(define (jsinvoke jsobject method . args)
  (invoke jsobject 'call (string method) (list->vector args)))


