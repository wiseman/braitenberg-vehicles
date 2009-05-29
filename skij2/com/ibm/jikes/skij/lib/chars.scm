;;; This file is part of Skij.
;;; Author: Michael Travers (mt@watson.ibm.com)

;;; Licensed Materials - See the file license.txt.
;;; (c) Copyright IBM Corp. 1997, 1998. All rights reserved.

; char? moved to write

(define character? char?)

(define char=? equal?)

;this can't be a reliable implementation, but I can't find a better one.
(define (char->integer char)
  (invoke char 'hashCode))		

; integer->char is a primitive

; some code uses these nonstandard names
(define char->int char->integer)
(define int->char integer->char)       

(define (char-upcase char)
  (invoke-static 'java.lang.Character 'toUpperCase char))

(define (char-downcase char)
  (invoke-static 'java.lang.Character 'toLowerCase char))

(define (char-lower-case? char)
  (invoke-static 'java.lang.Character 'isLowerCase char))

(define (char-upper-case? char)
  (invoke-static 'java.lang.Character 'isUpperCase char))
  
(define (char-alphabetic? char)
  (invoke-static 'java.lang.Character 'isLetter char))

(define (char-numeric? char)
  (invoke-static 'java.lang.Character 'isDigit char))

(define (char-whitespace? char)
  (invoke-static 'java.lang.Character 'isWhitespace char))

(define (char=? c1 c2)
  (equal? c1 c2))

(define (char-lower-case ch)
  (invoke-static 'java.lang.Character 'toLowerCase ch))

;;; These work only in JDK 1.2 or up
'(defmacro (def-char-compare comp)
  `(begin
     (define (,(symbol-conc 'char comp '?) s1 s2)
       (,comp (invoke s1 'compareTo s2) 0))
     (define (,(symbol-conc 'char-ci comp '?) s1 s2) 
       (,comp (invoke (char-lower-case s1)
		      'compareTo
		      (char-lower-case s2))))))

(defmacro (def-char-compare comp)
  `(begin
     (define (,(symbol-conc 'char comp '?) s1 s2)
       (,comp (char->integer s1) (char->integer s2)))
     (define (,(symbol-conc 'char-ci comp '?) s1 s2) 
       (,comp (char->integer (char-lower-case s1))
	      (char->integer (char-lower-case s2))))))

(def-char-compare <)
(def-char-compare >)
(def-char-compare >=)
(def-char-compare <=)

(define (char-ci=? s1 s2)
  (invoke (char-lower-case s1) 'equals (char-lower-case s2)))
