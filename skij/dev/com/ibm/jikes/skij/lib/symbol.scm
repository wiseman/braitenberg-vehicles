;;; This file is part of Skij.
;;; Author: Michael Travers (mt@watson.ibm.com)

;;; Licensed Materials - See the file license.txt.
;;; (c) Copyright IBM Corp. 1997, 1998. All rights reserved.

(define (symbol? thing)
  (instanceof thing 'com.ibm.jikes.skij.Symbol))

(define (symbol->string sym)
  (peek sym 'name))

(define (string->symbol name)
  (invoke-static 'com.ibm.jikes.skij.Symbol 'intern name))

; old habits die hard
(define intern string->symbol)

(define (symbol-conc . args)
  (intern (apply string-append (map to-string args))))

; local extension: returns #t or #f
(defmacro (bound? symbol)
  `(not (%%null? (invoke (current-environment) 'getBindingSafe ,symbol))))
