;;; This file is part of Skij.
;;; Author: Michael Travers (mt@watson.ibm.com)

;;; Licensed Materials - See the file license.txt.
;;; (c) Copyright IBM Corp. 1997, 1998. All rights reserved.

;use display for listener. For real scheme printing, (require-write)

;(define write display)

(define (require-write)
  (if (eq? write display)
      (load-resource "lib/write.scm")))

;;; these essential procedures are defined here so they can
;;; be used by init code

(define (member thing list)
  (if (null? list) #f
      (if (equal? thing (car list))
	  list
	  (member thing (cdr list)))))

(define string-append
  (lambda args
    (define buf (new 'java.lang.StringBuffer))
    (for-each (lambda (string)
		(invoke buf 'append string))
	      args)
    (to-string buf)))

(define *loaded-resources* '())

;;; called by Scheme.loadResource
(define (provide-resource resource)
  (set! *loaded-resources* (cons resource *loaded-resources*)))

(define (load-resource resource-name . from)
  (set! resource-name (to-string resource-name))	;canonicalize
  (set! from (if (null? from) "com.ibm.jikes.skij.Scheme" (car from)))
  (invoke-static 'com.ibm.jikes.skij.Scheme 'loadResource resource-name from)
  resource-name)

(define (require-resource resource-name)
  (set! resource-name (to-string resource-name))
  (%synchronized
   '*loaded-resources*
   (lambda ()
     (if (member resource-name *loaded-resources*)
	 #t
	 (load-resource resource-name)))))

; older interface
(define (require package)
  (set! package (to-string package)) ;canonicalize
  (require-resource (string-append "lib/" package ".scm")))

;;; Autoloading
  
(invoke-static 'com.ibm.jikes.skij.TopEnvironment 'initAutoload 
	       '((class-supports-readLine? "io") (output-port? "io") (input-port? "io") (read-line "io") (copy-until-eof "io") (eof-object? "io") (close-output-port "io") (close-input-port "io") (print "io") (newline "io") (char-ci=? "chars") (char-ci<=? "chars") (char<=? "chars") (char-ci>=? "chars") (char>=? "chars") (char-ci>? "chars") (char>? "chars") (char-ci<? "chars") (char<? "chars") (def-char-compare "chars") (char-lower-case "chars") (char=? "chars") (char-whitespace? "chars") (char-numeric? "chars") (char-alphabetic? "chars") (char-upper-case? "chars") (char-lower-case? "chars") (char-downcase "chars") (char-upcase "chars") (int->char "chars") (char->int "chars") (char->integer "chars") (char=? "chars") (character? "chars") (restore-all-procedures "encapsulate") (restore-procedure "encapsulate") (save-procedure "encapsulate") (encapsulate "encapsulate") (for-all-files "files") (with-open-for-append "files") (directory "files") (call-with-output-file "files") (call-with-input-file "files") (open-output-file "files") (open-input-file "files") (generic-write "pp") (unmacro "pp") (process-tree "pp") (proc-body "pp") (proc-form "pp") (ppp "pp") (pp "pp") (args "proc") (procedure-environment "proc") (procedure-arity "proc") (procedure-arity-valid? "proc") (procedure? "proc") (set-proxy-host "tcp") (open-tcp-conn "tcp") (untrace-all "trace") (restore-original "trace") (save-original "trace") (untrace "trace") (ptrace "trace") (settrace "trace") (trace-encapsulate "trace") (traced-proc "trace") (traceout "trace") (tracein "trace") (traceprint "trace") (set-current-node "trace") (make-tracer "trace") (tree-add-mouse-listener "tree") (make-adaptor "tree") (generate-tree "tree") (set-open "tree") (node-parent "tree") (add-child "tree") (root-node "tree") (set-root "tree") (coerce-node "tree") (make-node "tree") (node? "tree") (make-tree "tree") (make-tree-panel "tree") (make-tree-window "tree") (repeat "anaphoric") (key "anaphoric") (acond "anaphoric") (awhen "anaphoric") (aand "anaphoric") (aif "anaphoric") (force "delay") (make-promise "delay") (delay "delay") (jsinvoke "javascript") (rsh "rsh") (make-font "menu") (set-menu-item-properties "menu") (make-menu-item "menu") (display-popup-menu "menu") (make-menu "menu") (make-popup-menu "menu") (decf "setf") (incf "setf") (deletef "setf") (pop "setf") (pushnew "setf") (push "setf") (setf "setf") (def-setf "setf") (nconc "lists") (remove-duplicates "lists") (ndelete "lists") (delete "lists") (last "lists") (butlast "lists") (filter "lists") (filter-out "lists") (last-cdr "lists") (position "lists") (list-ref "lists") (list-tail "lists") (append1 "lists") (append "lists") (sort "lists") (reverse "lists") (%ass "lists") (assoc "lists") (assv "lists") (assq "lists") (memv "lists") (memq "lists") (length "lists") (list? "lists") (cadddr "lists") (cdddr "lists") (cddar "lists") (cdadr "lists") (cdaar "lists") (caddr "lists") (cadar "lists") (caadr "lists") (caaar "lists") (cddr "lists") (cdar "lists") (caar "lists") (java-backtrace "java") (backtrace-inspect "java") (method-apropos "java") (coerce-class "java") (get-method "java") (ignore-errors-and-warn "java") (ignore-errors "java") (start-application "java") (exit "java") (enumeration->list "java") (for-enumeration "java") (map-enumeration "java") (error "java") (catch "java") (%or-null "java") (new-cl "classloader") (class-from-paths "classloader") (cl-class-named "classloader") (make-class-loader "classloader") (graph-inspect "graph") (ttrace "textrace") (trace-encapsulate "textrace") (proc-name "textrace") (with-traceprint "textrace") (traceprint "textrace") (macro "macro") (macroexpand "macro") (defmacro "macro") (defmacro "macro") (make-font "jmenu") (make-jmenu-item "jmenu") (display-jpopup-menu "jmenu") (make-jpopup-menu "jmenu") (string-from-user1 "dialogs") (string-from-user "dialogs") (write "write") (unless "control") (when "control") (not "control") (eqv? "control") (case "control") (cond "control") (define-memoized "memoize") (fluid-let "let") (do "let") (let* "let") (letrec "let") (named-let "let") (let "let") (dynamic "dynamic") (eval-quasi "quasi") (where-is "apropos") (for-bindings "apropos") (apropos "apropos") (defstruct "defstruct") (inspect-data "describe") (describe "describe") (bound? "symbol") (symbol-conc "symbol") (intern "symbol") (string->symbol "symbol") (symbol->string "symbol") (symbol? "symbol") (pythagorean "amb") (square? "amb") (integers "amb") (let-amb "amb") (call/cc "amb") (fail "amb") (aset "array") (aref "array") (list->int-vector "array") (%make-array "array") (containers "swing") (lists->array "swing") (make-table-panel "swing") (make-table "swing") (make-swing-button "swing") (make-swing-window-for-panel "swing") (make-swing-window "swing") (swing-class "swing") (class-exists? "swing") (now-string "time") (with-timeout "time") (time "time") (now "time") (inspect-components "inspect") (inspect-tree "inspect") (inspect-class-methods "inspect") (inspect "inspect") (jive-inspect "inspect") (hashtable-contents "hashtable") (for-hashtable "hashtable") (map-hashtable "hashtable") (clear-hashtable "hashtable") (hashtable-lookup "hashtable") (hashtable-remove "hashtable") (hashtable-put "hashtable") (hashtable-get "hashtable") (identity "hashtable") (make-hashtable "hashtable") (make-interrupter "interrupt") (make-listener*** "listener") (make-applet-listener "listener") (make-listener** "listener") (make-awt-listener-window "listener") (make-swing-listener-window "listener") (line-end? "listener") (lcm "numeric") (gcd "numeric") (modulo "numeric") (quotient "numeric") (expt "numeric") (ceiling "numeric") (ceil "numeric") (atan "numeric") (acos "numeric") (atan "numeric") (floor "numeric") (log "numeric") (exp "numeric") (sqrt "numeric") (def-math "numeric") (double "numeric") (float "numeric") (short "numeric") (byte "numeric") (long "numeric") (abs "numeric") (odd? "numeric") (even? "numeric") (modulo "numeric") (string->number "numeric") (number->string "numeric") (positive? "numeric") (negative? "numeric") (zero? "numeric") (inexact->exact "numeric") (exact->inexact "numeric") (inexact? "numeric") (exact? "numeric") (real? "numeric") (integer? "numeric") (rational? "numeric") (real? "numeric") (complex? "numeric") (number? "numeric") (%fill-vector "vector") (%make-vector "vector") (memq-vector "vector") (for-vector "vector") (map-vector "vector") (vector "vector") (make-vector "vector") (list->vector "vector") (vector->list "vector") (room "runtime") (gc "runtime") (shell-exec "runtime") (shell-to-string "runtime") (shell "runtime") (start-http-service "server") (simple-client "server") (remote-eval "server") (make-eval-server "server") (local-host "server") (make-server "server") (list->string "string") (string->list "string") (parse-substrings "string") (string-ref "string") (string-ci=? "string") (string-ci<=? "string") (string<=? "string") (string-ci>=? "string") (string>=? "string") (string-ci>? "string") (string>? "string") (string-ci<? "string") (string<? "string") (def-string-compare "string") (string=? "string") (string-length "string") (read-from-string "string") (with-input-from-string "string") (write-to-string "string") (display-to-string "string") (with-string-output-port "string") (string-replace-string "string") (string-replace "string") (string-search "string") (string-trim "string") (substring "string") (string-set! "string") (string-ref "string") (make-string "string") (string "string") (install-null-security-manager "applet") (run-applet "applet") (make-button "window") (make-refreshed-window "window") (add-window-close-handler "window") (primp-window "window") (make-window-for-panel "window") (make-window "window") (thread-inspector "thread") (sleep "thread") (user-threads "thread") (all-threads "thread") (current-thread "thread") (in-own-thread "thread") (run-in-thread "thread") (synchronized "thread") (arand "random") (random-range "random") (random "random")))

;;; Load a user init file if there is one

(define (system-property name)
  (invoke-static 'java.lang.System 'getProperty name))
    
;;; this set of things is used in many early-loading files (write, quasi)
;;; so define them here to save trouble. 

;;; moved to primitives
;(define (vector? thing)
;  (invoke (invoke thing 'getClass) 'isArray))

;;; expressed this way to avoid loading macro and quasi right away
(define instanceof
  (new 'com.ibm.jikes.skij.Macro
       (current-environment)
       'instanceof 
       '(ignore thing class)
       '(begin 
	  (list '%instanceof
		thing
		(if (and (list? class) 
			 (eq? (car class) 'quote)
			 (invoke '#,(class-named 'com.ibm.jikes.skij.Symbol)
				 'isInstance
				 (cadr class))) ;using symbol? makes this macro recursive...
		    (list 'quote (class-named (eval class)))
		    class)
		))))

(define (boolean? x)
  (instanceof x 'java.lang.Boolean))

(define (char? thing)
  (instanceof thing 'java.lang.Character))

(define (string? x)
  (instanceof x 'java.lang.String))

;;; load user init file (unless in an applet or initForm supplied)
(if (and (%%null? (invoke (global-environment) 'getBindingSafe '*applet*))
	 (%%null? (peek-static 'com.ibm.jikes.skij.Scheme 'initForm)))
    (%catch
     (lambda () 
       (load (string-append (system-property "user.home")
			    (system-property "file.separator")
			    ".skij")))))

;;; Support for backtrace
(define (backtrace . exception)
  (define (print-bt l)
    (unless (null? l)
	    (print-bt (cdr l))
	    (display "\n-> ")
	    (display (car l))))
  (set! exception (if (null? exception)
		      (peek-static 'com.ibm.jikes.skij.SchemeException 'lastForUser)
		      (car exception)))
  (display "Backtrace for ")
  (display exception)
  (print-bt (peek exception 'backtrace))
  (display "\n")
  (%null))

(define java2?
  (peek-static 'com.ibm.jikes.skij.util.Dynvoke
	       'java2))

