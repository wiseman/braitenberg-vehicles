;;; Browser interface

;;; this lets you control an external web browser from Java (crudely, through
;;; the DOS command line or shell). It illustrates Skij's interface to the
;;; shell, string manipulation, and use of the reflection APIs.

; example of use:
;  (view-method-documentation 'java.awt.Window 'show)

; ****************************************************************

; this variable should point to a local copy of the executable for the Netscape browser
; (or another browser that accepts command-line arguments)
(define *netscape-cmd* "d:\\Program Files\\Netscape\\Communicator\\Program\\netscape.exe ")


(define (browse-url url)
  (shell-exec (string-append *netscape-cmd* url)
	      (current-output-port)))

(define *javadoc-url* "http://java.sun.com/products/jdk/1.1/docs/api/")

; if you just have a class and method name, use this
(define (method-name-doc-url class method-name)
  (set! method-name (to-string method-name))
  (set! class (if (symbol? class) (class-named class) class))
  (let ((methods (filter (lambda (method) (equal? method-name (invoke method 'getName)))
			  (vector->list (invoke class 'getMethods))))
	(classes '())
	(declaring-class #f))
    (let loop ((rest methods))
      (unless (null? rest)
	      (pushnew (invoke (car rest) 'getDeclaringClass) classes)
	      (loop (cdr rest))))
    (if (null? classes)
	(error `(no method named ,method-name for class ,class))
	(set! declaring-class (most-specific-class classes)))
    (string-append *javadoc-url* (invoke declaring-class 'getName) ".html#" method-name)))

;;; if you have a method or constructor object, use this
(define (method-doc-url method)
  (define param-types (vector->list (invoke method 'getParameterTypes)))  
  (define buf (new 'java.lang.StringBuffer))
  (define constructor? (instanceof method 'java.lang.reflect.Constructor))
  (define (app string) (invoke buf 'append string))
  (app *javadoc-url*)
  (app (invoke (invoke method 'getDeclaringClass) 'getName))
  (app ".html#")
  (if constructor?
      (app (last (parse-substrings (invoke method 'getName) (char->int #\.))))
      (app (invoke method 'getName)))
  (app "(")
  (define nparams (length param-types))
  (for-each (lambda (param)
	      (app (abbreviate-class-name param #f #f))
	      (set! nparams (- nparams 1))
	      (if (not (= nparams 0))
		  (app ", ")))
	    param-types)
  (app ")")
  (to-string buf))

(define (abbreviate-class-name class relative-to use-java.lang?)
  (if (invoke class 'isArray)
      (string-append (abbreviate-class-name (invoke class 'getComponentType) relative-to use-java.lang?) "[]")
      (begin
	(define name (invoke class 'getName))
	(define parts (parse-substrings name 46))
	(cond ((and use-java.lang?
		    (equal? (car parts) 'java)
		    (equal? (cadr parts) 'lang)
		    (null? (cdddr parts)))
	       (last parts))
	      (relative-to
	       (define rparts (parse-substrings (invoke relative-to 'getName) 46))
	       (define package (butlast parts))
	       (define rpackage (butlast rparts))
	       (if (equal? package rpackage)
		   (last parts)
		   name))
	      (#t name)))))

(define (most-specific-class classes)
  (cond ((null? (cdr classes))
	 (car classes))
	((invoke (car classes) 'isAssignableFrom (cadr classes))
	 (most-specific-class (cdr classes)))
	(#t (most-specific-class (cons (car classes) (cddr classes))))))

(define (view-method-documentation class method)
  (browse-url (method-name-doc-url class method)))
