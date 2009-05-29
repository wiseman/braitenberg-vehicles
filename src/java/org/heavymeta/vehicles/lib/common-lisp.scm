;; -----
;;
;; Add a thin Common Lisp veneer to Skij.
;;
;; -----

;;; Make Skij's setf more like CL's:
;;; Return a value and accept multiple <place> <new value> pairs.
;
;;(require 'setf)
;
;(defmacro (setf . forms)
;  (if (= (length forms) 2)
;    (let ((place (car forms))
;          (value (cadr forms)))
;      (cond ((symbol? place)
;             (let ((val (gentemp)))
;               `(let ((,val ,value))
;                  (set! ,place ,val)
;                  ,val)))
;            ((pair? place)
;             (let ((val (gentemp)))
;               `(let ((,val ,value))
;                  ,(apply (hashtable-get setf-ht
;                                         (car place)
;                                         (lambda (v p) (error `(no setf method for ,p))))
;                          val place)
;                  ,val)))
;            (#t (error `(cant setf ,place)))))
;    `(begin
;      (setf ,(car forms) ,(cadr forms))
;      (setf ,@(cddr forms)))))


;; A CL defun that won't even choke on document strings.

(defmacro (defun name args . body)
  (if (and (> (length body) 1)
           (string? (car body)))
    `(define ,(cons name args) ,@(cdr body))
    `(define ,(cons name args) ,@body)))

(defmacro (defparameter symbol value)
 `(define ,symbol ,value))

(defmacro (defvar symbol value)
  `(when (not (bound? ',symbol))
     (define ,symbol ,value)))

(defun symbol-name (symbol)
  (symbol->string symbol))

(defun symbolp (thing)
  (symbol? thing))

(defun first (list)
  (car list))

(defun second (list)
  (cadr list))

(defmacro (dolist iter . body)
 (let ((var (first iter))
       (list (second iter))
       (list-var (gentemp)))
   `(let ((,var '()))
      (do ((,list-var ,list (cdr ,list-var)))
          ((null? ,list-var))
        (set! ,var (car ,list-var))
        ,@body))))

(defmacro (dotimes iter . body)
 (let ((var (first iter))
       (max-expr (second iter))
       (max-var (gentemp)))
   `(let ((,max-var ,max-expr))
      (do ((,var 0 (+ ,var 1)))
          ((>= ,var ,max-var))
        ,@body))))

(defmacro (progn . body)
  `(begin ,@body))

(define (acons key datum a-list)
  (cons (cons key datum) a-list))

(defun null (thing)
  (or (null? thing)
      (eq? #f thing)))

(defun listp (thing)
  (list? thing))

(defun funcall (fn . args)
  (apply fn args))


;; We do the best we can for MEMBER and FIND without
;; actually having keyword arguments.

(defun member-key-test (thing list key test)
  (do ((l list (cdr l)))
      ((or (null? l)
           (funcall test thing (funcall key (car l))))
       (if (null? l)
         #f
         l))))

(defun member (thing list)
  (member-key-test thing list identity eqv?))

         
(defun find-key-test (thing list key test)
  (let ((result (member-key-test thing list key test)))
    (if result
      (car result)
      #f)))

(defmacro (assert expr)
 `(when (not ,expr)
    (error ,(string-append "Assertion failed: "
                           (write-to-string expr)))))

(defun every (fun list)
  (if (null list)
    #t
    (if (funcall fun (car list))
      (every fun (cdr list))
      #f)))

(defun mapcar (fn list)
  (map fn (or list '())))

(defun mapcan (fn list)
  (apply append (map fn list)))


(defmacro (labels fns . body)
 `(letrec ,(mapcar (lambda (fn)
                     (let ((name (car fn))
                           (args (cadr fn))
                           (body (cddr fn)))
                       `(,name (lambda ,args ,@body))))
                   fns)
          ,@body))

(defmacro (flet fns . body)
 `(let ,(mapcar (lambda (fn)
                  (let ((name (car fn))
                        (args (cadr fn))
                        (body (cddr fn)))
                    `(,name (lambda ,args ,@body))))
                fns)
    ,@body))

(defun atom (thing)
  (not (pair? thing)))

(defmacro (declare . things)
  #f)

(define (warn . things)
  (print (string-append "; Warning: " (stringify things))))

(defun typep (thing class-name)
  (let ((class1 (class-of thing))
        (class2 (class-named class-name)))
    (invoke class2 'isAssignableFrom class1)))

(defun make-hash-table ()
  (make-hashtable))

(defun gethash (key table)
  (hashtable-get table key #f))

(def-setf (gethash key table) value
  `(hashtable-put ,table ,key ,value))

(defun remhash (key table)
  (hashtable-remove table key))


(define pi 3.141592653589793)




;; "Lisp Error" -> lerror

(define (lerror . things)
  (error (stringify things)))

(defun stringify (things)
  (apply string-append
         (map (lambda (x)
                (if (string? x)
                  x
                  (write-to-string x)))
              things)))


(define (make-instance class . slots)
  (let ((obj (new class)))
    (mpoke obj slots)
    obj))

(define (mpoke obj pokes)
  (do ((pokes pokes (cddr pokes)))
      ((null? pokes))
    (poke obj (first pokes) (second pokes))))


(defun string-upcase (s)
  (list->string (mapcar (lambda (c)
                          (char-upcase c))
                        (string->list s))))

(define gentemp gensym)
