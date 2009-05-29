(load "org/heavymeta/vehicles/lib/common-lisp.scm")

(define (directory? path)
  (let ((f (new 'java.io.File path)))
    (invoke f 'isDirectory)))

(define (rdirectory dir extension)
  (rdirectory-aux (make-extension-filter extension) '() (list dir)))

(define (make-extension-filter ext)
  (lambda (f)
    (invoke f 'endsWith ext)))


(define (rdirectory-aux filter done to-go)
  (if (null to-go)
    done
    (let ((current (car to-go)))
      (if (directory? current)
        (rdirectory-aux filter
                        done
                        (append (cdr to-go)
                                (mapcar (lambda (f)
                                          (string-append current "/" f))
                                        (directory current))))
        (if (funcall filter current)
          (rdirectory-aux filter (cons current done) (cdr to-go))
          (rdirectory-aux filter done (cdr to-go)))))))


(define (compile)
  (let ((files (rdirectory "org" ".java")))
    (javac files)))


                    
(define (javac args)
  (let ((c (new 'sun.tools.javac.Main (peek-static 'java.lang.System 'out)
                "javac"))
        (f (%make-array (list (+ (length args) 0)) (class-named 'java.lang.String)))
        (a (class-named 'java.lang.reflect.Array)))
    (let ((i 0))
      (dolist (file args)
        (invoke-static a 'set f i file)
        (incf i)))
    (invoke c 'compile f)))

  
    