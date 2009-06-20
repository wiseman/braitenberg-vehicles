;; --------------------
;; Patches and improvements for Skij.
;; --------------------

;; -----
;; textrace.scm
;; -----

(define (traceprint msg)
  (display
   (with-string-output-port
     (lambda (port)
       (newline port)
       (let loop ((i (dynamic *trace-level*)))
         (unless (zero? i)
           (display "  " port)
           (loop (- i 1))))
       (write msg port)))))

(define *ttraced-procs* (make-hash-table))

(defmacro (ttrace procname)
  `(begin
    (setf (gethash ',procname *ttraced-procs*) ,procname)
    (set! ,procname
	  (trace-encapsulate ,procname))))

(defmacro (unttrace procname)
  (let ((old-proc-var (gentemp)))
  `(let ((,old-proc-var (gethash ',procname *ttraced-procs*)))
     (when ,old-proc-var
       (set! ,procname ,old-proc-var)
       (remhash ',procname *ttraced-procs*)))))


;; -----
;; setf.scm
;; -----

(defmacro (setf . forms)
  (if (= (length forms) 2)
    (let ((place (car forms))
          (value (cadr forms))
          (val-var (gentemp)))
      (cond ((symbol? place)
             `(let ((,val-var ,value))
                (set! ,place ,val-var)
                ,val-var))
            ((pair? place)
             (let ((proc (hashtable-get setf-ht (car place) #f)))
               (if proc
                 `(let ((,val-var ,value))
                    ,(apply proc val-var place)
                    ,val-var)
                 (error `(cannot setf ,place)))))
            (#t (error `(cannot setf ,place)))))
    `(begin
      (setf ,(car forms) ,(cadr forms))
      (setf ,@(cddr forms)))))



;; -----
;; swing.scm
;; -----

(define (class-exists? class-name)
  (instanceof (catch (class-named class-name)) 'java.lang.Class))

(define swing-package
  (cond ((class-exists? 'javax.swing.JFrame)
         'javax.swing)
        ((class-exists? 'com.sun.java.swing.JFrame)
         'com.sun.java.swing)
        ((class-exists? 'java.awt.swing.JFrame)
         'java.awt.swing)
        (error "Swing classes not found.")))

         

;; -----
;; jmenu.scm
;; -----

(define (make-jpopup-menu menu-items)
  (define menu (new (swing-class 'JPopupMenu)))
  (for-each (lambda (menu-item)
	      (invoke menu 'add menu-item))
	    menu-items)
  menu)

(define (make-jmenu-item title . keys)
  (key keys procedure #f)
  (key keys enabled? #t)
  (key keys bold? #f)
  (define menu-item (new (swing-class 'JMenuItem) title))
  (if procedure
      (invoke menu-item 'addActionListener 
	      (new 'com.ibm.jikes.skij.misc.GenericCallback procedure)))
  (invoke menu-item 'setEnabled enabled?)
  (if bold?
      (invoke menu-item 'setFont
	      (make-font (invoke menu-item 'getFont) #f (peek-static 'java.awt.Font 'BOLD)  #f)))
  menu-item)


;; -----
;; thread.scm
;; -----


(define (thread-inspector)
;  (require 'swing)
  (define bigpanel (new (swing-class 'JPanel)))
  (invoke bigpanel 'setLayout (new 'java.awt.GridLayout 0 1))
  (for-each (lambda (thread)
	      (define smallpanel (new 'com.sun.java.swing.JPanel))
	      (invoke smallpanel 'add (new (swing-class 'JTextArea) (invoke thread 'getName)))
	      (invoke smallpanel 'add (make-button 'Suspend (lambda (evt) (invoke thread 'suspend))))
	      (invoke smallpanel 'add (make-button 'Resume (lambda (evt) (invoke thread 'resume))))
	      (invoke smallpanel 'add (make-button 'Interrupt (lambda (evt) (invoke thread 'interrupt))))
	      (invoke bigpanel 'add smallpanel))
	    (user-threads))
  (make-window 'Threads bigpanel))
