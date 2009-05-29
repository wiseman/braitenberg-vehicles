;;;-*- Mode: Lisp; Package: VEHICLES -*-
;;----------------------------------------------------------------------
;; 
;; Copyright:   Copyright (c) 1999 John Wiseman
;; File:        probe.lisp
;; Created:     14 June 1998
;; Author:      John Wiseman (jjwiseman@gmail.com)
;; 
;; Description: 
;; 
;; Changes:     
;; 
;;----------------------------------------------------------------------

(in-package "VEHICLES")


(defun make-probe (world node kind)
  (let ((name (name-of node)))
    (flet ((report (value)
             (format T "~&[~S] Node ~A: ~S" (world-time world) name value)))
      (ecase kind
        ((:change)
         (let ((old-value (cons nil nil)))
           #'(lambda (node)
               (let ((value (output node)))
                 (when (not (eql old-value value))
                   (setf old-value value)
                   (report value))))))
        ((:continuous)
         #'(lambda (node)
             (report (output node))))))))
         


(defun str-lookup (var bindings)
  "Looks up the value of a variable in a binding environment."
  (cdr (assoc (string var) bindings :key #'string :test #'equalp)))

(defun attach-probe (world platform node &optional (kind :change))
  (let ((platform-name (string platform))
        (node-name (string node)))
    (dolist (p (append (world-vehicles world)
                       (world-lamps world)))
      (when (equalp (string (name-of p)) platform-name)
        (let ((node (str-lookup node-name (nodes p))))
          (if (null node)
            (error "Unknown node ~S in ~S." node p)
            (if (functionp kind)
              (push kind (probes node))
              (push (make-probe world node kind) (probes node)))))))))

