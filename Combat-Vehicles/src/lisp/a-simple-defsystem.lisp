;;;-*- Mode: Lisp; Package: A-SIMPLE-DEFSYSTEM -*-
;;----------------------------------------------------------------------
;; 
;; File:        a-simple-defsystem.lisp
;; Created:     25 April 1996
;; Author:      Will Fitzgerald
;; 
;; Description: A simple defsystem
;; 
;; Changes:     
;;
;; jjw  7/11/96   operate-on-file methods for LOAD and COMPILE operations
;;		  check whether source file exists before comparing its
;;		  date to the binary file's.
;;
;; jjw  7/18/96   Made this-files-pathname work for ACL/unix according
;;		  to Alain's suggestion.  Now define 
;;		  *a-simple-defsystem-version* with defparameter.
;;
;; jjw  10/16/96  This-files-directory now calls translate-logical-pathname
;;		  on the source pathname before trying to extract the
;;		  directory.  The problem is that some .system files use
;;		  the directory without worrying about the logical host.
;;		  Therefore, this fix is a kludge and the right thing is to
;;		  either remove the this-files-directory function and let
;;		  users do their own manipulations of the pathname or add
;;		  this-files-logical-host as a new function.
;; 
;;----------------------------------------------------------------------

#|

Copyright 1997 Neodesic Corporation

SBIR Rights Notice

These SBIR data are furnished with SBIR rights under Contract No. NAS
9-19340.  For a period of 4 years after acceptance of all items to be
delivered under this contract, the Government agrees to use these data
for Government purposes only, and they shall not be disclosed outside
the Government (including disclosure for procurement purposes) during
such period without permission of the Contractor, except that, subject
to the foregoing use and disclosure prohibitions, such data may be
disclosed for use by support Contractors.  After the aforesaid 4-year
period the Government has a royalty-free license to use, and to
authorize others to use on its behalf, these data for Government
purposes, but is relieved of all disclosure prohibitions and assumes
no liability for unauthorized use of these data by third parties.
This Notice shall be affixed to any reproductions of these data, in
whole or in part.

Disclaimer of Warranty

These SBIR data are provided "AS IS" and without warranty of any kind and
Neodesic Corporation expressly disclaims all other warranties, express or
implied, including, but not limited to, the implied warranties of
merchantability, fitness for a particular purpose, title and
non-infringement.

|#



;; DEFINE-SYSTEM:
;; 
;; a function (not a macro!) for defining systems:
;; 
;; (ASD:DEFINE-SYSTEM name
;;   :HOME-DIRECTORY directory 
;;   :DIRECTORY directory
;;   :MODULES list-of-modules
;;   :USE-BIN-DIR t or nil (default: T)
;;   :PACKAGE package (default: *package*)
;;    )
;;
;; 
;; The combination of the HOME-DIRECTORY and the DIRECTORY
;; form the "root" of the system. Either or both of these
;; can be used. The function (THIS-FILES-DIRECTORY) can
;; be used to determine a system definition file's directory.
;; To be portable, the HOME-DIRECTORY and DIRECTORY should be
;; in list format (:ABSOLUTE .. ) or (:RELATIVE ...). If
;; both HOME-DIRECTORY and DIRECTORY are defined, then the
;; DIRECTORY should be RELATIVE to the HOME-DIRECTORY. The home
;; directory can be overridden in the LOAD-SYSTEM, COMPILE-SYSTEM,
;; etc. forms.
;;
;;A module is one of the following:
;;
;;  a file name with no extension. File names with extensions
;;    are not guaranteed to work.
;;    in which case the file is acted upon (ie, compiled, etc)
;;
;;  a file specification with the syntax
;;    (:FILE :NAME name :TYPE type :DIRECTORY dir)
;;    :TYPE and :DIRECTORY are optional. If :DIRECTORY is specified it
;;    should be a 'relative directory list,' that is, of the form
;;    '(:RELATIVE ...).  The file specification is merged with the root
;;    directory of the system.  The file is acted upon (ie, compiled, etc);
;; 
;;  a system name with the syntax:
;;    (:SYSTEM name)
;;    in which case the submodules are acted upon;
;; 
;;  a system location
;;    (:SYSTEM name location)
;;    in which case the file at location is loaded, and then the system is
;;    acted upon. The location has to have the same syntax as the file
;;    specification described above;
;;
;;  a conditional module list
;;    (name module+)
;;    in which name is the name of the conditional module list, and name is
;;    followed by any number of the allowed module specifications above.
;;
;;
;; USE-BIN-DIR means to use a separate binary directory created,
;; if necessary, at the same level as the system directory.
;; The name of the system is implementation dependent, and can
;; be retrieved with the function (BINARY-SUBDIRECTORY-NAME)
;; and can be set with (SET-BINARY-SUBDIRECTORY-NAME).
;;
;;The files, etc. are loaded or compiled into PACKAGE.
;; 
;; 
;; Examples:
;; 
;; perhaps in: CODE/LISP/UTILITIES/UTILITIES.SYSTEM
;;(ASD:DEFINE-SYSTEM "UTILITIES"
;;  :DIRECTORY (this-files-directory)
;;  :MODULES
;;  '("PIPES"
;;    "SHOW"
;;    "TEST")
;;  :PACKAGE "UTILITIES")
;;
;; perhaps in: CODE/LISP/MEMORY/MEMORY.SYSTEM
;;(ASD:DEFINE-SYSTEM "MEMORY"
;;  :DIRECTORY (this-files-directory)
;;  :MODULES
;;  '("CLASS-DEFINITIONS"
;;    "ACCESSORS"
;;    "MACROS")
;;  :PACKAGE "MEMORY")
;; 
;; perhaps in: CODE/LISP
;; 
;; (ASD:DEFINE-SYSTEM "TOP"
;;   :DIRECTORY (this-files-directory)
;;   :MODULES
;;   '((:SYSTEM "UTILITIES" '(:DIRECTORY "UTILITIES" :NAME "UTILITIES" :TYPE "SYSTEM"))
;;     (:SYSTEM "MEMORY" '(:DIRECTORY "MEMORY" :NAME "MEMORY" :TYPE "SYSTEM")))
;;   )
;;
;; 
;;(LOAD-SYSTEM name) ... loads system if it's not already loaded. 
;;  binary files are loaded if they are newer than source files,
;;  otherwise the source files are loaded.
;; 
;;(COMPILE-SYSTEM name)... compiles system
;;
;;Optional arguments are:
;;
;;(LOAD-SYSTEM name (force nil) (load-source nil) (verbose t) (test nil) (home-directory nil))
;;  :force, if t, will load all files again
;;  :load-source, if t, will load source files instead of binary files
;;  :verbose, if t (default), will print messages along the way
;;  :test, if t, will just print messages, not actually do anything
;;  :home-directory, if specified, supercedes system's home directory
;;
;;(COMPILE-SYSTEM name (force nil) (recompile nil) (and-load? nil) (verbose t) (test nil) (home-directory nil))
;;  :force, if t, will recompile source files newer than source files
;;  :recompile, if t, will recompile every file
;;  :and-load?, if t, will load the file after compiling it
;;  :verbose, if t (default), will print messages along the way
;;  :test, if t, will just print messages, not actually do anything
;;  :home-directory, if specified, supercedes system's home directory
;; 
;; (EDIT-SYSTEM name) ... calls the system  editor on the files in system
;;
;; (COPY-SYSTEM name :output-directory output-directory-pathname)
;;                    ... copies the _source_ files to output directory.
;;
;; Optional arguments:
;; (COPY-SYSTEM name :output-directory output-directory-pathname (test nil) (verbose t) (home-directory nil))
;;  :verbose, if t (default), will print messages along the way
;;  :test, if t, will just print messages, not actually do anything
;;  :home-directory, if specified, supercedes system's home directory
;;
;;(DEFINED-SYSTEMS) returns a list of defined systems.
;;
;;(REMOVE-SYSTEM name) removes system 
;;
;;(SYSTEM-NAMED name) finds system with this name.
;;
;; (UNSET-SYSTEM-STATE state name) "unsets" a particular system state
;; to an unfinished state. For example, (UNSET-SYSTEM-STATE 'COMPILE name)
;; informs the system that the system can be recompiled; similarly with
;; (UNSET-SYSTEM-STATE 'LOAD name)
;;
;; The type of the typical source file is found in (SOURCE-FILE-TYPE),
;; and be set with (SET-SOURCE-FILE-TYPE). It defaults to "lisp".
;;
;; There are a number of implementation dependencies in this file; if  you
;; get an error of the type
;; "Add Code here to implement..."
;; this indicates  you've hit an implementation dependency.
;; Sorry.

(in-package "CL-USER")

(CL:defpackage "A-SIMPLE-DEFSYSTEM" 
  (:use "COMMON-LISP")
  (:nicknames "ASD")
  (:export
   "*A-SIMPLE-DEFSYSTEM-VERSION*"
   
   "DEFINED-SYSTEMS"
   "DEFINE-SYSTEM"
   "REMOVE-SYSTEM"
   "SYSTEM-NAMED"
   "LOAD-SYSTEM"
   "COMPILE-SYSTEM"
   "EDIT-SYSTEM"
   "COPY-SYSTEM"
   "SET-BINARY-SUBDIRECTORY-NAME"
   "BINARY-SUBDIRECTORY-NAME"
   "SET-SOURCE-FILE-TYPE"
   "SOURCE-FILE-TYPE"

   "UNSET-SYSTEM-STATE"
   "THIS-FILES-DIRECTORY"
   "THIS-FILES-PATHNAME"
   ))

(in-package "A-SIMPLE-DEFSYSTEM")

(defparameter *version* 0.3)


(defvar *systems* nil "All known systems.")

(defvar *binary-file-type* 
  (pathname-type (compile-file-pathname "test")))

(defparameter *BINARY-SUBDIRECTORY-NAME*
  (car (list #+genera                  "genera"   ;;; symbolics lisp machines
	     #+(and lucid mips lcl4.0) "4.0mbin"  ;;; Decstations (lucid)
	     #+(and lucid
		    mips (not lcl4.0)) "mbin"     ;;; Decstations (lucid)
	     #+(and lucid pa)          "hbin"     ;;; HP's Precision Architecture (lucid)
             #+(and lucid hp300)       "6bin"     ;;; HP300's (lucid)
	     #+(and lucid sparc)       "sparcbin" ;;; Sun 4s
	     #+(and lucid sun mc68000) "sun3bin"  ;;; Sun 3s
	     #+(and lucid sun)         "sunbin"   ;;; Other sun
	     #+next                    "nextbin"  ;;; NeXTs (Allegro)
	     #+(and kcl vax)           "kclvax"   ;;; KCL (Kyoto Common Lisp) on vax
	     #+(and kcl mips)          "kclmips"  ;;; KCL on mips
	     #+(and kcl ibmrt)         "kclrt"    ;;; KCL in IBM RT
	     #+(and :coral :ccl-1.3)   "Macl1.3.2"  ;;; MACL (Macintosh Allegro CL)
             #+(and  :ccl-3 :powerpc)  "MCL-PPC3" ;;; Macintosh Common Lisp, v. 3, PowerPC           
             #+(and  :ccl-3)           "MCL3"     ;;; Macintosh Common Lisp, v. 3
             #+(and  :ccl-2)           "MCL2"     ;;; Macintosh Common Lisp, v. 2

	     "random-bin"))
  "The subdirectory in which to place binary files.")

(defvar *source-file-type* "lisp")

(defvar *not-finished-text* "Not finished")

(defstruct system 
  name            ; string  name of system
  root            ; root pathname
  submodules      ; submodules (files or systems)
  state           ; state of current system (compiled, loaded, etc)
  options         ; options 
  package         ; package to load files into
  )

(defmethod print-object ((self system) stream)
  (with-slots (name state) self
    (print-unreadable-object (self stream :type t :identity t)
       (format stream "~a :state ~a" name state)
      )))

;;----------------------------------------------------------------------
;; functions for setting/querying binary subdirectory name
;;----------------------------------------------------------------------
(defun BINARY-SUBDIRECTORY-NAME ()
  *BINARY-SUBDIRECTORY-NAME*)

(defun set-BINARY-SUBDIRECTORY-NAME (name)
  (check-type name string)
  (setf *BINARY-SUBDIRECTORY-NAME* name))

(defun source-file-type ()
  *source-file-type*)

(defun set-source-file-type (name)
  (check-type name string)
  (setf *source-file-type* name))

;;----------------------------------------------------------------------
;; functions for creating/querying/changing system states
;;----------------------------------------------------------------------


(defun make-system-state (state)
  (cons state *not-finished-text*))

(defun set-system-state (system-state state-value)
  (setf (rest system-state) state-value)
  )

(defun add-system-state (state system)
  (setf (system-state system)
        (cons state
              (system-state system))))

(defun system-state-value (state system)
  (rest (assoc state (system-state system))))

(defun set-system-state-value (state state-value system)
  (let ((system-state (assoc state (system-state system))))
    (if system-state
      (set-system-state system-state state-value)
      (let ((state (make-system-state state)))
        (set-system-state state state-value)
        (add-system-state state system)
        ))
    state-value))

(defun unset-system-state (state system)
  (when (not (system-p system)) (setf system (system-named system)))
  (set-system-state-value state *not-finished-text* system))

;;----------------------------------------------------------------------
;; functions on creating/querying systems
;;----------------------------------------------------------------------

(defun defined-systems () *systems*)

(defun add-system (new-system)
  "add a system structure to the list of systems"
  (let ((found-list (member (system-name new-system)
                            (defined-systems)
                            :test 'string-equal
                            :key 'system-name)))
    (if found-list
      (setf (first found-list) new-system)
      (setf *systems* (cons new-system *systems*)))))

(defun system-named (system-name)
  (find system-name (defined-systems) 
        :key 'system-name :test 'string-equal))

(defun remove-system (system-or-name)
  (if (stringp system-or-name)
    (setf *systems* (remove (system-named system-or-name) *systems*))
    (setf *systems* (remove system-or-name *systems*))))


(defun define-system (name &key 
                           directory 
                           home-directory
                           modules 
                           (use-bin-dir t)
                           (package *package*))
  (check-type name string)
  (add-system 
   (make-system
    :name name
    :root (make-system-root-pathname home-directory directory)
    :state ()
    :submodules modules
    :options (if use-bin-dir `(:use-bin-dir) nil)
    :package package))
  name)

;;----------------------------------------------------------------------
;; submodule specifications
;; (:SYSTEM name load-file)
;;----------------------------------------------------------------------

(defun system-spec? (l) (and (listp l) (eq (first l) :system)))
(defun system-name-of (system-spec) (second system-spec))
(defun system-pathname-specs-of (system-spec) (third system-spec))

;;----------------------------------------------------------------------
;; file specifications
;;----------------------------------------------------------------------

(defun file-spec? (l) (or (stringp l) 
                          (and (listp l) 
                               (eq (first l) :file))))

;;----------------------------------------------------------------------
;; Operating on systems.
;;----------------------------------------------------------------------

(defun operate-on-system (name operation force? result-flag test verbose home-directory keys)
  (let ((sys (system-named name)))
    (unless sys
      (error "Couldn't find a system named ~A" name))
    (if (and (equal (system-state-value operation sys) result-flag)  (null force?))
      (when verbose (format t "~%;; ~S is already ~A." name (system-state-value operation sys)))
      (let ((dir (if home-directory (make-system-root-pathname home-directory (system-root sys))
                                     (system-root sys)))
	    (modules (system-submodules sys))
            (package (system-package sys)))
        (when verbose (format t "~%;; Performing ~S on ~A." operation name ))
        (set-system-state-value operation *not-finished-text* sys)
        (with-compilation-unit ()
          (process-modules sys dir package modules operation
                           force? result-flag test verbose keys))
        (when verbose (format t "~%;; Finished ~a on ~a." operation name))
        (unless test (set-system-state-value operation result-flag sys))))))


(defun process-modules (sys dir package modules operation force? result-flag test verbose keys)
  (load-undefined-submodules-first dir modules verbose)
  (dolist (module modules)
    (cond
     ((file-spec? module)
      (operate-on-file operation 
                       (source-pathname module dir)
                       (binary-pathname module dir (system-options sys))
                       package
                       test
                       verbose
                       keys))
     ((system-spec? module)
      (operate-on-system (system-name-of module)
                         operation
                         force?
                         result-flag
                         test
                         verbose
                         dir
                         keys))
     ((listp module)
      (when (or (key-in? (car module) keys)
                (key-in? :ALL-GROUPS keys))
        (process-modules sys dir package (cdr module) operation force?
                         result-flag test verbose keys)))
     (T (error "In system ~A, ~s is not an allowed module type."
               (system-name-of sys) module)))))


(defun load-undefined-submodules-first (dir modules verbose)
  (dolist (module modules)
    (when
      (and (system-spec? module)
           (system-pathname-specs-of module)
           (not (system-named (system-name-of module))))
      (load (make-merged-pathname dir (system-pathname-specs-of module))
            :verbose verbose))))
           

(defun key-in? (key keys)
  (find key keys :test #'eql))

;;----------------------------------------------------------------------
;; operate on file methods
;;----------------------------------------------------------------------

(defmethod operate-on-file ((operation (eql 'LOAD))
                            source-pathname
                            binary-pathname
                            package
                            test
                            verbose
                            keys)
  (let (file-to-load)
    (if (or (null (probe-file binary-pathname))
            (key-in? :LOAD-SOURCE keys))
      (setf file-to-load source-pathname)
      (cond ((null (probe-file source-pathname))
             (cerror "Load the binary ~s"
                     "The source file ~*~s does not exist, but the binary does."
                     binary-pathname source-pathname)
             (setq file-to-load binary-pathname))
            (T
             (setf file-to-load (which-file-newer source-pathname
                                                  binary-pathname)))))
    (if test
      (format t "~%;;TEST: loading ~S" file-to-load)
      (let ((*package* (find-package package)))
        (load file-to-load :verbose verbose)))))

(defun load-system (name &key (force nil) (verbose t) (test nil) (load-source nil)
                           (home-directory nil) (group :ALL-GROUPS))
  (let ( (keys (append
                (if load-source '(:LOAD-SOURCE) '())
                (if (listp group) group (list group)))) )
    (operate-on-system name 'LOAD force "loaded" test verbose home-directory keys)
    (values)))
    
(defmethod operate-on-file ((operation (eql 'COMPILE))
                            source-pathname
                            binary-pathname
                            package
                            test
                            verbose
                            keys)
  (if (or (null (probe-file  binary-pathname))
          (key-in? :RECOMPILE keys)
          (and (null (probe-file source-pathname))
               (error "Source file ~s does not exist." source-pathname))
          (eq source-pathname (which-file-newer source-pathname binary-pathname)))
    (if test
      (format t "~%;;TEST: Compiling ~A as ~A" source-pathname binary-pathname)
      (progn
        (let ((*package* (find-package package)))
          (common-lisp:compile-file source-pathname :output-file binary-pathname :verbose verbose :force t))
        (when (key-in? :AND-LOAD keys)
          (let ((*package* (find-package package)))
            (common-lisp:load binary-pathname :verbose verbose)))))))

(defun compile-system (name &key (force nil) (verbose t) (test nil) 
                               (and-load? nil) (recompile nil)
                               (home-directory)
                               (group :ALL-GROUPS))
  (let ((keys (append 
               (if recompile '(:RECOMPILE) '())
               (if and-load? '(:AND-LOAD) '())
               (if (listp group) group (list group)))))
    (operate-on-system name 'COMPILE (or force recompile) "compiled" test verbose home-directory keys))
  (values))

(defmethod operate-on-file  ((operation (eql 'EDIT))
                            source-pathname
                            binary-pathname
                            package
                            test
                            verbose
                            keys)
  (declare (ignore binary-pathname package verbose keys))
  (if test
    (format t "~%;;TEST: Editing ~A" source-pathname)
    (ed source-pathname)))

(defun edit-system (name &key test home-directory (group :ALL-GROUPS))
  (let ((keys (if (listp group) group (list group))))
    (operate-on-system name 'EDIT nil "edited" test nil home-directory keys)))

(defun absolute->relative (dir-list)
  (cons :RELATIVE (rest dir-list)))

(defmethod operate-on-file ((operation (eql 'COPY))
                            source-pathname
                            binary-pathname
                            package
                            test
                            verbose
                            keys)
  (declare (ignore package binary-pathname))
  (let* ((to-dirpath (second (member :output-directory keys)))
         (to-pathname (merge-pathnames (make-pathname :name (pathname-name source-pathname)
                                                      :type (pathname-type source-pathname)
                                                      :directory (absolute->relative (pathname-directory source-pathname)))
                                       to-dirpath)))
    (asd-ensure-directories-exist to-dirpath) 
  (if (or (null (probe-file to-pathname))
          (eq source-pathname (which-file-newer source-pathname to-pathname)))
    (if test
      (format t "~%;;TEST: Copying ~A to ~A" source-pathname to-pathname)
      (asd-copy-file source-pathname to-pathname :if-exists :supersede :verbose verbose)))))

(defun copy-system (name &key output-directory test home-directory (verbose t))
  (unless output-directory (error "Output directory must be defined."))
  (unless (pathnamep output-directory) (error "Output directory must be a pathname."))
  (operate-on-system name 'COPY T "copied" test verbose home-directory `(:output-directory ,output-directory)))

(defmethod operate-on-file  (operation
                                source-pathname
                                binary-pathname
                                package
                                test
                                verbose
                                keys)
  (declare (ignore source-pathname binary-pathname package verbose keys test))
  (error "Don't know how to ~S." operation))
                            



;;----------------------------------------------------------------------
;; utility functions for working with pathnames, etc.
;;----------------------------------------------------------------------

(defun which-file-newer (file1 file2)
  (if (> (file-write-date file1) (file-write-date file2))
    file1 file2))

(defun make-system-root-pathname (home-directory directory)
  (cond
   ((null home-directory)
    (ensure-pathname-from-directory directory))
   ((null directory)
    (ensure-pathname-from-directory home-directory))
   (t (merge-pathnames
       (ensure-pathname-from-directory directory)
       (ensure-pathname-from-directory home-directory)))))

(defun ensure-pathname-from-directory (directory)
  (cond
   ((pathnamep directory) directory)
   ((listp directory) 
    (if (or (eq (car directory) :ABSOLUTE)
            (eq (car directory) :RELATIVE))
    (make-pathname :directory directory)
    (error "Invalid directory list: ~S 
(Should begin with :ABSOLUTE or :RELATIVE)." directory)))
   ((stringp directory)
    (let ((pn (parse-namestring directory)))
      (if (pathname-directory pn)
        pn
        (error "Could not parse a directory from ~S" directory))))))


(defun source-pathname (filename dir)
  ; filename is either a string or a list
  (let ((file-pathname 
         (if (stringp filename) 
           (parse-namestring filename)
           (apply 'make-pathname (cdr filename)))))
    (merge-pathnames 
     (make-pathname :name (pathname-name file-pathname)
                    :type  (or (pathname-type file-pathname) (source-file-type))
                    :directory (pathname-directory file-pathname)
                    )
     dir)))

(defun binary-pathname (filename dir options)
  (let ((file-pathname 
         (if (stringp filename) 
           (parse-namestring filename)
           (apply 'make-pathname (cdr filename)))))
    (merge-pathnames 
     (make-pathname :name (pathname-name file-pathname)
                    :directory (if (find :use-bin-dir options)
                                 `(:RELATIVE  ,*BINARY-SUBDIRECTORY-NAME*)
                                 (pathname-directory file-pathname))
                    :type *binary-file-type*)
     dir)))

(defun make-merged-pathname (dir specs)
  (if (pathnamep dir)
    (merge-pathnames (apply 'make-pathname
                            `(,@specs))
                     dir)
    (apply 'make-pathname `(,@specs :defaults ,(make-pathname :directory dir)))))

(defun directory-pathname (pathspec)
  (parse-namestring (directory-namestring pathspec)))
                 
#+MCL (defun mcl-ensure-directories-exist (pathspec &key verbose)
        (let* ((dir-path (directory-pathname pathspec))
              (created (if (probe-file dir-path) NIL
                           (ccl:create-file dir-path :if-exists NIL))))
          (when (and created verbose)
            (format *standard-output* ";; ~S created." created))
          (if created (values created T)
              (values dir-path NIL))))


(defun asd-ensure-directories-exist (pathspec &key verbose)
  (if (fboundp 'cl::ensure-directories-exist) ; this is the ANSI standard...
    (funcall 'cl::ensure-directories-exist pathspec :verbose verbose)
    #+MCL (mcl-ensure-directories-exist pathspec :verbose verbose)
    #-MCL (error "Add Code here to implement ensure-directories-exist.")))


(defun this-files-directory ()
  "returns a list of the current files directory"
  (let ((source-pn (this-files-pathname)))
    (if (pathnamep source-pn)
      (pathname-directory (translate-logical-pathname source-pn))
      NIL)))
      
      
(defun this-files-pathname ()
  "Returns the directory pathname of the file currently being loaded."
  (car 
   (list
    #+(and :allegro :unix) excl:*source-pathname*
    #+lucid  lucid::*source-pathname*
    #+MCL  (parse-namestring ccl::*LOADING-FILE-SOURCE-FILE*)
    #+(and :coral (not MCL)) (car ccl::*loading-files*)
    #+genera (concatenate 'string
			  (host-namestring sys:fdefine-file-pathname)
			  ":"
			  sys:fdefine-file-pathname
			  )
    #+next  *source-pathname*
    #-(or lucid :coral genera next mcl (and :allegro :unix))
    (error "Add Code here to implement this-files-pathname."))))


(defun asd-copy-file (old-pathname new-pathname &key if-exists verbose)
  (when verbose (format *standard-output* "~&;; Copying ~S to ~S" old-pathname new-pathname))
  #+MCL(ccl:copy-file old-pathname new-pathname :if-exists if-exists)
  #-MCL(error "Add Code here to implement asd-copy-file."))
         
