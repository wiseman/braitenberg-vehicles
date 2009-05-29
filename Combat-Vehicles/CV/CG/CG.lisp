;;; -------------------------------------------------------------------
;;; -------------------------------------------------------------------
;;; TG: Graphics the right way!
;;;
;;; Copyright (c) R. James Firby 1989,1991
;;; -------------------------------------------------------------------
;;; -------------------------------------------------------------------

(or (find-package "CG")
    (make-package "CG" 
                  :use '("COMMON-LISP" "CCL")
                  :nicknames '("COMMON-GRAPHICS")))

(in-package "CG")

(import '(CL-USER::cg-code-directory))

(when (not (boundp 'cg-code-directory))
  (defparameter cg-code-directory
    '(:absolute "Macintosh HD" "Chip" "New Simulator" "Common Graphics")))
 
(setq CG-Symbols
 '(->COLOR ->FONT
   BUTTON-1 BUTTON-1-UP BUTTON-NO-UP
   CIRCLE CLOSE-VIEW CLOSE-WINDOW COMPOSE-PICTURES COMPOSITE-PICTURE? 
    COPY-PICTURE COLOR? COLOR-BRIGHTEN
    COLOR/BLACK COLOR/BLUE COLOR/CYAN COLOR/GREEN COLOR/MAGENTA 
    COLOR/RED COLOR/WHITE COLOR/YELLOW COLOR/FOREGROUND COLOR/BACKGROUND
   DEPICTION DEPICTION-REGION DILATE-TO DILATE-TO! DILATE DILATE!
    DRAW-POINT DRAW-SEGMENT DRAW-RECTANGLE DRAW-OVAL DRAW-POLYGON DRAW-TEXT
   EMPTY-PICTURE! EMPTY-PICTURE? EVENT-HANDLER
   FONT? 
    FONT/STANDARD FONT/SYSTEM
   HIGHLIGHT HIGHLIGHT?
   INCLUDES-POINT? INSERT!
   MAKE-EMPTY-PICTURE MAKE-EVENT-TABLE
   NULL-EVENT-TABLE
   OPEN-VIEW OPEN-WINDOW OVAL OVAL?
   PICTURE-COMPONENTS PICTURE-COMPONENTS-INCLUDE-POINT?
    PICTURE-COMPONENTS-INCLUDING-POINT PICTURE-COMPONENTS-WITHIN-REGION
    PICTURE? POINT POINT? POLYGON POLYGON?
    POST-EVENT POINTER-ENTER POINTER-EXIT POINTER-POSITION POINTER-DRAG
   RECTANGLE RECTANGLE? REGULAR-POLYGON RELOCATE RELOCATE! REMOVE REMOVE!
   SEGMENT SEGMENT? SLATE
   TEXT TEXT? TEXTURE? THE-EMPTY-PICTURE TRANSLATE-TO TRANSLATE-TO!
    TRANSLATE TRANSLATE!
    TEXTURE/0% TEXTURE/100% TEXTURE/25% TEXTURE/50% TEXTURE/75%
   VIEW?
   WINDOW? WINDOW-OPEN? WINDOW-SIZE WINDOW-CLOSE WITH-DELAYED-UPDATE
    WITH-CHANGE-TO-HIGHLIGHT WITH-DRAWING-TO-SLATE))

(export CG-Symbols)

(when (find-package "ASD")
  (funcall (find-symbol "DEFINE-SYSTEM" (find-package "ASD"))
    "CG"
    :DIRECTORY (asd::this-files-directory)
    :PACKAGE "CG"
    :MODULES
    '("mac_interface"
      "fonts" "primitives" "windows" "pictures" "slates" "events"))
)

(when (not (find-package "ASD"))
  (load (make-pathname :directory cg-code-directory :name "mac_interface"))

  (load (make-pathname :directory cg-code-directory :name "fonts"))
  (load (make-pathname :directory cg-code-directory :name "primitives"))
  (load (make-pathname :directory cg-code-directory :name "windows"))
  (load (make-pathname :directory cg-code-directory :name "pictures"))
  (load (make-pathname :directory cg-code-directory :name "slates"))
  (load (make-pathname :directory cg-code-directory :name "events"))
)


