;;;;;;;;init.lisp;;;;;;;;;;

(format T "~&;Loading init.Lisp")

;(set-view-position (find-window "Listener") #@(648 410))
;(set-view-size     (find-window "Listener") #@(500 400))

; -------------------------------------------------
; Define the logical location of interesting stuff

(setf (logical-pathname-translations "Common-Graphics")
      '(("*.*" "Vanna:Chip:Common Graphics:")))

; -------------------------
; Set environment variables

(setq *warn-if-redefine* nil)

; --------------------------------
; Fix indenting a little

(setf (cdr (assq 'if *fred-special-indent-alist*)) 0)  ; if
(push (cons 'receive  2) *fred-special-indent-alist*)   ; receive
(push (cons 'iterate  2) *fred-special-indent-alist*)   ; iterate
(push (cons 'xcase    1) *fred-special-indent-alist*)   ; xcase
(push (cons 'array-do 2) *fred-special-indent-alist*)   ; array-do

; ----------------------
; Make output look nice!

(format T "~&~%")

; ---------------------------
; Reset environment variables

(setq *warn-if-redefine* T)
