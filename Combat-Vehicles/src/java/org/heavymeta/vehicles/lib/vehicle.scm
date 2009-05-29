;;; General Utilities
;
;;; Simple World
;
;(define *world* (new 'World))
;
;(define *v1* (make-vehicle *world* 2 1 (new 'java.awt.Color 255 0 0)))
;(poke *v1* 'max_speed 20)
;
;(define *v2* (make-vehicle *world* -2 -2 (new 'java.awt.Color 0 255 0)))
;(poke *v2* 'max_speed 15)
;
;(define *v3* (make-vehicle *world* 4 4 (new 'java.awt.Color 0 0 255)))
;(poke *v3* 'max_speed 25)
;
;(define *l1* (make-lamp *world* 0 1 "Light"))
;(define *l2* (make-lamp *world* -1 -1 "Light"))
;(define *l3* (make-lamp *world* 1 -1 "Light"))
;
;
;(invoke (peek *world* 'radiation_hierarchy)
;        'addChild
;        "Light" "Light")


(define (step-n n)
  (if (= n 0)
    #t
    (begin (invoke *world* 'step)
           (render-it)
           (step-n (- n 1)))))


(define (world-lamps world)
  (enumeration->list (invoke (peek world 'lamps) 'elements)))

(define (world-vehicles world)
  (enumeration->list (invoke (peek world 'vehicles) 'elements)))



(define (render-it)
  (render-world *world* (invoke window* 'getGraphics)))


;;(define window* (make-refreshed-window "JVehicles" 300 300
;;    (lambda (graphics)
;;      (render-world *world* graphics))))

;;(define renderer* (new 'org.heavymeta.vehicles.VRenderer window*))
;;(invoke renderer* 'setScale 20.0)
;;(invoke renderer* 'setOrigin 0.0 0.0)



;;(define (render-world world graphics)
;;  (invoke renderer* 'render graphics world))
  
  
  



;(poke renderer* 'toolkit (invoke window* 'getToolkit))     
;(poke renderer* 'sync #f)




;(define (render-lamp lamp graphics)
;  (invoke-static 'VGraphics 'setColor graphics yellow)
;  (let* ((l (invoke lamp 'location))
;         (x (peek l 'x))
;         (y (peek l 'y)))
;  (invoke-static 'VGraphics 'fillOval graphics
;    (integer (xform x))
;    (integer (xform y))
;    (integer 10) (integer 10))))


;(define (render-vehicle v graphics)
;  (invoke-static 'VGraphics 'setColor graphics red)
;  (let* ((l (invoke v 'location))
;         (x (xform (peek l 'x)))
;         (y (xform (peek l 'y)))
;         (theta (peek (invoke v 'orientation) 'theta)))
;    (let ((xx (xform x))
;          (xy (xform y)))
;      (invoke-static 'VGraphics 'drawLine graphics
;              (integer x) (integer y)
;              (integer (+ x (* (cos theta) 10)))
;              (integer (+ y (* (sin theta) 10))))
;            )))




;;(define byteout (new 'java.io.ByteArrayOutputStream))
;;(define objout (new 'java.io.ObjectOutputStream byteout))
;;(invoke objout 'writeObject *world*)
;;(define array (invoke byteout 'toByteArray))
