(define +vehicle-package+ "org.heavymeta.vehicles")

(define (vehicles-class class)
  (string-append +vehicle-package+ "." class))


(define (load-vehicles-resource file)
  (load-resource file '#,(vehicles-class "Parser")))



(load-vehicles-resource "lib/common-lisp.scm")
(load-vehicles-resource "lib/utils.scm")
(load-vehicles-resource "lib/vehicle.scm")
(load-vehicles-resource "lib/parsing.scm")
(load-vehicles-resource "lib/random.scm")
(load-vehicles-resource "lib/ui.scm")

