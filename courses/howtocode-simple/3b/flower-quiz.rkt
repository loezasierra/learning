#lang racket

(require 2htdp/image)
(require 2htdp/universe)

;; A flower that grows
;; =====================
;; Constants

(define FLOWER-IMG .)

(define BYTE 8)

(define WIDTH  (* BYTE BYTE 20))
(define HEIGHT (/ WIDTH 1.5))

(define MTS (empty-scene WIDTH HEIGHT))

(define SCALEBY .01)

(define SCALESTART .1)

;; =========================
;; Data definitions:
(define-struct flower (x y scale))
;; Flower is (make-flower Integer[0, WIDTH] Integer[0, HEIGHT] Float[0, ...])
;; interp. x and y as the pixel coordinates of Flower on screen
;;         scale as the scale of Flower image
(define F1 (make-flower  0  0  1)) ;; Flower is at top left of screen with scale of 1
(define F2 (make-flower 64 64 .5)) ;; Flower is at 64x64 pixel coordinates and is half of its size

(define (fn-for-flower f)
  (... (flower-x f)       ; Integer[0, ...]
       (flower-y f)       ; Integer[0, ...]
       (flower-scale f))) ; Float[0, ...]

;; Template rules used:
;; - compound: 3 fields

;; =========================
;; Functions:

;; Start main with (main (make-flower 0 0 0))
(define (main f)
  (big-bang f
    (on-tick  scale-flower) ; Flower -> Flower
    (to-draw  render)       ; Flower -> Image
    (on-mouse new-flower))) ; Flower Int Int MouseEvent -> Flower

;; Flower -> Flower
;; return a Flower with SCALEBY added to its scale where Flower scale is greater than 0
(check-expect (scale-flower (make-flower 0 0 1))
              (make-flower 0 0 (+ 1 SCALEBY))) ; check correct scale
(check-expect (scale-flower (make-flower 64 64 3.2))
              (make-flower 64 64 (+ 3.2 SCALEBY))) ; check correct scale
(check-expect (scale-flower (make-flower 128 128 0))
              (make-flower 128 128 0)) ; check 0 scale Flower
(check-expect (scale-flower (make-flower 64 64 -2.3))
              (make-flower 64 64 -2.3)) ; check negative scale
#; 
(define (scale-flower f) f) ; stub

; copy template from Flower
(define (scale-flower f)
  (if (> (flower-scale f) 0)
      (make-flower (flower-x f) (flower-y f) (+ (flower-scale f) SCALEBY))
      f))

;; Flower -> Image
;; render appropriately scaled FLOWER-IMG at appropriate coordinates where Flower's scale is greater than 0
(check-expect (render (make-flower 0 0 1))
              (place-image (scale 1 FLOWER-IMG) 0 0 MTS)) ; check correct rendering
(check-expect (render (make-flower 64 64 3.2))
              (place-image (scale 3.2 FLOWER-IMG) 64 64 MTS)) ; check correct rendering
(check-expect (render (make-flower 128 128 0))
              (place-image (circle 0 "solid" "white") 0 0 MTS)) ; check blank rendering where Flower scale less than 0

; (define (render f) (place-image (scale 1 FLOWER-IMG) 0 0 MTS)) ; stub

;; copy template from Flower
(define (render f)
  (if (> (flower-scale f) 0)
      (place-image (scale (flower-scale f) FLOWER-IMG) (flower-x f) (flower-y f) MTS)
      (place-image (circle 0 "solid" "white") 0 0 MTS)))

;; Flower Int Int MouseEvent -> Flower
;; return a new Flower of SCALESTART scale at clicked position
(check-expect (new-flower (make-flower 0 0 1) 64 64 "button-down")
              (make-flower 64 64 SCALESTART)) ; check correct input
(check-expect (new-flower (make-flower 64 64 1.6) 8 8 "button-down")
              (make-flower 8 8 SCALESTART)) ; check coorect input
(check-expect (new-flower (make-flower 0 0 1) 64 64 "button-up")
              (make-flower 0 0 1)) ; check incorrect input

;; (define (new-flower f x y me) f) ; stub

;; mouse handler template
#;
(define (new-flower f x y me)
  (cond [(mouse=? me "button-down") (... f x y)]
        [else
         (... f x y)]))

(define (new-flower f x y me)
  (cond [(mouse=? me "button-down")
           (make-flower x y SCALESTART)]
        [else f]))
