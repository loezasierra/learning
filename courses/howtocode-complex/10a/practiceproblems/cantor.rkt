#lang racket

(require 2htdp/image)
(require 2htdp/universe)

;; cantor-starter.rkt

; 
; PROBLEM:
; 
; A Cantor Set is another fractal with a nice simple geometry.
; The idea of a Cantor set is to have a bar (or rectangle) of
; a certain width w, then below that are two recursive calls each
; of 1/3 the width, separated by a whitespace of 1/3 the width.
; 
; So this means that the
;   width of the whitespace   wc  is  (/ w 3)
;   width of recursive calls  wr  is  (/ (- w wc) 2)
;   
; To make it look better a little extra whitespace is put between
; the bars.
; 
; 
; Here are a couple of examples (assuming a reasonable CUTOFF)
; 
; (cantor CUTOFF) produces:
; 
; .
; 
; (cantor (* CUTOFF 3)) produces:
; 
; .
; 
; And that keeps building up to something like the following. So
; as it goes it gets wider and taller of course.
; 
; .
; 
; 
; PROBLEM A:
; 
; Design a function that consumes a width and produces a cantor set of 
; the given width.
; 
; 
; PROBLEM B:
; 
; Add a second parameter to your function that controls the percentage 
; of the recursive call that is white each time. Calling your new function
; with a second argument of 1/3 would produce the same images as the old 
; function.
; 
; PROBLEM C:
; 
; Now you can make a fun world program that works this way:
;   The world state should simply be the most recent x coordinate of the mouse.
;   
;   The to-draw handler should just call your new cantor function with the
;   width of your MTS as its first argument and the last x coordinate of
;   the mouse divided by that width as its second argument.
;   
; 
; 



;; Constants
(define CUTOFF 5)
(define STEP-HEIGHT 20)
(define SPACING (rectangle 0 (/ STEP-HEIGHT 2) "solid" "white"))
(define CAN-CLR "blue")

;; Integer -> Image
;; produce a Cantor Set of given width
(check-expect (cantor CUTOFF) (rectangle CUTOFF STEP-HEIGHT "solid" CAN-CLR))
(check-expect (cantor (* CUTOFF 3))
              (above (rectangle (* CUTOFF 3) STEP-HEIGHT "solid" CAN-CLR)
                     SPACING
                     (local [(define wc (/ (* CUTOFF 3) 3))
                             (define wr (rectangle (/ (- (* CUTOFF 3) wc) 2) STEP-HEIGHT "solid" CAN-CLR))]
                       (beside wr (rectangle wc STEP-HEIGHT "solid" "white") wr))))

(define (cantor w)
  (cond [(<= w CUTOFF) (rectangle w STEP-HEIGHT "solid" CAN-CLR)]
        [else
         (above (rectangle w STEP-HEIGHT "solid" CAN-CLR)
                SPACING
                (local [(define wc (/ w 3))
                        (define wr (/ (- w wc) 2))
                        (define blk (rectangle wc STEP-HEIGHT "solid" "white"))
                        (define leg (cantor wr))]
                  (beside leg blk leg)))]))



;; Integer Number -> Image
;; produce a Cantor Set of given width with given whitespace
(check-expect (cantor-space CUTOFF 1/3) (rectangle CUTOFF STEP-HEIGHT "solid" CAN-CLR))
(check-expect (cantor-space (* CUTOFF 3) 1/3)
              (above (rectangle (* CUTOFF 3) STEP-HEIGHT "solid" CAN-CLR)
                     SPACING
                     (local [(define wc (/ (* CUTOFF 3) 3))
                             (define wr (rectangle (/ (- (* CUTOFF 3) wc) 2) STEP-HEIGHT "solid" CAN-CLR))]
                       (beside wr (rectangle wc STEP-HEIGHT "solid" "white") wr))))

(define (cantor-space w s)
  (cond [(<= w CUTOFF) (rectangle w STEP-HEIGHT "solid" CAN-CLR)]
        [else
         (above (rectangle w STEP-HEIGHT "solid" CAN-CLR)
                SPACING
                (local [(define wc (* w s))
                        (define wr (/ (- w wc) 2))
                        (define blk (rectangle wc STEP-HEIGHT "solid" "white"))
                        (define leg (cantor-space wr s))]
                  (beside leg blk leg)))]))


(define MTS-WIDTH 1500)
(define MTS-HEIGHT 800)
(define MTS (empty-scene MTS-WIDTH MTS-HEIGHT))

(define CTR-Y (/ MTS-HEIGHT 2))
(define CTR-X (/ MTS-WIDTH 2))

;; start the world with (main 0)

(define (main m)
  (big-bang m
    (to-draw  render-cantor)
    (on-mouse next-pos)))

(define (render-cantor m)
  (place-image (cantor-space MTS-WIDTH (/ m MTS-WIDTH)) CTR-X CTR-Y MTS))

(define (next-pos m x y me)
  (cond [(< x 0) 0]
        [(> x MTS-WIDTH) MTS-WIDTH]
        [else x]))

