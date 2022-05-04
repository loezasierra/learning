#lang racket

(require 2htdp/image)
(require 2htdp/universe)

;; traffic-light-starter.rkt

; 
; PROBLEM:
; 
; Design an animation of a traffic light. 
; 
; Your program should show a traffic light that is red, then green, 
; then yellow, then red etc. For this program, your changing world 
; state data definition should be an enumeration.
; 
; Here is what your program might look like if the initial world 
; state was the red traffic light:
; .
; Next:
; .
; Next:
; .
; Next is red, and so on.
; 
; To make your lights change at a reasonable speed, you can use the 
; rate option to on-tick. If you say, for example, (on-tick next-color 1) 
; then big-bang will wait 1 second between calls to next-color.
; 
; Remember to follow the HtDW recipe! Be sure to do a proper domain 
; analysis before starting to work on the code file.
; 
; Note: If you want to design a slightly simpler version of the program,
; you can modify it to display a single circle that changes color, rather
; than three stacked circles. 
; 


(require 2htdp/image)
(require 2htdp/universe)

;; Traffic light animation

;; =======================
;; Constants:

(define GREEN-IMG  .)

(define YELLOW-IMG .)

(define RED-IMG    .)

(define HEIGHT (* 8 8 3))
(define WIDTH  (/ HEIGHT 2))

(define CTR-X (/ WIDTH  2))
(define CTR-Y (/ HEIGHT 2))

(define MTS (empty-scene WIDTH HEIGHT "Sky Blue"))

;; =====================
;; Data definitions:

;; Light is one of:
;; - "green"
;; - "yellow"
;; - "red"
;; interp. the current traffic light color
;; example redundant for enumeration
#;
(define (fn-for-light l)
  (cond [(string=? l "green")  (...)]
        [(string=? l "yellow") (...)]
        [(string=? l "red")    (...)]))

;; Template rules used:
;; - enumeration: Light
;; - atomic-distinct: "green", "yellow", "red"

;; ======================
;; Functions:

;; Light -> Light
;; start the world with (main "green")

(define (main l)
  (big-bang l            ; Light
    (on-tick next-light 1) ; Light -> Light
    (to-draw render)))   ; Light -> Image

;; Light -> Light
;; return the next light color
; (define (next-light l) "red") ; stub
(check-expect (next-light "green")  "yellow")
(check-expect (next-light "yellow") "red")
(check-expect (next-light "red")    "green")

; copy template from Light
(define (next-light l)
  (cond [(string=? l "green")  "yellow"]
        [(string=? l "yellow") "red"]
        [(string=? l "red")    "green"]))

;; Light -> Image
;; return corresponding image given light color
; (define (render l) RED-IMG) ;stub
(check-expect (render "green")  (place-image GREEN-IMG CTR-X CTR-Y MTS))
(check-expect (render "yellow") (place-image YELLOW-IMG CTR-X CTR-Y MTS))
(check-expect (render "red")    (place-image RED-IMG CTR-X CTR-Y MTS))

; copy template from Light
(define (render l)
  (cond [(string=? l "green")  (place-image GREEN-IMG CTR-X CTR-Y MTS)]
        [(string=? l "yellow") (place-image YELLOW-IMG CTR-X CTR-Y MTS)]
        [(string=? l "red")    (place-image RED-IMG CTR-X CTR-Y MTS)]))
