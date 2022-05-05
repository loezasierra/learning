#lang racket

(require 2htdp/image)
(require 2htdp/universe)

; 
; PROBLEM:
; 
; As we learned in the cat world programs, cats have a mind of their own. When they 
; reach the edge they just keep walking out of the window.
; 
; Cows on the other hand are docile creatures. They stay inside the fence, walking
; back and forth nicely.
; 
; Design a world program with the following behaviour:
;    - A cow walks back and forth across the screen.
;    - When it gets to an edge it changes direction and goes back the other way
;    - When you start the program it should be possible to control how fast a
;      walker your cow is.
;    - Pressing space makes it change direction right away.
;    
; To help you here are two pictures of the right and left sides of a lovely cow that 
; was raised for us at Brown University.
; 
; .     .
; 
; Once your program works here is something you can try for fun. If you rotate the
; images of the cow slightly, and you vary the image you use as the cow moves, you
; can make it appear as if the cow is waddling as it walks across the screen.
; 
; Also, to make it look better, arrange for the cow to change direction when its
; nose hits the edge of the window, not the center of its body.
; 


;; Produces a cow that walks back and forth accross the screen

;; ===============================
;; Constants:

(define RCOW .)
(define LCOW .)

(define BYTE 8)

(define HEIGHT (* BYTE BYTE 3))
(define WIDTH  (* HEIGHT 2))

(define CTR-Y (/ HEIGHT 2))

(define MTS (empty-scene WIDTH HEIGHT))

;; ================================
;; Data definitions:

(define-struct cow (position velocity))
;; Cow is (make-cow Integer[0, Width] Integer)
;; interp. position as cow's position on x-axis (in pixels)
;;         velocity as cow's speed in direction (pixels per tick)
(define C1 (make-cow 10  1)) ; Cow at position 10, moving -> at 1 pixel per tick
(define C2 (make-cow 16 -2)) ; Cow at position 16, moving <- at 2 pixels per tick
#;
(define (fn-for-cow c)
  (... (cow-position c)   ; integer[0, WIDTH]
       (cow-velocity c))) ; integer

;; Template rules used:
;; - compound: 2 fields

;; ================================
;; Functions:

;; Cow -> Cow
;; start the world with... TODO

(define (main c)
  (big-bang c
    (on-tick advance-cow)         ; Cow -> Cow
    (to-draw render)              ; Cow -> Image
    (on-key  change-direction)))  ; Cow KeyEvent -> Cow

;; Cow -> Cow
;; Change cow's position by cow's velocity, bounce off edges
(check-expect (advance-cow (make-cow 16  2)) (make-cow (+ 16 2)  2))     ; middle
(check-expect (advance-cow (make-cow 16 -2)) (make-cow (- 16 2) -2))

(check-expect (advance-cow (make-cow (- WIDTH 3) 3)) (make-cow WIDTH 3)) ; reaches edge
(check-expect (advance-cow (make-cow 3          -3)) (make-cow 0    -3))

(check-expect (advance-cow (make-cow (- WIDTH 1) 3)) (make-cow WIDTH -3)) ; tries to pass edge
(check-expect (advance-cow (make-cow 1          -3)) (make-cow 0      3))

; (define (advance-cow c) C1) ; stub

; copy template from Cow
(define (advance-cow c)
  (cond [(> (+ (cow-position c) (cow-velocity c)) WIDTH) (make-cow WIDTH (- (cow-velocity c)))]
        [(< (+ (cow-position c) (cow-velocity c)) 0)     (make-cow 0     (- (cow-velocity c)))]
        [else (make-cow (+ (cow-position c) (cow-velocity c)) (cow-velocity c))]))

;; Cow -> Image
;; Render appropriate cow image at cow's position based on cow's direction
(check-expect (render (make-cow 16  2)) (place-image RCOW 16 CTR-Y MTS))
(check-expect (render (make-cow 64 -2)) (place-image LCOW 64 CTR-Y MTS))

;(define (render c) (circle 5 "red")) ; stub

; copy template from Cow
(define (render c)
  (if (> (cow-velocity c) 0)
      (place-image RCOW (cow-position c) CTR-Y MTS)
      (place-image LCOW (cow-position c) CTR-Y MTS)))

;; Cow KeyEvent -> Cow
;; reverse cow's direction when spacebar pressed
(check-expect (change-direction (make-cow 16  2) " ") (make-cow 16 -2)) ; check -> to <-
(check-expect (change-direction (make-cow 16 -2) " ") (make-cow 16  2)) ; check <- to ->
(check-expect (change-direction (make-cow 16  2) "a") (make-cow 16  2)) ; check other input

;(define (change-direction c ke) C1) ; stub

(define (change-direction c ke)
  (if (key=? ke " ")
      (make-cow (cow-position c) (-(cow-velocity c)))
      c))
