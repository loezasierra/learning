#lang racket

(require 2htdp/image)
(require 2htdp/universe)

;; cat-starter.rkt

; 
; PROBLEM:
; 
; Use the How to Design Worlds recipe to design an interactive
; program in which a cat starts at the left edge of the display 
; and then walks across the screen to the right. When the cat
; reaches the right edge it should just keep going right off 
; the screen.
; 
; Once your design is complete revise it to add a new feature,
; which is that pressing the space key should cause the cat to
; go back to the left edge of the screen. When you do this, go
; all the way back to your domain analysis and incorporate the
; new feature.
; 
; To help you get started, here is a picture of a cat, which we
; have taken from the 2nd edition of the How to Design Programs 
; book on which this course is based.
; 
; .
; 


;; Cat walks accross screen

;; ========================
;; Constants:
(define WIDTH 600)
(define HEIGHT 400)

(define CTR-Y (/ WIDTH 2))

(define SPEED 3)

(define MTS (empty-scene WIDTH HEIGHT))

(define CAT-IMG .)

;; =======================
;; Data definitions:

;; CAT is integer
;; interp. the x-coordinate position of the cat on screen
(define CAT0 0)           ; cat on left side of screen
(define CAT1 (/ WIDTH 2)) ; cat in middle of screen
(define CAT2 WIDTH)       ; cat on right side of screen

(define (fn-for-cat c)
  (... c))

;; Templates rules used:
;; - atomic non-distinct: integer

;; ========================
;; Functions:

;; Cat -> Cat
;; start the world with (main 0)
;;
(define (main c)
  (big-bang c                 ; Cat
    (on-tick  advance-cat)    ; Cat -> Cat
    (to-draw  render)         ; Cat -> Image
    (on-key   handle-key)     ; Cat KeyEvent -> Cat
    (on-mouse handle-click))) ; Cat Integer Integer MouseEvent -> Cat

;; Cat -> Cat
;; produce the next cat position, changing x-axis position by SPEED pixels to right
;(define (advance-cat c) 0) ;stub

(check-expect (advance-cat 0) (+ 0 SPEED))
(check-expect (advance-cat 6) (+ 6 SPEED))

; use template from CAT
(define (advance-cat c)
  (+ c SPEED))

;; Cat -> Image
;; produce cat image at appropriate place
(check-expect (render 5) (place-image CAT-IMG 5 CTR-Y MTS))

; (define (render c) MTS) ; stub

; use template from CAT
(define (render c)
  (place-image CAT-IMG c CTR-Y MTS))

;; Cat KeyEvent -> Cat
;; reset cat to start when space key is pressed
(check-expect (handle-key 10 " ")  0)
(check-expect (handle-key 10 "a") 10)
(check-expect (handle-key  0 " ")  0)
(check-expect (handle-key  0 "a")  0)

; (define (handle-key c ke) 0) ;stub

(define (handle-key c ke)
  (cond [(key=? ke " ") 0]
        [else c]))

;; Cat Integer Integer MouseVent -> Cat
;; produce cat image at left mouse click position
(check-expect (handle-click  0 16 64 "button-down") 16) ; check mouse click where Cat = 0
(check-expect (handle-click 50 16 64 "button-down") 16) ; check mouse click where Cat != 0
(check-expect (handle-click  0  0 32 "button-down")  0) ; check click at x-axis 0 position
(check-expect (handle-click 50 16 64 "button-up")   50) ; check other mouse event

; (define (handle-click c x y me) 0) ; stub

; use template from Cat

(define (handle-click c x y me)
  (cond [(mouse=? me "button-down") x]
        [else c]))
