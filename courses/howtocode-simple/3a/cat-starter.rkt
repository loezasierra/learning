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
  (big-bang c              ; Cat
    (on-tick advance-cat)  ; Cat -> Cat
    (to-draw render)))     ; Cat -> Image

;; Cat -> Cat
;; produce the next cat position, changing x-axis position by +1
;(define (advance-cat c) 0) ;stub

(check-expect (advance-cat 0) 1)
(check-expect (advance-cat 6) 7)

; use template from CAT
(define (advance-cat c)
  (+ c 1))

;; Cat -> Image
;; produce cat image at appropriate place
(check-expect (render 5) (place-image CAT-IMG 5 CTR-Y MTS))

; (define (render c) MTS) ; stub

; use template from CAT
(define (render c)
  (place-image CAT-IMG c CTR-Y MTS))