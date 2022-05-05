#lang racket

(require 2htdp/image)
(require 2htdp/universe)

;; water-balloon-starter.rkt

; PROBLEM:
; 
; In this problem, we will design an animation of throwing a water balloon.  
; When the program starts the water balloon should appear on the left side 
; of the screen, half-way up.  Since the balloon was thrown, it should 
; fly across the screen, rotating in a clockwise fashion. Pressing the 
; space key should cause the program to start over with the water balloon
; back at the left side of the screen. 
; 
; NOTE: Please include your domain analysis at the top in a comment box. 
; 
; Use the following images to assist you with your domain analysis:
; 
; 
; 1)
; 2).
; .
; 3)
; .
; 4)
; 
; .
;     
; 
; Here is an image of the water balloon:
; (define WATER-BALLOON.)
; 
; 
; 
; NOTE: The rotate function wants an angle in degrees as its first 
; argument. By that it means Number[0, 360). As time goes by your balloon 
; may end up spinning more than once, for example, you may get to a point 
; where it has spun 362 degrees, which rotate won't accept. 
; 
; The solution to that is to use the modulo function as follows:
; 
; (rotate (modulo ... 360) (text "hello" 30 "black"))
; 
; where ... should be replaced by the number of degrees to rotate.
; 
; NOTE: It is possible to design this program with simple atomic data, 
; but we would like you to use compound data.


; Analysis
; .


;; Water balloon animation

;; =======================
;; Constants:

(define BALLOON-IMG .)

(define BYTE 8)

(define WIDTH  (* BYTE BYTE 10))
(define HEIGHT (/ WIDTH 3))

(define CTR-Y (/ HEIGHT 2))

(define MTS (empty-scene WIDTH HEIGHT))

(define SPEED 10)

(define ROTATEBY -10)

;; =======================
;; Data Definitions:

(define-struct balloon (position rotation))
;; Balloon is (make-balloon Integer[0, ...] Integer[0, 359])
;; interp. position as balloon's position on x-axis
;;         rotation as balloon's degrees rotation
(define B1 (make-balloon  8   0)) ; balloon is 8 pixels along x-axis and has no rotation
(define B2 (make-balloon 32 180)) ; balloon is 32 pixels along x-axis and has rotated 180 degrees

(define (fn-for-balloon b)
  (... (b-position)   ; Integer[0, ...]
       (b-rotation))) ; Integer[0, 359]

;; function templates used:
;; - compound: 2 fields

;; =======================
;; Functions:
;; Start main with (main (make-balloon 0 0))

(define (main b)
  (big-bang b
    (on-tick advance-balloon)   ; Balloon -> Balloon
    (to-draw render)            ; Balloon -> Image
    (on-key  restart-balloon))) ; Balloon KeyEvent -> Balloon

;; Ballooon -> Balloon
;; return Ballon where Balloon has moved by SPEED and rotated by ROTATEBY
(check-expect (advance-balloon (make-balloon  0   0))
              (make-balloon (+  0 SPEED) (modulo (+   0 ROTATEBY) 360))) ; check basic positiion & rotation
(check-expect (advance-balloon (make-balloon 20 359))
              (make-balloon (+ 20 SPEED) (modulo (+ 359 ROTATEBY) 360))) ; check rotation past 360

; (define (advance-balloon b) b) ; stub

; copy template from Balloon
(define (advance-balloon b)
  (make-balloon (+ SPEED (balloon-position b)) (rotate-balloon (balloon-rotation b))))

;; Integer[0, ...] -> Integer[0, 359]
;; return appropriate degrees rotation by ROTATEBY as to not go past 359
(check-expect (rotate-balloon  10) (+ 10 ROTATEBY))                 ; check basic rotation
(check-expect (rotate-balloon 359) (modulo (+ 359 ROTATEBY) 360))   ; check rotation past 360

; (define (rotate-balloon i) i) ;stub

; (define (rotate-balloon i)    ; template
;    (... i))

(define (rotate-balloon i)
  (modulo (+ i ROTATEBY) 360))

;; Balloon -> Image
;; place balloon image at approprate position with appropriate rotation
(check-expect (render (make-balloon   0   0)) (place-image (rotate   0 BALLOON-IMG)   0 CTR-Y MTS))
(check-expect (render (make-balloon 128 150)) (place-image (rotate 150 BALLOON-IMG) 128 CTR-Y MTS))

; (define (render b) BALLOON-IMG) ; stub

; copy template from Balloon

(define (render b)
  (place-image (rotate (balloon-rotation b) BALLOON-IMG) (balloon-position b) CTR-Y MTS))

;; Balloon KeyEvent -> Balloon
;; reset balloon to beginning when spacebar pressed
(check-expect (restart-balloon B1 " ") (make-balloon 0 0))
(check-expect (restart-balloon B1 "a") B1)
(check-expect (restart-balloon B2 " ") (make-balloon 0 0))

; (define (restart-balloon b ke) b) ; stub

#;
(define (restart-balloon b ke) ;template
  (cond [(key=? ke " ") (...b)]
        [else (... b)]))

(define (restart-balloon b ke)
  (cond [(key=? ke " ") (make-balloon 0 0)]
        [else b]))
