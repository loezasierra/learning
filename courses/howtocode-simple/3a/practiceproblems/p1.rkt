#lang racket

(require 2htdp/image)
(require 2htdp/universe)

;; countdown-animation starter.rkt

; 
; PROBLEM:
; 
; Design an animation of a simple countdown. 
; 
; Your program should display a simple countdown, that starts at ten, and
; decreases by one each clock tick until it reaches zero, and stays there.
; 
; To make your countdown progress at a reasonable speed, you can use the 
; rate option to on-tick. If you say, for example, 
; (on-tick advance-countdown 1) then big-bang will wait 1 second between 
; calls to advance-countdown.
; 
; Remember to follow the HtDW recipe! Be sure to do a proper domain 
; analysis before starting to work on the code file.
; 
; Once you are finished the simple version of the program, you can improve
; it by reseting the countdown to ten when you press the spacebar.
; 


;; Program counts down at 1 per tick

;; ================================
;; Constants:
(define START 10)
(define STOP   0)

(define TICKS 1)

(define IMG-SIZE     50)
(define IMG-COLOR "pink")

(define RESET " ")

;; =================================
;; Data definitions:

;; Time is Integer[START, STOP]
;; interp. integer is current countdown number
(define T0       START) ; Countdown is at beginning
(define T1 (/ START 2)) ; Countdown is half-way
(define T2        STOP) ; Countdown has finished
#;
(define (fn-for-time t)
  (... t))

;; Template rules used:
;; - atomic non-distinct: integer[START, STOP]

;; =================================
;; Functions:

;; Time -> Time
;; start the world with (main 10)

(define (main t)
  (big-bang t                     ; Time
    (on-tick advance-time TICKS)  ; Time -> Time
    (to-draw render)              ; Time -> Image
    (on-key reset-time)))         ; Time KeyEvent -> Time            

;; Time -> Time
;; return current time - 1 where time is not STOP
(check-expect (advance-time START) (- START 1))
(check-expect (advance-time STOP) STOP)

; (define (advance-time t) 0) ;stub

; copy template from Time
(define (advance-time t)
  (if (= t STOP)
      t
      (- t 1)))

;; Time -> Image
;; render time as text image
(check-expect (render 16) (text "16" IMG-SIZE IMG-COLOR))
(check-expect (render  0) (text  "0" IMG-SIZE IMG-COLOR))

; (define (render t) (text "1" 5 "red")) ; stub

; copy template from Time
(define (render t)
  (text (number->string t) IMG-SIZE IMG-COLOR))

;; Time KeyEvent -> Time
;; reset countdown to START when spacebar is pressed
(check-expect (reset-time 16 " ") START)
(check-expect (reset-time STOP " ") START)
(check-expect (reset-time START " ") START)
(check-expect (reset-time 1 "a") 1)

; (define (reset-time t ke) 0) ; stub

; copy template from Time
(define (reset-time t ke)
  (cond [(key=? ke " ") START]
        [else t]))
