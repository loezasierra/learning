#lang rocket

;; rocket-starter.rkt

;; =================
;; Data definitions:

; 
; PROBLEM A:
; 
; You are designing a program to track a rocket's journey as it descends 
; 100 kilometers to Earth. You are only interested in the descent from 
; 100 kilometers to touchdown. Once the rocket has landed it is done.
; 
; Design a data definition to represent the rocket's remaining descent. 
; Call it RocketDescent.
; 


;; RocketDescent is integer[0, 100]
;; interp. the rocket's remaining descent in kilometers
(define RD1 100) ; 100 km remaining
(define RD2 50)  ; 50  km remaining
(define RD3  0)  ; Rocket has landed
#;
(define (fn-for-rocketdescent rd)
  (... rd))

;; Template rules used:
;; - atomic non-distinct: integer[0, 100]

;; =================
;; Functions:

; 
; PROBLEM B:
; 
; Design a function that will output the rocket's remaining descent distance 
; in a short string that can be broadcast on Twitter. 
; When the descent is over, the message should be "The rocket has landed!".
; Call your function rocket-descent-to-msg.
; 


; RocketDescent -> string
; return a message with rocket's remaining descent, with a special msg when descent is over
; (define (rocket-descent-to-msg rd) "TOUCHDOWN") ;stub

(check-expect (rocket-descent-to-msg 100) "The rocket is 100 km away from touchdown.")
(check-expect (rocket-descent-to-msg 50)  "The rocket is 50 km away from touchdown.")
(check-expect (rocket-descent-to-msg  0)  "The rocket has landed!")

; copy template from RocketDescent
(define (rocket-descent-to-msg rd)
  (if (> rd 0)
      (string-append "The rocket is " (number->string rd) " km away from touchdown.")
      "The rocket has landed!"))