#lang racket

;; HtDD Design Quiz

;; Age is Natural
;; interp. the age of a person in years
(define A0 18)
(define A1 25)

#;
(define (fn-for-age a)
  (... a))

;; Template rules used:
;; - atomic non-distinct: Natural


; Problem 1:
; 
; Consider the above data definition for the age of a person.
; 
; Design a function called teenager? that determines whether a person
; of a particular age is a teenager (i.e., between the ages of 13 and 19).

; Age -> bool
; returns true if age is between 13 and 19 inclusive
; (define (teenager? a) false) ;stub

(check-expect (teenager? 12) false)
(check-expect (teenager? 15) true)
(check-expect (teenager? 20) false)

; copy template from Age
(define (teenager? a)
  (and (>= a 13)
       (<= a 19)))

; Problem 2:
; 
; Design a data definition called MonthAge to represent a person's age
; in months.

;; MonthAge is integer
;; interp. the age of a person in months
(define A2  6)
(define A3 48)
#;
(define (fn-for-month-age a)
  (... a))

; Template rules used:
; - atomic non-distinct: integer

; Problem 3:
; 
; Design a function called months-old that takes a person's age in years 
; and yields that person's age in months.
; 

;; Age -> MonthAge
;; returns person's age in months
; (define (months-old a) 0) ;stub

(check-expect (months-old 1) 12)
(check-expect (months-old 8) (* 12 8))

; copy function from MonthAge
(define (months-old a)
  (* a 12))

; Problem 4:
; 
; Consider a video game where you need to represent the health of your
; character. The only thing that matters about their health is:
; 
;   - if they are dead (which is shockingly poor health)
;   - if they are alive then they can have 0 or more extra lives
; 
; Design a data definition called Health to represent the health of your
; character.
; 
; Design a function called increase-health that allows you to increase the
; lives of a character.  The function should only increase the lives
; of the character if the character is not dead, otherwise the character
; remains dead.


;; Health is one of:
;; - integer
;; - "dead"
;; interp. number of extra lives player has where not "dead"

(define P0 5) ; player has 5 extra lives
(define P1 0) ; player has 0 extra lives
(define P2 "dead") ; player is dead
#;
(define (fn-for-health h)
  (cond [(number?  h) (... h)]
        [(string=? h "dead") (...)]))

;; Template rules used:
;; - one of Health
;; - atomic non-distinct: integer
;; - atomic distinct value: "dead"

;; Health -> Health
;; increase lives of a character by 1 if character is not dead
; (define (increase-health h) 1) ;stub

(check-expect (increase-health 0) 1)
(check-expect (increase-health 5) 6)
(check-expect (increase-health "dead") "dead")

; copy template from Health
(define (increase-health h)
  (cond [(number? h) (+ h 1)]
        [(string=? h "dead") "dead"]))
