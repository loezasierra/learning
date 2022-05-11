#lang racket

;; boolean-list-starter.rkt

;; =================
;; Data definitions:

; 
; PROBLEM A:
; 
; Design a data definition to represent a list of booleans. Call it ListOfBoolean. 
; 



;; ListOfBoolean is one of:
;; - empty
;; - (cons boolean ListOfBoolean)
;; interp. a list of booleans
(define LOB1 empty) ; an empty list
(define LOB2 (cons true empty)) ; single boolean list
(define LOB3 (cons true (cons false empty))) ; 2 element boolean list

(define (fn-for-lob lob)
  (cond [(empty? lob) (...)]
        [else
         (... (first lob)
              (fn-for-lob (rest lob)))]))

;; Template rules used:
;; - one of: 2 cases
;; - atmoic distinct: empty
;; - compound: (cons boolean ListOfBoolean)
;; - self reference: (rest lob)


;; =================
;; Functions:

; 
; PROBLEM B:
; 
; Design a function that consumes a list of boolean values and produces true 
; if every value in the list is true. If the list is empty, your function 
; should also produce true. Call it all-true?
; 



;; ListOfBoolean -> bool
;; return true if every value in list is true or list is empty
(check-expect (all-true empty) true) ; check base case
(check-expect (all-true (cons true empty)) true) ; check single element true
(check-expect (all-true (cons false empty)) false) ; check single element false
(check-expect (all-true (cons true (cons true (cons true empty)))) true) ; check 3 element true
(check-expect (all-true (cons true (cons false (cons true empty)))) false) ; check 3 element false

; (define (all-true lob) false) ;stub

; copy template from ListOfBoolean
(define (all-true lob)
  (cond [(empty? lob) true]
        [else
         (equal? (first lob)
                 (all-true (rest lob)))]))

