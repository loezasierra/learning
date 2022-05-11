#lang racket

;; largest-starter.rkt

;; =================
;; Data definitions:

; 
; Remember the data definition for a list of numbers we designed in Lecture 5f:
; (if this data definition does not look familiar, please review the lecture)
; 


;; ListOfNumber is one of:
;;  - empty
;;  - (cons Number ListOfNumber)
;; interp. a list of numbers
(define LON1 empty)
(define LON2 (cons 60 (cons 42 empty)))
#;
(define (fn-for-lon lon)
  (cond [(empty? lon) (...)]
        [else
         (... (first lon)
              (fn-for-lon (rest lon)))]))

;; Template rules used:
;;  - one of: 2 cases
;;  - atomic distinct: empty
;;  - compound: (cons Number ListOfNumber)
;;  - self-reference: (rest lon) is ListOfNumber

;; =================
;; Functions:

; 
; PROBLEM:
; 
; Design a function that consumes a list of numbers and produces the largest number 
; in the list. You may assume that all numbers in the list are greater than 0. If
; the list is empty, produce 0.
; 



;; ListOfNumber -> Number
;; returns largest number in list of numbers or 0 if empty
(check-expect (largest-num empty) 0) ; check empty list
(check-expect (largest-num (cons 8 empty)) 8) ; check single element list
(check-expect (largest-num (cons 64 (cons 128 (cons 8 empty)))) 128) ; check 3 element list

; (define (largest-num lon) 0) ;stub

; copy template from ListOfNumber
(define (largest-num lon)
  (cond [(empty? lon) 0]
        [else
         (larger-num (first lon)
                     (largest-num (rest lon)))]))

;; Number Number -> Number
;; returns the larger number of two numbers
(check-expect (larger-num  16 64) 64)
(check-expect (larger-num  64 16) 64)
(check-expect (larger-num 128 16) 128)

; (define (larger-num n1 n2) n1) ;stub

(define (larger-num n1 n2)
  (cond [(> n1 n2) n1]
        [else n2]))
