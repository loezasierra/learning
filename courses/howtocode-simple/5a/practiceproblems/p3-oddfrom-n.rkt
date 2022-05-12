#lang racket

;; odd-from-n-starter.rkt

;  PROBLEM:
;  
;  Design a function called odd-from-n that consumes a natural number n, and produces a list of all 
;  the odd numbers from n down to 1. 
;  
;  Note that there is a primitive function, odd?, that produces true if a natural number is odd.
;  



;; Integer -> ListOfInteger
;; return list of odd numbers from [n, 1]
(check-expect (odd-from-n 1) (cons 1 empty)) ; check base case
(check-expect (odd-from-n 5) (cons 5
                                   (cons 3
                                         (cons 1 empty)))) ; check input of 5

; (define (odd-from-n n) empty) ;stub

(define (odd-from-n n)
  (cond [(= 1 n) (cons 1 empty)]
        [else
         (if (odd? n) (cons n (odd-from-n (sub1 n)))
                      (odd-from-n (sub1 n)))]))
