#lang racket

;; sum-n-tr-starter.rkt

; 
; PROBLEM:
; 
; Consider the following function that consumes Natural number n and produces the sum 
; of all the naturals in [0, n].
;     
; Use an accumulator to design a tail-recursive version of sum-n.
; 


;; Natural -> Natural
;; produce sum of Natural[0, n]

(check-expect (sum-n 0) 0)
(check-expect (sum-n 1) 1)
(check-expect (sum-n 3) (+ 3 2 1 0))

;(define (sum-n n) 0) ;0

(define (sum-n n0)
  ;; acc is Number; sum of numbers seen so far

  ;; (sum-n 3) ; outer call
  ;; ((sum-n 3) 0) ; 3 + 0
  ;; ((sum-n 2) 3) ; 2 + 3
  ;; ((sum-n 1) 5) ; 1 + 5
  ;; ((sum-n 0) 6) ; return 6
  (local [(define (sum-n n acc)
            (cond [(zero? n) acc]
                  [else
                   (sum-n (sub1 n)
                          (+ acc n))]))]
    (sum-n n0 0)))
