#lang racket

;; product-tr-starter.rkt

; 
; PROBLEM:
; 
; (A) Consider the following function that consumes a list of numbers and produces
;     the product of all the numbers in the list. Use the stepper to analyze the behavior 
;     of this function as the list gets larger and larger. 
;     
; (B) Use an accumulator to design a tail-recursive version of product.
; 


;; (listof Number) -> Number
;; produce product of all elements of lon
(check-expect (product empty) 1)
(check-expect (product (list 2 3 4)) 24)

(define (product lon0)
  ;; acc is Number; product of lon0 so far

  ;; (product (list 2 3 4))
  ;; (product (list 2 3 4)  1)
  ;; (product (list   3 4)  2)
  ;; (product (list     4)  6)
  ;; (product (list      ) 24)
  
  (local [(define (product lon acc)
            (cond [(empty? lon) acc]
                  [else
                   (product (rest lon)
                            (* acc (first lon)))]))]
    (product lon0 1)))
