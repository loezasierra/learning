#lang racket

;; strictly-decreasing-starter.rkt

; 
; PROBLEM:
; 
; Design a function that consumes a list of numbers and produces true if the 
; numbers in lon are strictly decreasing. You may assume that the list has at 
; least two elements.
; 



;; (listof Integer) -> Bool
;; return true if numbers in lon are strictly decreasing
;; ASSUME: list has at least 2 elements
(check-expect (strict-dec (list 2 1)) true)
(check-expect (strict-dec (list 1 2)) false)
(check-expect (strict-dec (list 5 3 1)) true)
(check-expect (strict-dec (list 5 3 3 1 1)) false)
(check-expect (strict-dec (list 5 1 3)) false)

(define (strict-dec lon0)

  ;; (strict-dec (list 5 3 1) ; outer call
  
  ;; (strict-dec (list 5 3 1) ?) ; true
  ;; (strict-dec (list   3 1) 5) ; true
  ;; (strict-dec (list     1) 3) ; true

  ;; (strict-dec (list 5 1 3)) ; outer call
  
  ;; (strict-dec (list 5 1 3) ?) ; true
  ;; (strict-dec (list   1 3) 5) ; true
  ;; (strict-dec (list     3) 1) ; false
  
  (local [(define (strict-dec lon acc)
            (cond [(empty? lon) true]
                  [else
                   (and (< (first lon) acc)
                        (strict-dec (rest lon) (first lon)))]))]
    (strict-dec lon0 (add1 (first lon0)))))
