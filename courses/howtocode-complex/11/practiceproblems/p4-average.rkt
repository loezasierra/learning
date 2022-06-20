#lang racket

;; average-starter.rkt

; 
; PROBLEM:
; 
; Design a function called average that consumes (listof Number) and produces the
; average of the numbers in the list.
; 



;; (listof Number) -> Number
;; return the average of the numbers in the list
(check-expect (average (list 0)) 0)
(check-expect (average (list 2 4 6)) 4)
(check-expect (average (list 4 12 36 72)) 31)

(define (average lon0)
  ;; sum is Number; sum of the numbers seen in lon0 so far
  ;; cnt is Number; count of numbers seen in lon0 so far
  
  ;; (average (list 4 12 36 72)) ; outer call
  ;; (average (list 4 12 36 72)   0 0)
  ;; (average (list   12 36 72)   4 1)
  ;; (average (list      36 72)  16 2)
  ;; (average (list         72)  52 3)
  ;; (average (list           ) 124 4) ; return (/ 124 4) = 31
  (local [(define (average lon sum cnt)
            (cond [(empty? lon) (/ sum cnt)]
                  [else
                        (average (rest lon)
                                 (+ sum (first lon))
                                 (add1 cnt))]))]
    (average lon0 0 0)))
