#lang racket

;; abstract-some-starter.rkt

; 
; PROBLEM:
; 
; Design an abstract function called some-pred? (including signature, purpose, 
; and tests) to simplify the following two functions. When you are done
; rewrite the original functions to use your new some-pred? function.
; 


;; ListOfNumber -> Boolean
;; produce true if some number in lon is positive
(check-expect (some-positive? empty) false)
(check-expect (some-positive? (list 2 -3 -4)) true)
(check-expect (some-positive? (list -2 -3 -4)) false)

(define (some-positive? lon) (mapor positive? lon))


;; ListOfNumber -> Boolean
;; produce true if some number in lon is negative
(check-expect (some-negative? empty) false)
(check-expect (some-negative? (list 2 3 -4)) true)
(check-expect (some-negative? (list 2 3 4)) false)

(define (some-negative? lon) (mapor negative? lon))


;; (X -> Bool) (listof X) -> Bool
;; return true if some X in (listof X) return true
(check-expect (mapor positive? empty) false)
(check-expect (mapor positive? (list 2 -3 -4)) true)
(check-expect (mapor negative? (list 2 3 4)) false)
(check-expect (local [(define (match-meme i) (or (= i 69) (= i 420)))]
                (mapor match-meme (list 50 69 20))) true)

(define (mapor fn lon)
  (cond [(empty? lon) false]
        [else
         (or (fn (first lon))
             (mapor fn (rest lon)))]))
