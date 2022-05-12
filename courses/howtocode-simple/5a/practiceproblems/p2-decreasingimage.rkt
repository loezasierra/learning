#lang racket

(require 2htdp/image)

;; decreasing-image-starter.rkt

;  PROBLEM:
;  
;  Design a function called decreasing-image that consumes a Natural n and produces an image of all the numbers 
;  from n to 0 side by side. 
;  
;  So (decreasing-image 3) should produce .


;; Constants
(define FONT-SIZE 24)
(define FONT-COLOR "black")

;; Integer -> Image
;; produces image of all numbers from n to 0
(check-expect (decreasing-image 0) (text (number->string 0) FONT-SIZE FONT-COLOR)) ; check base case
(check-expect (decreasing-image 3) (beside (text (number->string 3) FONT-SIZE FONT-COLOR)
                                           (text (number->string 2) FONT-SIZE FONT-COLOR)
                                           (text (number->string 1) FONT-SIZE FONT-COLOR)
                                           (text (number->string 0) FONT-SIZE FONT-COLOR))) ; check 4 numbers

; (define (decreasing-image n) empty-image)

(define (decreasing-image n)
  (cond [(zero? n) (text "0" FONT-SIZE FONT-COLOR)]
        [else
         (beside (text (number->string n) FONT-SIZE FONT-COLOR)
                 (decreasing-image (sub1 n)))]))
