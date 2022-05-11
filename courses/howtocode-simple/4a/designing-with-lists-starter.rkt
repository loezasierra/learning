#lang racket

;; designing-with-lists-1-starter.rkt

; 
; PROBLEM:
; 
; You've been asked to design a program having to do with all the owls
; in the owlery.
; 
; (A) Design a data definition to represent the weights of all the owls. 
;     For this problem call it ListOfNumber.
; (B) Design a function that consumes the weights of owls and produces
;     the total weight of all the owls.
; (C) Design a function that consumes the weights of owls and produces
;     the total number of owls.
;     



;; Data:

;; ListOfNumber is one of:
;; - empty
;; - (cons Number ListOfNumber)
;; interp. each number in the list is an owl weight in ounces
(define LON1 empty)
(define LON2 (cons 60 (cons 42 empty)))
#;
(define (fn-for-lon lon)
  (cond [(empty? lon) (...)]
        [else
         (... (first lon)
              (fn-for-lon (rest lon)))]))

;; Template rules used:
;; - one of: 2 items
;; - atomic distinct: empty
;; - compound: (cons Number ListOfNumber)
;; - self-refernence: (rest lon) is ListOfNumber


;; Functions:

;; ListOfNumber -> Float
;; return total weight of owls in list
(check-expect (totalweight empty) 0)
(check-expect (totalweight (cons 64 empty)) 64)
(check-expect (totalweight (cons 32 (cons 64 empty))) 96)

; (define (totalweight lon) 0) ;stub

; copy template from ListOfNumber
(define (totalweight lon)
  (cond [(empty? lon) 0]
        [else
         (+ (first lon)
            (totalweight (rest lon)))]))

;; ListOfNumber -> Int
;; return total number of owls in list
(check-expect (numberofowls empty) 0)
(check-expect (numberofowls (cons 64 empty)) 1)
(check-expect (numberofowls (cons 32 (cons 64 empty))) 2)

; (define (numberofowls lon) 0) ;stub

; copy template from ListOfNumber
(define (numberofowls lon)
  (cond [(empty? lon) 0]
        [else
         (+ 1
            (numberofowls (rest lon)))]))
