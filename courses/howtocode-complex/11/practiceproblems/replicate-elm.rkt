#lang racket

;; replicate-elm-starter.rkt

; 
; PROBLEM:
; 
; Design a function that consumes a list of elements and a natural n, and produces 
; a list where each element is replicated n times. 
; 
; (replicate-elm (list "a" "b" "c") 2) should produce (list "a" "a" "b" "b" "c" "c")
; 



;; (listof X) Integer -> (listof X)
;; returns a list where each element is replicated n times
(check-expect (replicate-elm (list "a" "b" "c") 1) (list "a" "b" "c"))
(check-expect (replicate-elm (list "a" "b" "c") 2) (list "a" "a" "b" "b" "c" "c"))
(check-expect (replicate-elm (list "a" "b" "c") 3) (list "a" "a" "a" "b" "b" "b" "c" "c" "c"))

(define (replicate-elm lox0 n)
  ;; acc is Integer; number of times (first lox) has been added to new list
  
  ;; (replicate-elm (list "a" "b" "c") 2) ;; outer call

  ;; (replicate-elm (list "a" "b" "c") 1) ; add "a" to list
  ;; (replicate-elm (list "a" "b" "c") 2) ; add "a" to list
  ;; (replicate-elm (list     "b" "c") 1) ; add "b" to list
  ;; (replicate-elm (list     "b" "c") 2) ; add "b" to list
  ;; (replicate-elm (list         "c") 1) ; add "c" to list
  ;; (replicate-elm (list         "c") 2) ; add "c" to list
  (local [(define (replicate-elm lox acc)
            (cond [(empty? lox) empty]
                  [else
                   (if (= acc n)
                       (cons (first lox) (replicate-elm (rest lox) 1))
                       (cons (first lox) (replicate-elm lox (add1 acc))))]))]
    (replicate-elm lox0 1)))
