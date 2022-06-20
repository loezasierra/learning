#lang racket

;; dropn-starter.rkt

; 
; PROBLEM:
; 
; Design a function that consumes a list of elements lox and a natural number
; n and produces the list formed by dropping every nth element from lox.
; 
; (dropn (list 1 2 3 4 5 6 7) 2) should produce (list 1 2 4 5 7)
; 



;; (litof X) Integer -> (listof X)
;; return a list where every nth element is dropped
(check-expect (dropn empty 0) empty)
(check-expect (dropn (list 1 2 3 4 5 6 7) 2) (list 1 2 4 5 7))
(check-expect (dropn (list 1 2 3 4 5 6 7 8) 3) (list 1 2 3 5 6 7))

(define (dropn lox0 n)
  ;; acc is Intger; number of elements left before dropping

  ;; (dropn (list 1 2 3 4 5 6 7) 2) ; outer call

  ;; (dropn (list 1 2 3 4 5 6 7) 2) ; include
  ;; (dropn (list   2 3 4 5 6 7) 1) ; include
  ;; (dropn (list     3 4 5 6 7) 0) ; drop
  ;; (dropn (list       4 5 6 7) 2) ; include
  ;; (dropn (list         5 6 7) 1) ; include
  ;; (dropn (list           6 7) 0) ; drop
  ;; (dropn (list             7) 2) ; include
  
  (local [(define (dropn lox acc)
            (cond [(empty? lox) empty]
                  [else
                   (if (zero? acc)
                       (dropn (rest lox) n)
                       (cons (first lox)
                             (dropn (rest lox)
                                    (sub1 acc))))]))]
    (dropn lox0 n)))
