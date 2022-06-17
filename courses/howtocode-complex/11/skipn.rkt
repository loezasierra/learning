#lang racket

;; skipn-starter.rkt

; 
; PROBLEM:
; 
; Design a function that consumes a list of elements lox and a natural number
; n and produces the list formed by including the first element of lox, then 
; skipping the next n elements, including an element, skipping the next n 
; and so on.
; 
;  (skipn (list "a" "b" "c" "d" "e" "f") 2) should produce (list "a" "d")
; 



;; (listof X) Integer -> (listof X)
;; returns a list where every n elements after first element is skipped
(check-expect (skipn empty 2) empty)
(check-expect (skipn (list "a" "b" "c" "d" "e" "f") 2) (list "a" "d"))
(check-expect (skipn (list "a" "b" "c" "d" "e" "f") 3) (list "a" "e"))
(check-expect (skipn (list "a" "b" "c" "d" "e" "f") 4) (list "a" "f"))

; counting down
#;
(define (skipn lox0 n)
  ;; acc is Integer; the number of elements to skip before including the next one
  ;; initial call: (skipn (list "a" "b" "c" "d" "e" "f") 2)
  
  ;; (skipn (list "a" "b" "c" "d" "e" "f") 0) ;include
  ;; (skipn (list     "b" "c" "d" "e" "f") 2) ;exclude
  ;; (skipn (list         "c" "d" "e" "f") 1) ;exclude
  ;; (skipn (list             "d" "e" "f") 0) ;include
  ;; (skipn (list                 "e" "f") 2)
  ;; (skipn (list                     "f") 1)
  (local [(define (skipn lox acc)
            (cond [(empty? lox) empty]
                  [else
                   (if (zero? acc)
                       (cons (first lox)
                             (skipn (rest lox)
                                    n))
                       (skipn (rest lox)
                              (sub1 acc)))]))]
(skipn lox0 0)))

; counting up
#;
(define (skipn lox0 n)
  ;; acc is Integer; the number of elements to skip before including the next one
  ;; initial call: (skipn (list "a" "b" "c" "d" "e" "f") 2)
  
  ;; (skipn (list "a" "b" "c" "d" "e" "f") 2) ;include
  ;; (skipn (list     "b" "c" "d" "e" "f") 0) ;exclude
  ;; (skipn (list         "c" "d" "e" "f") 1) ;exclude
  ;; (skipn (list             "d" "e" "f") 2) ;include
  ;; (skipn (list                 "e" "f") 0)
  ;; (skipn (list                     "f") 1)
  (local [(define (skipn lox acc)
            (cond [(empty? lox) empty]
                  [else
                   (if (= acc n)
                       (cons (first lox)
                             (skipn (rest lox)
                                    0))
                       (skipn (rest lox)
                              (add1 acc)))]))]
(skipn lox0 n)))
