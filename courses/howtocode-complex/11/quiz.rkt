#lang racket

;  PROBLEM 1:
;  
;  Assuming the use of at least one accumulator, design a function that consumes a list of strings,
;  and produces the length of the longest string in the list. 
;  



;; (listof String) -> Integer
;; return the length of the longest string in the list
(check-expect (longest-string empty) 0)
(check-expect (longest-string (list "a")) 1)
(check-expect (longest-string (list "bc" "a" "defg")) 4)
(check-expect (longest-string (list "a" "bc" "defghij" "klmn")) 7)

(define (longest-string los)
  ;; lng is integer; length of longest string seen so far
  ;; (longest-string (list "bc" "a" "defg"))
  ;; (longest-string (list "bc" "a" "defg") 0)
  ;; (longest-string (list      "a" "defg") 2)
  ;; (longest-string (list          "defg") 2)
  ;; (longest-string (list                ) 4)
  (local [(define (fn-for-los los lng)
            (cond [(empty? los) lng]
                  [else
                   (if (> (string-length (first los)) lng)
                       (fn-for-los (rest los) (string-length (first los)))
                       (fn-for-los (rest los) lng))]))]
    (fn-for-los los 0)))


;  PROBLEM 2:
;  
;  The Fibbonacci Sequence https://en.wikipedia.org/wiki/Fibonacci_number is 
;  the sequence 0, 1, 1, 2, 3, 5, 8, 13,... where the nth element is equal to 
;  n-2 + n-1. 
;  
;  Design a function that given a list of numbers at least two elements long, 
;  determines if the list obeys the fibonacci rule, n-2 + n-1 = n, for every 
;  element in the list. The sequence does not have to start at zero, so for 
;  example, the sequence 4, 5, 9, 14, 23 would follow the rule. 
;  



;; (listof Integer) -> Bool
;; return true if list of numbers obeys the fibonacci rule
;; ASSUME: List is at least 2 elements long
(check-expect (fib (list 0 1)) true)
(check-expect (fib (list 5 9)) true)
(check-expect (fib (list 0 1 5)) false)
(check-expect (fib (list 0 1 1 2 9)) false)
(check-expect (fib (list 0 1 1 2 3 5 8 13 21 34 55)) true)
(check-expect (fib (list 4 5 9 14 23)) true)

(define (fib lon0)
  ;; n1 is n-1 of current element in lon0
  ;; n2 is n-2 of current element in lon0
  (local [(define (fib lon n2 n1)
            (cond [(empty? lon) true]
                  [else
                   (if (= (first lon) (+ n1 n2))
                       (fib (rest lon) n1 (first lon))
                       false)]))]
    (fib (rest(rest lon0)) (first lon0) (first (rest lon0)))))

;  PROBLEM 3:
;  
;  Refactor the function below to make it tail recursive.  
;  


;; Natural -> Natural
;; produces the factorial of the given number
(check-expect (fact 0) 1)
(check-expect (fact 3) 6)
(check-expect (fact 5) 120)

(define (fact n)
  ;; rsf is results-so-far
  ;; (fact 3) ; outter call
  ;; (fact 3 1) 3)
  ;; (fact 2 3) 6)
  ;; (fact 1 6) 6)
  ;; (fact 0 6) 6)
  (local [(define (fact n rsf)
            (cond [(zero? n) rsf]
                  [else 
                   (fact (sub1 n) (* n rsf))]))]
    (fact n 1)))

#;
(define (fact n)
  (cond [(zero? n) 1]
        [else 
         (* n (fact (sub1 n)))]))



;  PROBLEM 4:
;  
;  Recall the data definition for Region from the Abstraction Quiz. Use a worklist 
;  accumulator to design a tail recursive function that counts the number of regions 
;  within and including a given region. 
;  So (count-regions CANADA) should produce 7



(define-struct region (name type subregions))
;; Region is (make-region String Type (listof Region))
;; interp. a geographical region

;; Type is one of:
;; - "Continent"
;; - "Country"
;; - "Province"
;; - "State"
;; - "City"
;; interp. categories of geographical regions

(define VANCOUVER (make-region "Vancouver" "City" empty))
(define VICTORIA (make-region "Victoria" "City" empty))
(define BC (make-region "British Columbia" "Province" (list VANCOUVER VICTORIA)))
(define CALGARY (make-region "Calgary" "City" empty))
(define EDMONTON (make-region "Edmonton" "City" empty))
(define ALBERTA (make-region "Alberta" "Province" (list CALGARY EDMONTON)))
(define CANADA (make-region "Canada" "Country" (list BC ALBERTA)))

#;
(define (fn-for-region r)
  (local [(define (fn-for-region r)
            (... (region-name r)
                 (fn-for-type (region-type r))
                 (fn-for-lor (region-subregions r))))
          
          (define (fn-for-type t)
            (cond [(string=? t "Continent") (...)]
                  [(string=? t "Country") (...)]
                  [(string=? t "Province") (...)]
                  [(string=? t "State") (...)]
                  [(string=? t "City") (...)]))
          
          (define (fn-for-lor lor)
            (cond [(empty? lor) (...)]
                  [else 
                   (... (fn-for-region (first lor))
                        (fn-for-lor (rest lor)))]))]
    (fn-for-region r)))


;; Region -> Integer
;; return number of regions within and including a given region
(check-expect (count-regions VANCOUVER) 1)
(check-expect (count-regions BC) 3)
(check-expect (count-regions CANADA) 7)

(define (count-regions r)
  ;; cnt is count of regions so far
  ;; todo is work list accumulator
  (local [(define (fn-for-region r todo cnt)
            (fn-for-lor (append (region-subregions r) todo)
                        (add1 cnt)))
          
          (define (fn-for-lor todo cnt)
            (cond [(empty? todo) cnt]
                  [else 
                   (fn-for-region (first todo)
                                  (rest todo)
                                  cnt)]))]
    (fn-for-region r empty 0)))
