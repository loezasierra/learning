#lang racket

(require 2htdp/image)

;; fold-functions-starter.rkt

;; At this point in the course, the type (listof X) means:

;; ListOfX is one of:
;; - empty
;; - (cons X ListOfX)
;; interp. a list of X

;; and the template for (listof X) is:

(define (fn-for-lox lox)
  (cond [(empty? lox) (...)]
        [else
         (... (first lox)
              (fn-for-lox (rest lox)))]))

; 
; PROBLEM:
; 
; Design an abstract fold function for (listof X). 
; 



;; (X Y -> Y) Y (listof X) -> Y
;; abstract fold function for (listof X)
(check-expect (fold + 0 (list 1 2 3)) (+ 1 2 3 0))
(check-expect (fold * 1 (list 1 2 3)) (* 1 2 3 1))

(define (fold fn b lox)
  (cond [(empty? lox) b]
        [else
         (fn (first lox)
             (fold fn b (rest lox)))]))


; 
; PROBLEM:
; 
; Complete the function definition for sum using fold. 
; 


;; (listof Number) -> Number
;; add up all numbers in list
(check-expect (sum empty) 0)
(check-expect (sum (list 2 3 4)) 9)

; (define (sum lon) 0) ;stub

(define (sum lon) (fold + 0 lon))


; 
; PROBLEM:
; 
; Complete the function definition for juxtapose using foldr. 
; 

;; (listof Image) -> Image
;; juxtapose all images beside each other
(check-expect (juxtapose empty) (square 0 "solid" "white"))
(check-expect (juxtapose (list (triangle 6 "solid" "yellow")
                               (square 10 "solid" "blue")))
              (beside (triangle 6 "solid" "yellow")
                      (square 10 "solid" "blue")
                      (square 0 "solid" "white")))

; (define (juxtapose loi) (square 0 "solid" "white")) ;stub

(define (juxtapose loi)
  (fold beside (square 0 "solid" "white") loi))


; 
; PROBLEM:
; 
; Complete the function definition for copy-list using foldr. 
; 

;; (listof X) -> (listof X)
;; produce copy of list
(check-expect (copy-list empty) empty)
(check-expect (copy-list (list 1 2 3)) (list 1 2 3))

; (define (copy-list lox) empty) ;stub

(define (copy-list lox)
  (fold cons empty lox))

;; ======================

; 
; PROBLEM:
; 
; Design an abstract fold function for Element (and (listof Element)). 
; 



(define-struct elt (name data subs))
;; Element is (make-elt String Integer ListOfElement)
;; interp. An element in the file system, with name, and EITHER data or subs.
;;         If data is 0, then subs is considered to be list of sub elements.
;;         If data is not 0, then subs is ignored.

;; ListOfElement is one of:
;;  - empty
;;  - (cons Element ListOfElement)
;; interp. A list of file system Elements

; .

(define F1 (make-elt "F1" 1 empty))
(define F2 (make-elt "F2" 2 empty))
(define F3 (make-elt "F3" 3 empty))
(define D4 (make-elt "D4" 0 (list F1 F2)))
(define D5 (make-elt "D5" 0 (list F3)))
(define D6 (make-elt "D6" 0 (list D4 D5)))
#;
(define (fn-for-element e)
  (local [(define (fn-for-element e)
            (... (elt-name e)    ;String
                 (elt-data e)    ;Integer
                 (fn-for-loe (elt-subs e))))

          (define (fn-for-loe loe)
            (cond [(empty? loe) (...)]
                  [else
                   (... (fn-for-element (first loe))
                        (fn-for-loe (rest loe)))]))]
    (fn-for-element e)))


;; (String Integer Y -> X) (X Y -> Y) Y Element -> X
;; abstract fold function for Element
(check-expect (local [(define (c1 name data rest) (cons name rest))]
                (fold-element c1 append empty D6))
              (list "D6" "D4" "F1" "F2" "D5" "F3"))

(define (fold-element c1 c2 b e)
  (local [(define (fn-for-element e)
            (c1 (elt-name e)    ;String
                (elt-data e)    ;Integer
                (fn-for-loe (elt-subs e))))

          (define (fn-for-loe loe)
            (cond [(empty? loe) b]
                  [else
                   (c2 (fn-for-element (first loe))
                       (fn-for-loe (rest loe)))]))]
    (fn-for-element e)))



; 
; PROBLEM
; 
; Complete the design of sum-data that consumes Element and producs
; the sum of all the data in the element and its subs
; 

;; Element -> Integer
;; produce the sum of all the data in element (and its subs)
(check-expect (sum-data F1) 1)
(check-expect (sum-data D5) 3)
(check-expect (sum-data D4) (+ 1 2))
(check-expect (sum-data D6) (+ 1 2 3))

; (define (sum-data e) 0) ;stub

(define (sum-data e)
  (local [(define (sum-data name data rest) (+ data rest))]
    (fold-element sum-data + 0 e)))

; 
; PROBLEM
; 
; Complete the design of all-names that consumes Element and produces a list of the
; names of all the elements in the tree. 
; 

;; Element       -> ListOfString
;; produce list of the names of all the elements in the tree
(check-expect (all-names F1) (list "F1"))
(check-expect (all-names D5) (list "D5" "F3"))
(check-expect (all-names D4) (list "D4" "F1" "F2"))
(check-expect (all-names D6) (list "D6" "D4" "F1" "F2" "D5" "F3"))
               
; (define (all-names e) empty) ;stub

(define (all-names e)
  (local [(define (all-names name data rest) (cons name rest))]
    (fold-element all-names append empty e)))

; 
; PROBLEM
; 
; If the tree is very large, then fold-element is not a good way to implement the find 
; function from last week.  Why? If you aren't sure then discover the answer by implementing
; find using fold-element and then step the two versions with different arguments.
; 
; 


;; ANSWER
;; Using fold-element, since we don't have access to the internal structure of the original function
;; we would have to pass in a boolean as c2. This means we would have recomputation.
;; 1. we would have to compute whether to produce true or false in our high level 'find' function (using recursion)
;; 2. the low level 'fold-element' function would again compute the recursion to go back in and get the data we are looking for
