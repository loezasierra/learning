#lang racket

(require 2htdp/image)
;  PROBLEM 1:
;  
;  Design an abstract function called arrange-all to simplify the 
;  above-all and beside-all functions defined below. Rewrite above-all and
;  beside-all using your abstract function.



;; (Image Image -> Image) (listof Image) -> Image
;; combines a list of images into a single image, arranged by placement
(check-expect (arrange-all above empty) empty-image)
(check-expect (arrange-all above (list (circle 20 "solid" "red") (circle 40 "solid" "blue")))
              (above (circle 20 "solid" "red") (circle 40 "solid" "blue")))

(define (arrange-all placement loi)
  (cond [(empty? loi) empty-image]
        [else
         (placement (first loi)
                    (arrange-all placement (rest loi)))]))


;; (listof Image) -> Image
;; combines a list of images into a single image, each image above the next one
(check-expect (above-all empty) empty-image)
(check-expect (above-all (list (rectangle 20 40 "solid" "red") (star 30 "solid" "yellow")))
              (above (rectangle 20 40 "solid" "red") (star 30 "solid" "yellow")))
(check-expect (above-all (list (circle 30 "outline" "black") (circle 50 "outline" "black") (circle 70 "outline" "black")))
              (above (circle 30 "outline" "black") (circle 50 "outline" "black") (circle 70 "outline" "black")))

;(define (above-all loi) empty-image)  ;stub

(define (above-all loi) (arrange-all above loi))



;; (listof Image) -> Image
;; combines a list of images into a single image, each image beside the next one
(check-expect (beside-all empty) (rectangle 0 0 "solid" "white"))
(check-expect (beside-all (list (rectangle 50 40 "solid" "blue") (triangle 30 "solid" "pink")))
              (beside (rectangle 50 40 "solid" "blue") (triangle 30 "solid" "pink")))
(check-expect (beside-all (list (circle 10 "outline" "red") (circle 20 "outline" "blue") (circle 10 "outline" "yellow")))
              (beside (circle 10 "outline" "red") (circle 20 "outline" "blue") (circle 10 "outline" "yellow")))

;(define (beside-all loi) empty-image)  ;stub

(define (beside-all loi) (arrange-all beside loi))





;  PROBLEM 2:
;  
;  Finish the design of the following functions, using built-in abstract functions. 
;  


;; Function 1
;; ==========

;; (listof String) -> (listof Natural)
;; produces a list of the lengths of each string in los
(check-expect (lengths empty) empty)
(check-expect (lengths (list "apple" "banana" "pear")) (list 5 6 4))

(define (lengths lst) (map string-length lst))



;; Function 2
;; ==========

;; (listof Natural) -> (listof Natural)
;; produces a list of just the odd elements of lon
(check-expect (odd-only empty) empty)
(check-expect (odd-only (list 1 2 3 4 5)) (list 1 3 5))

(define (odd-only lon) (filter odd? lon))



;; Function 3
;; ==========

;; (listof Natural -> Boolean
;; produce true if all elements of the list are odd
(check-expect (all-odd? empty) true)
(check-expect (all-odd? (list 1 2 3 4 5)) false)
(check-expect (all-odd? (list 5 5 79 13)) true)

(define (all-odd? lon) (andmap odd? lon))



;; Function 4
;; ==========

;; (listof Natural) -> (listof Natural)
;; subtracts n from each element of the list
(check-expect (minus-n empty 5) empty)
(check-expect (minus-n (list 4 5 6) 1) (list 3 4 5))
(check-expect (minus-n (list 10 5 7) 4) (list 6 1 3))

(define (minus-n lon n)
  (local [(define (sub-n i) (- i n))]
    (map sub-n lon)))



;  PROBLEM 3
;  
;  Consider the data definition below for Region. Design an abstract fold function for region, 
;  and then use it do design a function that produces a list of all the names of all the 
;  regions in that region.
;  
;  For consistency when answering the multiple choice questions, please order the arguments in your
;  fold function with combination functions first, then bases, then region. Please number the bases 
;  and combination functions in order of where they appear in the function.
;  
;  So (all-regions CANADA) would produce 
;  (list "Canada" "British Columbia" "Vancouver" "Victoria" "Alberta" "Calgary" "Edmonton")


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


;; (String Z Y -> X) (X Y -> Y) Y Z Z Z Z Z Region -> X
;; Abstract fold function for Region

;; constants for testing
(define (t-c-reg name t sub) (+ t sub))
(define (t-c-lor r lor) (+ r lor))
(define t-b-lor 0)
(define t-cont 0)
(define t-coun 0)
(define t-prov 0)
(define t-stat 0)
(define t-city 1)
(check-expect (fold-region t-c-reg t-c-lor t-b-lor t-cont t-coun t-prov t-stat t-city VANCOUVER)
              1) ; Count cities in Vancouver region
(check-expect (fold-region t-c-reg t-c-lor t-b-lor t-cont t-coun t-prov t-stat t-city CANADA)
              4) ; Count cities in Canada region

(define (fold-region c-reg c-lor b-lor cont coun prov stat city r)
  (local [(define (fn-for-region r)
            (c-reg (region-name r)
                   (fn-for-type (region-type r))
                   (fn-for-lor (region-subregions r))))
          
          (define (fn-for-type t)
            (cond [(string=? t "Continent") cont]
                  [(string=? t "Country") coun]
                  [(string=? t "Province") prov]
                  [(string=? t "State") stat]
                  [(string=? t "City") city]))
          
          (define (fn-for-lor lor)
            (cond [(empty? lor) b-lor]
                  [else 
                   (c-lor (fn-for-region (first lor))
                          (fn-for-lor (rest lor)))]))]
    (fn-for-region r)))


;; Region -> (listof String)
;; returns a list of all the names of all the regions in Region
(check-expect (all-regions VANCOUVER) (list "Vancouver"))
(check-expect (all-regions CANADA) (list "Canada" "British Columbia" "Vancouver" "Victoria" "Alberta" "Calgary" "Edmonton"))

(define (all-regions r)
  (local [(define (c-reg name type subs) (cons name subs))
          (define (c-lor r lor) (append r lor))
          (define b-lor empty)
          (define cont false)
          (define coun false)
          (define prov false)
          (define stat false)
          (define city false)]
  (fold-region c-reg c-lor b-lor cont coun prov stat city r)))
