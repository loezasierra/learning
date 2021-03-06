#lang racket

(require 2htdp/image)

;; fractals-starter.rkt

; 
; PROBLEM: 
; 
; Design a function that consumes a number and produces a Sierpinski
; triangle of that size. Your function should use generative recursion.
; 
; One way to draw a Sierpinski triangle is to:
; 
;  - start with an equilateral triangle with side length s
;  
;      .
;      
;  - inside that triangle are three more Sierpinski triangles
;      .
;      
;  - and inside each of those... and so on
;  
; So that you end up with something that looks like this:
;    
; 
;    
; 
; .
;    
; Note that in the 2nd picture above the inner triangles are drawn in 
; black and slightly smaller just to make them clear. In the real
; Sierpinski triangle they should be in the same color and of side
; length s/2. Also note that the center upside down triangle is not
; an explicit triangle, it is simply formed from the other triangles.
; 
; 


(define CUTOFF 5)

;; Number -> Image
;; produce a Sierpinski Triangle of given size
(check-expect (stri CUTOFF) (triangle CUTOFF "outline" "red"))
(check-expect (stri (* CUTOFF 2))
              (overlay (triangle (* 2 CUTOFF) "outline" "red")
                       (local [(define sub (triangle CUTOFF "outline" "red"))]
                         (above           sub
                                          (beside sub sub)))))

; (define (stri s) (square 0 "solid" "white")) ;stub

#;
(define (genrec-fn d) ;template
  (cond [(trivial? d) (trivial-answer d)]
        [else
         (... d 
              (genrec-fn (next-problem d)))]))

(define (stri s)
  (cond [(<= s CUTOFF) (triangle s "outline" "red")]
        [else
         (overlay (triangle s "outline" "red")
                  (local [(define sub (stri (/ s 2)))]
                    (above sub
                           (beside sub sub))))]))


; 
; PROBLEM:
; 
; Design a function to produce a Sierpinski carpet of size s.
; 
; Here is an example of a larger Sierpinski carpet.
; 
; .
; 



;; Integer -> Image
;; produce a Sierpinski carpet of given size
(check-expect (sier-carpet CUTOFF) (square CUTOFF "outline" "red"))
(check-expect (sier-carpet (* CUTOFF 2))
              (local [(define sub (square (/ (* CUTOFF 2) 3) "outline" "red"))]
                (above (beside sub sub sub)
                       (beside sub (square (/ (* CUTOFF 2) 3) "solid" "white") sub)
                       (beside sub sub sub))))

(define (sier-carpet s)
  (cond [(<= s CUTOFF) (square s "outline" "red")]
        [else
         (local [(define sub (sier-carpet (/ s 3)))
                 (define blank (square (/ s 3) "solid" "white"))]
           (above (beside sub sub sub)
                  (beside sub blank sub)
                  (beside sub sub sub)))]))
