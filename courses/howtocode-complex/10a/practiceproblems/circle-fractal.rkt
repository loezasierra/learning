#lang racket

(require 2htdp/image)

;; circle-fractal-starter.rkt

; 
; PROBLEM :
; 
; Design a function that will create the following fractal:
; 
;             .
; 
;             
; 
; Each circle is surrounded by circles that are two-fifths smaller. 
; 
; You can build these images using the convenient beside and above functions
; if you make your actual recursive function be one that just produces the
; top leaf shape. You can then rotate that to produce the other three shapes.
; 
; You don't have to use this structure if you are prepared to use more
; complex place-image functions and do some arithmetic. But the approach
; where you use the helper is simpler.
; 
; Include a termination argument for your design.


;; =================
;; Constants:

(define STEP (/ 2 5))
(define TRIVIAL-SIZE 5)

; Termination Argument:
; 
; base case: (<= s TRIVIAL-SIZE)
; 
; reduction: (* s (/ 2 5))
; 
; argument: repeated multiplication by two-fifths will eventually
; be less than TRIVIAL-SIZE



;; Integer -> Imaage
;; produce a fractal image where each circle is surrounded by circles that are two-fifrths smaller
(check-expect (circlefrac TRIVIAL-SIZE) (circle TRIVIAL-SIZE "solid" "blue"))
(check-expect (circlefrac (/ TRIVIAL-SIZE STEP))
              (local [(define leaf (circle TRIVIAL-SIZE "solid" "blue"))
                      (define mid (circle (/ TRIVIAL-SIZE STEP) "solid" "blue"))]
                (above leaf
                       (beside leaf mid leaf)
                       leaf)))
(check-expect (circlefrac (/ (/ TRIVIAL-SIZE STEP) STEP))
              (local [(define mid (circle (/ (/ TRIVIAL-SIZE STEP) STEP) "solid" "blue"))
                      (define leaf
                        (local [(define edge (circle TRIVIAL-SIZE "solid" "blue"))
                                (define mid (circle (/ TRIVIAL-SIZE STEP) "solid" "blue"))]
                          (above edge
                                 (beside edge mid edge))))]
                (above leaf
                       (beside (rotate 90 leaf) mid (rotate 270 leaf))
                       (rotate 180 leaf))))

(define (circlefrac s)
  (local [(define (full-image s)
            (cond [(<= s TRIVIAL-SIZE) (circle s "solid" "blue")]
                  [else
                   (local [(define leaf (make-leaf (* s STEP)))
                           (define mid (circle s "solid" "blue"))]
                     (above leaf
                            (beside (rotate 90 leaf) mid (rotate 270 leaf))
                            (rotate 180 leaf)))]))
          
          (define (make-leaf s)
            (cond [(<= s TRIVIAL-SIZE) (circle s "solid" "blue")]
                  [else
                   (local [(define sub (make-leaf (* s STEP)))
                           (define mid (circle s "solid" "blue"))]
                     (above sub
                            (beside (rotate 90 sub) mid (rotate 270 sub))))]))]
    (full-image s)))
