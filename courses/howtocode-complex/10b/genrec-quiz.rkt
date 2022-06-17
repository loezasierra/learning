#lang racket

(require 2htdp/image)

;  PROBLEM 1:
;  
;  In the lecture videos we designed a function to make a Sierpinski triangle fractal. 
;  
;  Here is another geometric fractal that is made of circles rather than triangles:
;  
;  .
;  
;  Design a function to create this circle fractal of size n and colour c.
;  


(define CUT-OFF 5)

;; Natural String -> Image
;; produce a circle fractal of size n and colour c
(check-expect (circle-fractal CUT-OFF "black") (circle CUT-OFF "outline" "black"))
(check-expect (circle-fractal (* 2 CUT-OFF) "red")
              (overlay (circle (* 2 CUT-OFF) "outline" "red")
                       (local [(define sub (circle CUT-OFF "outline" "red"))]
                         (beside sub sub))))
(check-expect (circle-fractal (* 4 CUT-OFF) "blue")
              (overlay (circle (* 4 CUT-OFF) "outline" "blue")
                       (local [(define sub (overlay (circle (* 2 CUT-OFF) "outline" "blue")
                                                    (local [(define sub (circle CUT-OFF "outline" "blue"))]
                                                      (beside sub sub))))]
                         (beside sub sub))))

; base: <= CUT-OFF
; reduction: n / 2
; argument: continual division of n by 2 will eventually get to less than CUT-OFF


(define (circle-fractal n c)
  (cond [(<= n CUT-OFF) (circle n "outline" c)]
        [else
         (overlay (circle n "outline" c)
                  (local [(define sub (circle-fractal (/ n 2) c))]
                    (beside sub sub)))]))


;  PROBLEM 2:
;  
;  Below you will find some data definitions for a tic-tac-toe solver. 
;  
;  In this problem we want you to design a function that produces all 
;  possible filled boards that are reachable from the current board. 
;  
;  In actual tic-tac-toe, O and X alternate playing. For this problem
;  you can disregard that. You can also assume that the players keep 
;  placing Xs and Os after someone has won. This means that boards that 
;  are completely filled with X, for example, are valid.
;  
;  Note: As we are looking for all possible boards, rather than a winning 
;  board, your function will look slightly different than the solve function 
;  you saw for Sudoku in the videos, or the one for tic-tac-toe in the 
;  lecture questions. 
;  


;; Value is one of:
;; - false
;; - "X"
;; - "O"
;; interp. a square is either empty (represented by false) or has and "X" or an "O"

(define (fn-for-value v)
  (cond [(false? v) (...)]
        [(string=? v "X") (...)]
        [(string=? v "O") (...)]))

;; Board is (listof Value)
;; a board is a list of 9 Values
(define B0 (list false false false
                 false false false
                 false false false))

(define B1 (list false "X"   "O"   ; a partly finished board
                 "O"   "X"   "O"
                 false false "X")) 

(define B2 (list "X"  "X"  "O"     ; a board where X will win
                 "O"  "X"  "O"
                 "X" false "X"))

(define B3 (list "X" "O" "X"       ; a board where Y will win
                 "O" "O" false
                 "X" "X" false))

(define (fn-for-board b)
  (cond [(empty? b) (...)]
        [else 
         (... (fn-for-value (first b))
              (fn-for-board (rest b)))]))



;; Board -> (listof Board)
;; return a list of all possible filled boards reachable from current board
(check-expect (possibleboards B2)
              (list (list "X"  "X"  "O" 
                          "O"  "X"  "O"
                          "X"  "X"  "X")
                    (list "X"  "X"  "O"  
                          "O"  "X"  "O"
                          "X"  "O"  "X")))
(check-expect (possibleboards B3)
              (list (list "X" "O" "X"
                          "O" "O" "X"
                          "X" "X" "X")
                    (list "X" "O" "X"
                          "O" "O" "X"
                          "X" "X" "O")
                    (list "X" "O" "X"
                          "O" "O" "O"
                          "X" "X" "X")
                    (list "X" "O" "X"
                          "O" "O" "O"
                          "X" "X" "O")))

(define (possibleboards b)
  (local [(define (fill-board b)
            (if (full-board? b)
                (cons b empty)
                (append (fill-board (fill-next-space b "X")) (fill-board (fill-next-space b "O")))))

          (define (full-board? b)
            (not (ormap false? b)))

          (define (fill-next-space b m)
            (cond [(empty? b) (error "Board should have an empty space")]
                  [else
                   (if (false? (first b))
                       (cons m (rest b))
                       (cons (first b) (fill-next-space (rest b) m)))]))]
    (fill-board b)))


;  PROBLEM 3:
;  
;  Now adapt your solution to filter out the boards that are impossible if 
;  X and O are alternating turns. You can continue to assume that they keep 
;  filling the board after someone has won though. 
;  
;  You can assume X plays first, so all valid boards will have 5 Xs and 4 Os.
;  
;  NOTE: make sure you keep a copy of your solution from problem 2 to answer 
;  the questions on edX.
;  



;; Board -> (listof Board)
;; return a list of all possible valid filled boards reachable from current board
(check-expect (possiblevalidboards B2)
              (list (list "X"  "X"  "O"  
                          "O"  "X"  "O"
                          "X"  "O"  "X")))
(check-expect (possiblevalidboards B3)
              (list (list "X" "O" "X"
                          "O" "O" "X"
                          "X" "X" "O")
                    (list "X" "O" "X"
                          "O" "O" "O"
                          "X" "X" "X")))

(define (possiblevalidboards b)
  (local [(define (valid-boards b)
            (prune-invalid (fill-board b)))

          (define (fill-board b)
            (if (full-board? b)
                (cons b empty)
                (append (fill-board (fill-next-space b "X")) (fill-board (fill-next-space b "O")))))

          (define (full-board? b)
            (not (ormap false? b)))

          (define (fill-next-space b m)
            (cond [(empty? b) (error "Board should have an empty space")]
                  [else
                   (if (false? (first b))
                       (cons m (rest b))
                       (cons (first b) (fill-next-space (rest b) m)))]))

          (define (prune-invalid lob)
            (filter valid-board? lob))

          (define (valid-board? b)
            (and (= 5 (count-marks b "X"))
                 (= 4 (count-marks b "O"))))

          (define (count-marks b m)
            (cond [(empty? b) 0]
                  [else
                   (if (equal? m (first b))
                       (+ 1 (count-marks (rest b) m))
                       (count-marks (rest b) m))]))]
    (valid-boards b)))
