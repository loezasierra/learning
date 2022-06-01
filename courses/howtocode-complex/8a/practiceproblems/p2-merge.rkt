#lang racket

;; merge-starter.rkt

; Problem:
; 
; Design the function merge. It consumes two lists of numbers, which it assumes are 
; each sorted in ascending order. It produces a single list of all the numbers, 
; also sorted in ascending order. 
; 
; Your solution should explicitly show the cross product of type comments table, 
; filled in with the values in each case. Your final function should have a cond 
; with 3 cases. You can do this simplification using the cross product table by 
; recognizing that there are subtly equal answers. 
; 
; Hint: Think carefully about the values of both lists. You might see a way to 
; change a cell content so that 2 cells have the same value.
; 


;; ============================
;; Data definitions:


;; ListOfString is one of:
;; - empty
;; - (cons String ListOfString)
;; interp. a list of strings

(define LS0 empty)
(define LS1 (cons "a" empty))
(define LS2 (cons "a" (cons "b" empty)))
(define LS3 (cons "c" (cons "b" (cons "a" empty))))

#;
(define (fn-for-los los)
  (cond [(empty? los) (...)]
        [else 
         (... (first los)
              (fn-for-los (rest los)))]))


;; =============================
;; Functions:


;; ListOfString ListOfString -> ListOfString
;; merges two sorted lists into a single sorted list
(check-expect (merge empty empty) empty)
(check-expect (merge empty (list 8)) (list 8))
(check-expect (merge (list 8) empty) (list 8))
(check-expect (merge (list 4) (list 4)) (list 4 4))
(check-expect (merge (list 2) (list 8)) (list 2 8))
(check-expect (merge (list 8) (list 2)) (list 2 8))
(check-expect (merge (list 2 6) (list 3 9)) (list 2 3 6 9))
(check-expect (merge (list 3 9) (list 2 6)) (list 2 3 6 9))
(check-expect (merge (list 2 4) (list 1 6 9)) (list 1 2 4 6 9))
(check-expect (merge (list 1 4 9) (list 2 6)) (list 1 2 4 6 9))

; (define (merge lista listb) empty) ;stub

(define (merge lista listb)
  (cond [(and (empty? lista) (empty? listb))
         empty]
        [(or (empty? lista) (empty? listb))
         (append lista listb)]
        [else
         (if (< (first lista) (first listb))
             (cons (first lista) (merge (rest lista) listb))
             (cons (first listb) (merge lista (rest listb)))
             )]))
