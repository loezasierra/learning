#lang racket

(require 2htdp/image)

;; Data definitions

(define-struct elt (name data subs))
;; Element is (make-elt String Integer ListOfElement)
;; interp. An element in the file system, with name, and EITHER data or subs.
;;         If data is 0, then subs is considered to be list of sub elements.
;;         If data is not 0, then subs is ignored.

;; ListOfElement is on of:
;; - empty
;; - (cons Element ListOfElement)
;; interp. a list of file system Elements

; .


(define F1 (make-elt "F1" 1 empty))
(define F2 (make-elt "F2" 2 empty))
(define D4 (make-elt "D4" 0 (list F1 F2)))
(define F3 (make-elt "F3" 3 empty))
(define D5 (make-elt "D5" 0 (list F3)))
(define D6 (make-elt "D6" 0 (list D4 D5)))

(define (fn-for-element e)
  (... (elt-name e)        ; String
       (elt-data e)        ; Integer
       (fn-for-loe (elt-subs e)))) ; ListOfElement

(define (fn-for-loe loe)
  (cond [(empty? loe) (...)]
        [else
         (... (fn-for-element (first loe))
              (fn-for-loe (rest loe))
              )]))



;; Functions


; 
; PROBLEM
; 
; Design a function that consumes Element and produces the sum of all the file data in 
; the tree.
; 



;; Element -> Integer
;; ListOfElement -> Integer
;; return sum of all file data in Element
(check-expect (sum-data F1) 1)
(check-expect (sum-data-in-list empty) 0)
(check-expect (sum-data D5) 3)
(check-expect (sum-data D4) (+ 1 2))
(check-expect (sum-data-in-list (list F1 F2)) (+ 1 2))
(check-expect (sum-data D6) (+ 1 2 3))

; (define (sum-data e) 0)
; (define (sum-data-in-list loe) 0)

(define (sum-data e)
  (if (zero? (elt-data e))
      (sum-data-in-list (elt-subs e))
      (elt-data e)))

(define (sum-data-in-list loe)
  (cond [(empty? loe) 0]
        [else
         (+ (sum-data (first loe))
            (sum-data-in-list (rest loe))
            )]))



; 
; PROBLEM
; 
; Design a function that consumes Element and produces a list of the names of all the elements in 
; the tree. 
; 

; .


;; Element -> ListOfString
;; ListOfElement -> ListOfString ??
;; return list of the names of all the elements in Element
(check-expect (list-names F1) (list "F1"))
(check-expect (list-names-in-list empty) empty)
(check-expect (list-names D5) (list "D5" "F3"))
(check-expect (list-names D4) (list "D4" "F1" "F2"))
(check-expect (list-names-in-list (list F1 F2)) (list "F1" "F2"))
(check-expect (list-names D6) (list "D6" "D4" "F1" "F2" "D5" "F3"))

; (define (list-names e) (list ""))
; (define (list-names-in-list loe) (list ""))

(define (list-names e)
  (cons (elt-name e)
        (list-names-in-list (elt-subs e))))

(define (list-names-in-list loe)
  (cond [(empty? loe) empty]
        [else
         (append (list-names (first loe))
                 (list-names-in-list (rest loe))
                 )]))



; 
; PROBLEM
; 
; Design a function that consumes String and Element and looks for a data element with the given 
; name. If it finds that element it produces the data, otherwise it produces false.
; 

; .



;; String Element -> Integer or False
;; String ListOfElement -> Integer or False
;; return Data of given Element Name in Element Tree. return false if not found.
(check-expect (find-data "F1" F1) 1)
(check-expect (find-data "F5" F1) false)
(check-expect (find-data-in-list "F1" empty) false)
(check-expect (find-data "F2" D4) 2)
(check-expect (find-data-in-list "F2" (cons F1 (cons F2 empty))) 2)
(check-expect (find-data "F3" D6) 3)
(check-expect (find-data "F4" D6) false)

; (define (find-data n e) false)
; (define (find-data-in-list n loe) false)

(define (find-data n e)
  (if (string=? n (elt-name e))
      (elt-data e)
      (find-data-in-list n (elt-subs e))))

(define (find-data-in-list n loe)
  (cond [(empty? loe) false]
        [else
         (if (not (false? (find-data n (first loe))))
             (find-data n (first loe))
             (find-data-in-list n (rest loe))
             )]))


; 
; PROBLEM
; 
; Design a function that consumes Element and produces a rendering of the tree. For example: 
; 
; (render-tree D6) should produce something like the following.
; .
; 
; HINTS:
;   - This function is not very different than the first two functions above.
;   - Keep it simple! Start with a not very fancy rendering like the one above.
;     Once that works you can make it more elaborate if you want to.
;   - And... be sure to USE the recipe. Not just follow it, but let it help you.
;     For example, work out a number of examples BEFORE you try to code the function. 
;     



;; Element -> Image
;; ListOfElement -> Image
;; return an tree image rendering of given Element
(define TEXT-SIZE 16)
(define TEXT-COLOR "black")
(define BRANCH-SPACING (rectangle 30 0 "solid" "white"))

(check-expect (render-element F1) (above (text "F1" TEXT-SIZE TEXT-COLOR) empty-image))
(check-expect (render-list-of-elements empty) empty-image)
(check-expect (render-list-of-elements (list F1 F2))
              (beside (text "F1" TEXT-SIZE TEXT-COLOR)
                      BRANCH-SPACING
                      (text "F2" TEXT-SIZE TEXT-COLOR)))
(check-expect (render-element D4)
              (above (text "D4" TEXT-SIZE TEXT-COLOR)
                     (beside (text "F1" TEXT-SIZE TEXT-COLOR)
                             BRANCH-SPACING
                             (text "F2" TEXT-SIZE TEXT-COLOR))
                     empty-image))
(check-expect (render-element D6)
              (above (text "D6" TEXT-SIZE TEXT-COLOR)
                     (beside (above (text "D4" TEXT-SIZE TEXT-COLOR)
                                    (beside (text "F1" TEXT-SIZE TEXT-COLOR)
                                            BRANCH-SPACING
                                            (text "F2" TEXT-SIZE TEXT-COLOR))
                                    empty-image)
                             BRANCH-SPACING
                             (above (text "D5" TEXT-SIZE TEXT-COLOR)
                                    (text "F3" TEXT-SIZE TEXT-COLOR)
                                    empty-image))))

; (define (render-element e) empty-image)
; (define (render-list-of-elements loe) empty-image)

(define (render-element e)
  (above (text (elt-name e) TEXT-SIZE TEXT-COLOR)
         (render-list-of-elements (elt-subs e))))

(define (render-list-of-elements loe)
  (cond [(empty? loe) empty-image]
        [else
         (beside (render-element (first loe))
                 (if (not (equal? (render-list-of-elements (rest loe)) empty-image))
                     BRANCH-SPACING empty-image)
                 (render-list-of-elements (rest loe))
                 )]))
