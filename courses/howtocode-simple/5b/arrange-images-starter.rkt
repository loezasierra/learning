#lang racket

(require 2htdp/image)

;; arrange-images-starter.rkt (problem statement)

; 
; PROBLEM:
; 
; In this problem imagine you have a bunch of pictures that you would like to 
; store as data and present in different ways. We'll do a simple version of that 
; here, and set the stage for a more elaborate version later.
; 
; (A) Design a data definition to represent an arbitrary number of images.
; 
; (B) Design a function called arrange-images that consumes an arbitrary number
;     of images and lays them out left-to-right in increasing order of size.
;     



;; ========================
;; Constants

;; for testing:

(define I1 (circle 20 "solid" "red"))
(define I2 (circle 30 "solid" "blue"))
(define I3 (circle 40 "solid" "green"))


;; =========================
;; Data definitions


;; ListOfImage is one of:
;; - empty
;; - (cons Image ListOfImage)
;; interp. a list of images
(define L0 empty) ; an empty list
(define L1 (cons I1 empty)) ; single element list
(define L2 (cons I1 (cons I2 empty))) ; 2 element list

(define (fn-for-loi loi)
  (cond [(empty? loi) (...)]
        [else
         (... (first loi)
              (fn-for-loi (rest loi)))]))


;; ==========================
;; Functions


;; ListOfImage -> Image
;; sort images in increasing order of size, then lay them out left to right
(check-expect (arrange-images empty) empty-image)
(check-expect (arrange-images (cons I1 (cons I2 empty)))
              (beside I1 I2 empty-image))
(check-expect (arrange-images (cons I2 (cons I1 empty)))
              (beside I1 I2 empty-image))

; (define (arrange-images loi) empty-image) ;stub

(define (arrange-images loi)
  (layout-images (sort-images loi)))


;; ListOfImage -> Image
;; place images beside each other in order of list
(check-expect (layout-images empty) empty-image)
(check-expect (layout-images (cons I1 (cons I2 empty)))
              (beside I1 I2 empty-image))

; (define (layout-images loi) empty-image) ;stub

(define (layout-images loi)
  (cond [(empty? loi) empty-image]
        [else
         (beside (first loi)
                 (layout-images (rest loi)))]))


;; ListOfImage -> ListOfImage
;; sort images in increasing order of size
(check-expect (sort-images empty) empty) ; check base case
(check-expect (sort-images (cons I1 (cons I2 empty)))
              (cons I1 (cons I2 empty))) ; check sorted list
(check-expect (sort-images (cons I2 (cons I1 empty)))
              (cons I1 (cons I2 empty))) ; check unsorted list of 2 elements
(check-expect (sort-images (cons I3 (cons I1 (cons I2 empty))))
              (cons I1 (cons I2 (cons I3 empty)))) ; check unsorted list of 3 elements

; (define (sort-images loi) loi) ;stub

(define (sort-images loi)
  (cond [(empty? loi) empty]
        [else
         (insert (first loi)
                 (sort-images (rest loi)))])) ; result of natural recursion will be sorted


;; Image ListOfImage -> ListOfImage
;; insert img in proper place in list (in increasing order of size)
;; ASSUME: list is already sorted
(check-expect (insert I1 empty) (cons I1 empty)) ; check base case
(check-expect (insert I1 (cons I2 (cons I3 empty))) (cons I1 (cons I2 (cons I3 empty)))) ; check element goes at beginning
(check-expect (insert I2 (cons I1 (cons I3 empty))) (cons I1 (cons I2 (cons I3 empty)))) ; check element goes in middle
(check-expect (insert I3 (cons I1 (cons I2 empty))) (cons I1 (cons I2 (cons I3 empty)))) ; check element goes at end

; (define (insert img loi) loi) ;stub

(define (insert img loi)
  (cond [(empty? loi) (cons img empty)]
        [else
         (if (larger? img (first loi))
             (cons (first loi)
                   (insert img 
                           (rest loi)))
             (cons img loi))]))


;; Image Image -> Boolean
;; return true if img1 is larger than img2 (by area)
(check-expect (larger? (rectangle 1 6 "solid" "red") (rectangle 2 3 "solid" "blue")) false) ; check where images are equal
(check-expect (larger? (rectangle 3 2 "solid" "red") (rectangle 6 1 "solid" "blue")) false) ; check where images are equal
(check-expect (larger? (rectangle 9 6 "solid" "red") (rectangle 2 3 "solid" "blue")) true)  ; check where 1st image is larger
(check-expect (larger? (rectangle 1 9 "solid" "red") (rectangle 2 3 "solid" "blue")) true)  ; check where 1st image is larger
(check-expect (larger? (rectangle 1 6 "solid" "red") (rectangle 9 3 "solid" "blue")) false) ; check where 2nd image is larger
(check-expect (larger? (rectangle 1 6 "solid" "red") (rectangle 2 9 "solid" "blue")) false) ; check where 2nd image is larger

; (define (larger? img1 img2) false) ;stub

(define (larger? img1 img2)
  (> (* (image-width img1) (image-height img1))
     (* (image-width img2) (image-height img2))))
