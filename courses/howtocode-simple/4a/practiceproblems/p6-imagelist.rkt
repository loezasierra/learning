#lang racket

(require 2htdp/image)

;; image-list-starter.rkt

;; =================
;; Data definitions:

; 
; PROBLEM A:
; 
; Design a data definition to represent a list of images. Call it ListOfImage. 
; 



;; ListOfImage is one of:
;; - empty
;; - (cons Image ListOfImage)
;; interp. a list of images
(define LOI1 empty) ; empty list
(define LOI2 (cons (rectangle 20 30 "solid" "red") empty)) ; list with 1 element
(define LOI3 (cons (rectangle 50 35 "solid" "blue")
                   (cons (rectangle 20 30 "solid" "red") empty))) ; list with 2 elements
#;
(define (fn-for-loi loi)
  (cond [(empty? loi) (...)]
        [else (... (first loi)
                   (fn-for-loi (rest loi)))]))

;; Templates rules used:
;; - one of: 2 cases
;; - atomic distinct: empty
;; - compound: (cons Image ListOfImage)
;; - self reference: (rest loi)


;; =================
;; Functions:

; 
; PROBLEM B:
; 
; Design a function that consumes a list of images and produces a number 
; that is the sum of the areas of each image. For area, just use the image's 
; width times its height.
; 



;; ListOfImage -> Number
;; returns sum of area for each image in a list of images or 0 if empty
(check-expect (sum-area empty) 0) ; check empty list
(check-expect (sum-area LOI2) (* 20 30)) ; check 1 element list
(check-expect (sum-area LOI3) (+ (* 50 35) (* 20 30))) ; check 2 element list

; (define (sum-area loi) 0) ;stub

; copy template from ListOfImage
(define (sum-area loi)
  (cond [(empty? loi) 0]
        [else (+ (image-area (first loi))
                 (sum-area (rest loi)))]))

;; Image -> Number
;; returns area of given image where area is the image's width times its height
(check-expect (image-area (rectangle 5 15 "solid" "red")) (* 5 15))
(check-expect (image-area (rectangle 20 85 "solid" "blue")) (* 20 85))

; (define (image-area i) 0) ;stub

(define (image-area i)
  (* (image-width i) (image-height i)))
