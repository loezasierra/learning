#lang racket

; PROBLEM:
; 
; Use the How to Design Functions (HtDF) recipe to design a function that consumes an image, 
; and appears to put a box around it. Note that you can do this by creating an "outline" 
; rectangle that is bigger than the image, and then using overlay to put it on top of the image. 
; For example:
; 
; (boxify (ellipse 60 30 "solid" "red")) should produce .
; 
; Remember, when we say DESIGN, we mean follow the recipe.
; 
; Leave behind commented out versions of the stub and template.
; 


; image -> image
; return given image with outline rectangle around it
; (define (boxify i) (circle 5 "solid" "red"))  ;stub
;(define (boxify i)  ;template
;  (... i))

(define (boxify i)
  (overlay
   (rectangle (+ (image-width i) 1) (+ (image-height i) 1) "outline" "black")
   i))

(check-expect (boxify (ellipse 60 30 "solid" "red")) .)