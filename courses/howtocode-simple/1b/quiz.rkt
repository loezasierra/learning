#lang racket

(require 2htdp/image)

; PROBLEM:
; 
; Design a function that consumes two images and produces true if the first is larger than the second.


; image image -> bool
; returns true if first image has a larger area (length * width) than second
;(define (imgislarger image1 image2) false) ;stub

;(define (imgislarger image1 image2)        ;template
;  (...image1, image2))

(check-expect (imgislarger (rectangle 10 10 "solid" "red")
                           (rectangle 1 1 "solid" "red"))
              true); check where 1st image is larger
(check-expect (imgislarger (rectangle 1 1 "solid" "red")
                           (rectangle 10 10 "solid" "red"))
              false); check where 2nd image is larger
(check-expect (imgislarger (rectangle 5 5 "solid" "red")
                           (rectangle 5 5 "solid" "blue"))
              false); check where images are the same area

(define (imgislarger image1 image2)
  (>
   (+ (image-height image1) (image-width image1))
   (+ (image-height image2) (image-width image2))))
