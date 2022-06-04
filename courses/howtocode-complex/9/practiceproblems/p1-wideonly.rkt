#lang racket

(require 2htdp/image)

;; wide-only-starter.rkt

; 
; PROBLEM:
; 
; Use the built in version of filter to design a function called wide-only 
; that consumes a list of images and produces a list containing only those 
; images that are wider than they are tall.
; 



;; (listof Image) -> (listof Image)
;; given a list of images,
;; return a list of images containing only those images where width > height
(define I1 (rectangle 60 10 "solid" "red"))
(define I2 (rectangle 20 70 "solid" "orange"))
(define I3 (rectangle 50 50 "solid" "yellow"))
(define I4 (rectangle 20 30 "solid" "green"))
(define I5 (rectangle 50 40 "solid" "blue"))

(define LOI1 (list I1 I2 I3 I4 I5))

(check-expect (wide-only empty) empty)
(check-expect (wide-only LOI1) (list I1 I5))

; (define (wide-only loi) empty) ;stub

(define (wide-only loi)
  (local [(define (wide? i) (> (image-width i) (image-height i)))]
    (filter wide? loi)))
