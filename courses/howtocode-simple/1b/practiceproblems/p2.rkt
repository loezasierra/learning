#lang racket

; PROBLEM:

; DESIGN function that consumes a string and determines whether its length is
; less than 5.  Follow the HtDF recipe and leave behind commented out versions 
; of the stub and template.

; string -> bool
; return true if string length < 5
; (define (less_than_five s) false) ;stub
; (define (less_than_five s )  ;template
;   (...s))

(define (less_than_five s)
  (< (string-length s) 5))

(check-expect (less_than_five "hola") true) ; check strlen 4
(check-expect (less_than_five "flower") false) ; check strlen 6
(check-expect (less_than_five "flora") false) ; check strlen 5