#lang racket

(require 2htdp/image)
;; SPD2-Design-Quiz-1.rkt


;; ======================================================================
;; Constants
(define COOKIES .)

;; ======================================================================
;; Data Definitions

;; Natural is one of:
;;  - 0
;;  - (add1 Natural)
;; interp. a natural number
(define N0 0)         ;0
(define N1 (add1 N0)) ;1
(define N2 (add1 N1)) ;2

#;
(define (fn-for-natural n)
  (cond [(zero? n) (...)]
        [else
         (... n   ; n is added because it's often useful                   
              (fn-for-natural (sub1 n)))]))

;; Template rules used:
;;  - one-of: two cases
;;  - atomic distinct: 0
;;  - compound: 2 fields
;;  - self-reference: (sub1 n) is Natural




; PROBLEM 1:
; 
; Complete the design of a function called pyramid that takes a natural
; number n and an image, and constructs an n-tall, n-wide pyramid of
; copies of that image.
; 
; For instance, a 3-wide pyramid of cookies would look like this:
; 
; .


;; Natural Image -> Image
;; produce an n-wide pyramid of the given image
(check-expect (pyramid 0 COOKIES) empty-image)
(check-expect (pyramid 1 COOKIES) COOKIES)
(check-expect (pyramid 3 COOKIES)
              (above COOKIES
                     (beside COOKIES COOKIES)
                     (beside COOKIES COOKIES COOKIES)))

; (define (pyramid n i) empty-image) ; stub

(define (pyramid n i)
  (cond [(zero? n) empty-image]
        [else
         (above 
          (pyramid (sub1 n) i)
          (image-row n i))]))


;; Natural Image -> Image
;; produce an n-wide row of images
;; ASSUME Natural > 0
(check-expect (image-row 1 COOKIES) COOKIES) ; check base case
(check-expect (image-row 3 COOKIES) (beside COOKIES COOKIES COOKIES)) ; check 3 wide images

; (define (image-row n i) (text (number->string n) 24 "black")) ;stub

(define (image-row n i)
  (cond [(= n 1) i]
        [else
         (beside i (image-row (sub1 n) i))]))


; Problem 2:
; Consider a test tube filled with solid blobs and bubbles.  Over time the
; solids sink to the bottom of the test tube, and as a consequence the bubbles
; percolate to the top.  Let's capture this idea in BSL.
; 
; Complete the design of a function that takes a list of blobs and sinks each
; solid blob by one. It's okay to assume that a solid blob sinks past any
; neighbor just below it.
; 
; To assist you, we supply the relevant data definitions.


;; Blob is one of:
;; - "solid"
;; - "bubble"
;; interp.  a gelatinous blob, either a solid or a bubble
;; Examples are redundant for enumerations
#;
(define (fn-for-blob b)
  (cond [(string=? b "solid") (...)]
        [(string=? b "bubble") (...)]))

;; Template rules used:
;; - one-of: 2 cases
;; - atomic distinct: "solid"
;; - atomic distinct: "bubble"


;; ListOfBlob is one of:
;; - empty
;; - (cons Blob ListOfBlob)
;; interp. a sequence of blobs in a test tube, listed from top to bottom.
(define LOB0 empty) ; empty test tube
(define LOB2 (cons "solid" (cons "bubble" empty))) ; solid blob above a bubble

#;
(define (fn-for-lob lob)
  (cond [(empty? lob) (...)]
        [else
         (... (fn-for-blob (first lob))
              (fn-for-lob (rest lob)))]))

;; Template rules used
;; - one-of: 2 cases
;; - atomic distinct: empty
;; - compound: 2 fields
;; - reference: (first lob) is Blob
;; - self-reference: (rest lob) is ListOfBlob

;; ListOfBlob -> ListOfBlob
;; produce a list of blobs that sinks the given solid blobs by one

(check-expect (sink empty) empty)
(check-expect (sink (cons "bubble" (cons "solid" (cons "bubble" empty))))
              (cons "bubble" (cons "bubble" (cons "solid" empty))))
(check-expect (sink (cons "solid" (cons "solid" (cons "bubble" empty))))
              (cons "bubble" (cons "solid" (cons "solid" empty))))
(check-expect (sink (cons "solid" (cons "bubble" (cons "bubble" empty))))
              (cons "bubble" (cons "solid" (cons "bubble" empty))))
(check-expect (sink (cons "solid" (cons "bubble" (cons "solid" empty))))
              (cons "bubble" (cons "solid" (cons "solid" empty))))
(check-expect (sink (cons "bubble" (cons "solid" (cons "solid" empty))))
              (cons "bubble" (cons "solid" (cons "solid" empty))))
(check-expect (sink (cons "solid"
                          (cons "solid"
                                (cons "bubble" (cons "bubble" empty)))))
              (cons "bubble" (cons "solid" 
                                   (cons "solid" (cons "bubble" empty)))))
; (define (sink lob) empty) ; stub

(define (sink lob)
  (cond [(empty? lob) empty]
        [else
         (if (solid? (first lob))
             (sink-solid (sink (rest lob)))
             (cons (first lob) (sink (rest lob))))]))


;; Blob -> Bool
;; return true if Blob is "solid"
(check-expect (solid? "solid") true)   ; check true
(check-expect (solid? "bubble") false) ; check false

;(define (solid? b) false) ;stub

(define (solid? b)
  (string=? b "solid"))


;; ListOfBlob -> ListOfBlob
;; return list with "solid" added between first element and rest of element
;; ASSUME list has at least 1 solid
(check-expect (sink-solid empty) (cons "solid" empty)) ; check base case
(check-expect (sink-solid (cons "solid" empty)) (cons "solid" (cons "solid" empty)))   ; check single element with existing solid
(check-expect (sink-solid (cons "bubble" empty)) (cons "bubble" (cons "solid" empty))) ; check single element with existing bubble
(check-expect (sink-solid (cons "solid" (cons "solid" empty)))
              (cons "solid" (cons "solid" (cons "solid" empty)))) ; check with 2 existing solid elements
(check-expect (sink-solid (cons "bubble" (cons "solid" empty)))
              (cons "bubble" (cons "solid" (cons "solid" empty)))) ; check with 1 existing bubble and 1 existing solid
(check-expect (sink-solid (cons "bubble" (cons "bubble" empty)))
              (cons "bubble" (cons "solid" (cons "bubble" empty)))) ; check with 2 existing bubble elements

; (define (sink-solid lob) lob) ;stub

(define (sink-solid lob)
  (cond [(empty? lob) (cons "solid" empty)]
        [else
         (cons (first lob)
               (cons "solid"
                     (rest lob)))]))
