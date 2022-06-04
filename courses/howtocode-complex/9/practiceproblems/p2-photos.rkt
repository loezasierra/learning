#lang racket

;; photos-starter.rkt

;; =================
;; Data definitions:

(define-struct photo (location album favourite))
;; Photo is (make-photo String String Boolean)
;; interp. a photo having a location, belonging to an album and having a
;; favourite status (true if photo is a favourite, false otherwise)
(define PHT1 (make-photo "photos/2012/june" "Victoria" true))
(define PHT2 (make-photo "photos/2013/birthday" "Birthday" true))
(define PHT3 (make-photo "photos/2012/august" "Seattle" true))
(define PHT4 (make-photo "photos/2014/january" "Blue" true))
(define PHT5 (make-photo "photos/2014/july" "Blue" true))
(define PHT6 (make-photo "photos/2014/november" "Blue" false))

;; =================
;; Functions:

; 
; PROBLEM:
; 
; Design a function called to-frame that consumes an album name and a list of photos 
; and produces a list of only those photos that are favourites and that belong to 
; the given album. You must use built-in abstract functions wherever possible. 
; 



;; String (listof Photo) -> (listof Photo)
;; given a list of Photos
;; return a list where p in Photos album matches String and favourite is true
(define LOP1 (list PHT1 PHT2 PHT3 PHT4 PHT5))

(check-expect (to-frame "Colors" empty) empty)
(check-expect (to-frame "Colors" LOP1) empty)
(check-expect (to-frame "Seattle" LOP1) (list PHT3))
(check-expect (to-frame "Blue" LOP1) (list PHT4 PHT5))

; (define (to-frame album lop) lop) ;stub

(define (to-frame album lop)
  (local [(define (frame? p)
            (and (string=? album (photo-album p))
                 (photo-favourite p)))]
    (filter frame? lop)))
