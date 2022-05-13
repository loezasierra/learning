#lang racket

(require 2htdp/image)
(require 2htdp/universe)

;; making-rain-filtered-starter.rkt1

; 
; PROBLEM:
; 
; Design a simple interactive animation of rain falling down a screen. Wherever we click,
; a rain drop should be created and as time goes by it should fall. Over time the drops
; will reach the bottom of the screen and "fall off". You should filter these excess
; drops out of the world state - otherwise your program is continuing to tick and
; and draw them long after they are invisible.
; 
; In your design pay particular attention to the helper rules. In our solution we use
; these rules to split out helpers:
;   - function composition
;   - reference
;   - knowledge domain shift
;   
;   
; NOTE: This is a fairly long problem.  While you should be getting more comfortable with 
; world problems there is still a fair amount of work to do here. Our solution has 9
; functions including main. If you find it is taking you too long then jump ahead to the
; next homework problem and finish this later.
; 
; 


;; Make it rain where we want it to.

;; =================
;; Constants:

(define WIDTH  300)
(define HEIGHT 300)

(define SPEED 5)

(define DROP (ellipse 4 8 "solid" "blue"))

(define MTS (rectangle WIDTH HEIGHT "solid" "light blue"))


;; =================
;; Data definitions:

(define-struct drop (x y))
;; Drop is (make-drop Integer Integer)
;; interp. A raindrop on the screen, with x and y coordinates.

(define D1 (make-drop  0 0))
(define D2 (make-drop  8 8))

#;
(define (fn-for-drop d)
  (... (drop-x d) 
       (drop-y d)))

;; Template Rules used:
;; - compound: 2 fields


;; ListOfDrop is one of:
;;  - empty
;;  - (cons Drop ListOfDrop)
;; interp. a list of drops

(define LOD1 empty)
(define LOD2 (cons (make-drop 10 20) (cons (make-drop 3 6) empty)))

#;
(define (fn-for-lod lod)
  (cond [(empty? lod) (...)]
        [else
         (... (fn-for-drop (first lod))
              (fn-for-lod (rest lod)))]))

;; Template Rules used:
;; - one-of: 2 cases
;; - atomic distinct: empty
;; - compound: (cons Drop ListOfDrop)
;; - reference: (first lod) is Drop
;; - self reference: (rest lod) is ListOfDrop

;; =================
;; Functions:

;; ListOfDrop -> ListOfDrop
;; start rain program by evaluating (main empty)
(define (main lod)
  (big-bang lod
    (on-mouse handle-mouse)   ; ListOfDrop Integer Integer MouseEvent -> ListOfDrop
    (on-tick  next-drops)     ; ListOfDrop -> ListOfDrop
    (to-draw  render-drops))) ; ListOfDrop -> Image


;; ListOfDrop Integer Integer MouseEvent -> ListOfDrop
;; if mevt is "button-down" add a new drop at that position
(check-expect (handle-mouse empty 0 0 "button-up") empty) ; check empty list with incorrect mouse event
(check-expect (handle-mouse empty 0 0 "button-down") (cons D1 empty)) ; check new drop in empty list
(check-expect (handle-mouse (cons D1 empty) 8 8 "button-up") (cons D1 empty)) ; check existing list with incorrect mouse event
(check-expect (handle-mouse (cons D1 empty) 8 8 "button-down") (cons D2 (cons D1 empty))) ; check new drop with existing drop in list
;; !!!
; (define (handle-mouse lod x y mevt) empty) ; stub

(define (handle-mouse lod x y mevt)
  (cond [(mouse=? mevt "button-down") (cons (make-drop x y) lod)]
        [else lod]))


;; ListOfDrop -> ListOfDrop
;; produce filtered and ticked list of drops
(check-expect (next-drops empty) empty) ; check base case
(check-expect (next-drops (cons (make-drop 0 0) empty)) (cons (make-drop 0 (+ 0 SPEED)) empty)) ; check single drop
(check-expect (next-drops (cons (make-drop 0 0) (cons (make-drop 64 64) empty)))
              (cons (make-drop 0 (+ 0 SPEED)) (cons (make-drop 64 (+ 64 SPEED)) empty))) ; check 2 drops
(check-expect (next-drops (cons (make-drop 16 HEIGHT) empty))
              empty) ; check for single filtered drop
(check-expect (next-drops (cons (make-drop 0 0) (cons (make-drop 16 HEIGHT) empty)))
              (cons (make-drop 0 (+ 0 SPEED)) empty)) ; check 2 drops where one should be filtered
(check-expect (next-drops (cons (make-drop 16 HEIGHT) (cons (make-drop 0 0) (cons (make-drop 64 HEIGHT) empty))))
              (cons (make-drop 0 (+ 0 SPEED)) empty)) ; check 3 drops where 2 should be filtered
(check-expect (next-drops (cons (make-drop 16 16) (cons (make-drop 64 HEIGHT) (cons (make-drop 0 0) empty))))
              (cons (make-drop 16 (+ 16 SPEED)) (cons (make-drop 0 (+ 0 SPEED)) empty))) ; check 3 drops where middle drop should be filtered

; (define (next-drops lod)empty) ; stub

(define (next-drops lod)
  (filter-drops (advance-drops lod)))


;; ListOfDrop -> ListOfDrop
;; return drop list where any drops off-screen are filtered out
(check-expect (filter-drops empty) empty) ; check base case
(check-expect (filter-drops (cons (make-drop 0 0) empty)) (cons (make-drop 0 0) empty)) ; check single no-filter drop
(check-expect (filter-drops (cons (make-drop 16 HEIGHT) empty))
              empty) ; check for single filtered drop
(check-expect (filter-drops (cons (make-drop 16 16) (cons (make-drop 64 HEIGHT) (cons (make-drop 0 0) empty))))
              (cons (make-drop 16 16) (cons (make-drop 0 0) empty))) ; check 3 drops where middle drop should be filtered

; (define (filter-drops lod) lod) ;stub

(define (filter-drops lod)
  (cond [(empty? lod) empty]
        [else
         (if (filter-drop? (first lod))
             (filter-drops (rest lod))
             (cons (first lod) (filter-drops (rest lod))))]))


;; ListOfDrop -> ListOfDrop
;; return drop list where each drop advances down the screen by SPEED
(check-expect (advance-drops empty) empty) ; check base case
(check-expect (next-drops (cons (make-drop 0 0) empty)) (cons (make-drop 0 (+ 0 SPEED)) empty)) ; check single drop
(check-expect (next-drops (cons (make-drop 0 0) (cons (make-drop 64 64) empty)))
              (cons (make-drop 0 (+ 0 SPEED)) (cons (make-drop 64 (+ 64 SPEED)) empty))) ; check 2 drops

; (define (advance-drops lod) lod) ;stub

(define (advance-drops lod)
  (cond [(empty? lod) empty]
        [else
         (cons (next-drop (first lod))
               (advance-drops (rest lod)))]))


;; Drop -> Bool
;; return true if drop has reached bottom of or gone past screen and should be filtered out
(check-expect (filter-drop? (make-drop 0 0)) false)
(check-expect (filter-drop? (make-drop WIDTH 0)) false)
(check-expect (filter-drop? (make-drop 0 HEIGHT)) true)
(check-expect (filter-drop? (make-drop 0 (+ HEIGHT 20))) true)

; (define (filter-drop? D1) true) ;stub

(define (filter-drop? d)
  (>= (drop-y d) HEIGHT))


;; Drop -> Drop
;; return drop where drop's y axis is drop's y + SPEED
(check-expect (next-drop (make-drop 0 0)) (make-drop 0 (+ 0 SPEED)))
(check-expect (next-drop (make-drop 64 64)) (make-drop 64 (+ 64 SPEED)))

; (define (next-drop D1) D1) ;stub

(define (next-drop d)
  (make-drop (drop-x d) 
             (+ (drop-y d) SPEED)))


;; ListOfDrop -> Image
;; Render the drops onto MTS
(check-expect (render-drops empty) MTS) ; check base case
(check-expect (render-drops (cons (make-drop 16 16) empty))
              (place-image DROP 16 16 MTS)) ; check single drop
(check-expect (render-drops (cons (make-drop 16 16) (cons (make-drop 64 64) empty)))
              (place-image DROP 16 16 (place-image DROP 64 64 MTS))) ; check two drops

; (define (render-drops lod) MTS) ; stub

(define (render-drops lod)
  (cond [(empty? lod) MTS]
        [else
         (place-drop-on (first lod) (render-drops (rest lod)))]))


;; Drop Image -> Image
;; render drop on given image
(check-expect (place-drop-on (make-drop 0 0) MTS) (place-image DROP 0 0 MTS)) ; check basic drop on MTS
(check-expect (place-drop-on (make-drop 8 8) (place-image DROP 0 0 MTS))
              (place-image DROP 8 8 (place-image DROP 0 0 MTS))) ; check drop render on image with existing drop

; (define (place-drop-on d img) MTS) ;stub

(define (place-drop-on d img)
  (place-image DROP (drop-x d) (drop-y d) img))
