;; spinning-bears-starter.rkt

(require 2htdp/image)
(require 2htdp/universe)

; PROBLEM:
; 
; In this problem you will design another world program. In this program the changing 
; information will be more complex - your type definitions will involve arbitrary 
; sized data as well as the reference rule and compound data. But by doing your 
; design in two phases you will be able to manage this complexity. As a whole, this problem 
; will represent an excellent summary of the material covered so far in the course, and world 
; programs in particular.
; 
; This world is about spinning bears. The world will start with an empty screen. Clicking
; anywhere on the screen will cause a bear to appear at that spot. The bear starts out upright,
; but then rotates counterclockwise at a constant speed. Each time the mouse is clicked on the 
; screen, a new upright bear appears and starts spinning.
; 
; So each bear has its own x and y position, as well as its angle of rotation. And there are an
; arbitrary amount of bears.
; 
; To start, design a world that has only one spinning bear. Initially, the world will start
; with one bear spinning in the center at the screen. Clicking the mouse at a spot on the
; world will replace the old bear with a new bear at the new spot. You can do this part 
; with only material up through compound. 
; 
; Once this is working you should expand the program to include an arbitrary number of bears.
; 
; Here is an image of a bear for you to use: .


;; Spinning bears
;; ======================
;; Constants

(define BEAR-IMG .)

(define ROTATEBY -5)

(define BYTE 8)

(define WIDTH (* BYTE BYTE 10))
(define HEIGHT WIDTH)

(define MTS (empty-scene WIDTH HEIGHT))


;; =======================
;; Data definitions


(define-struct bear (x y rotation))
;; Bear is (make-bear Integer[0, WIDTH] Integer[0, HEIGHT] Integer[0, 359])
;; interp. a bear where x & y are its pixel coordinates on screen
;;                where rotation is its angle of rotation in degrees
(define B1 (make-bear 0 0 0)) ; a bear at the top left of the screen with no rotation
(define B2 (make-bear (/ WIDTH 2) (/ HEIGHT 2) 180)) ; a bear in the middle of the screen, upside down

(define (fn-for-bear b)
  (... (bear-x b)          ; Integer[0, WIDTH]
       (bear-y b)          ; Integer[0, HEIGHT]
       (bear-rotation b))) ; Integer[0, 359]

;; Template rules used:
;; - compound: (make-bear Integer[0, WIDTH] Integer[0, HEIGHT] Integer[0, 359])


;; ListOfBear is one of:
;; - empty
;; - (cons Bear ListOfBear)
;; interp. a list of Bears
(define LOB1 empty) ; an empty list
(define LOB2 (cons (make-bear 0 0 0) empty)) ; single element list
(define LOB3 (cons (make-bear 0 0 0)
                   (cons (make-bear 64 64 190)
                         (cons (make-bear 128 128 32) empty)))) ; 3 element list

(define (fn-for-lob lob)
  (cond [(empty? lob) (...)]
        [else (... (fn-for-bear (first lob))
                   (fn-for-lob (rest lob)))]))

;; Template rules used:
;; - one of: 2 cases
;; - atomic distinct: empty
;; - compound: (cons Bear ListOfBear)
;; - reference: (first los) is Bear
;; - self-reference: (rest los) is ListOfBear


;; =======================
;; Functions


;; start the world with (main empty)

(define (main lob)
  (big-bang lob             ; ListOfBear
    (on-tick advance-bears) ; ListOfBear -> ListOfBear
    (to-draw render)        ; ListOfBear -> Image
    (on-mouse new-bear)))   ; ListOfBear Int Int MouseEvent -> ListOfBear


;; ListOfBear -> ListOfBear
;; return ListOfBear where each Bear is rotated by ROTATEBY, or empty if list is empty
(check-expect (advance-bears empty) empty) ; check base case
(check-expect (advance-bears (cons (make-bear 64 64 0) empty))
              (cons (make-bear 64 64 (modulo (+ ROTATEBY 0) 360)) empty)) ; check basic rotation of single element
(check-expect (advance-bears (cons (make-bear 64 64 0)
                                   (cons (make-bear 16 16 359) empty)))
              (cons (make-bear 64 64 (modulo (+ ROTATEBY 0) 360))
                    (cons (make-bear 16 16 (modulo (+ ROTATEBY 359) 360)) empty))) ; check 2 element rotation, one past 360

; (define (rotate-bear lob) lob) ;stub

; copy template from ListOfBear
(define (advance-bears lob)
  (cond [(empty? lob) empty]
        [else (cons (rotate-bear (first lob))
                    (advance-bears (rest lob)))]))


;; Bear -> Bear
;; return Bear rotated by ROTATEBY amount without going past 359
(check-expect (rotate-bear (make-bear 64 64 0))
              (make-bear 64 64 (modulo (+ ROTATEBY 0) 360))) ; check basic rotation
(check-expect (rotate-bear (make-bear 16 16 359))
              (make-bear 16 16 (modulo (+ ROTATEBY 359) 360))) ; check rotation past 360

; (define (rotate-bear b) b) ;stub

; copy template from Bear
(define (rotate-bear b) 
  (make-bear (bear-x b) 
             (bear-y b) 
             (modulo (+ (bear-rotation b) ROTATEBY) 360))) ; Integer[0, 359]


;; ListOfBear -> Image
;; render a list of bears on screen
(check-expect (render empty) (place-image empty-image 0 0 MTS)) ; check base case
(check-expect (render (cons (make-bear 0 0 0) empty))
              (place-image (rotate 0 BEAR-IMG) 0 0
                           (place-image empty-image 0 0 MTS))) ; check single bear render
(check-expect (render (cons (make-bear 0 0 0)
                            (cons (make-bear 64 64 200) empty)))
              (place-image (rotate 0 BEAR-IMG) 0 0
                           (place-image (rotate 200 BEAR-IMG) 64 64
                                        (place-image empty-image 0 0 MTS)))) ; check two bear render

; (define (render lob) empty-image) ;stub

; copy template from ListOfBear
(define (render lob)
  (cond [(empty? lob) (place-image empty-image 0 0 MTS)]
        [else (place-image (bear-image (first lob))
                           (bear-x (first lob))
                           (bear-y (first lob))
                           (render (rest lob)))]))


;; Bear -> Image
;; return appropriately rotated bear image
(check-expect (bear-image (make-bear 0 0 0)) (rotate 0 BEAR-IMG))
(check-expect (bear-image (make-bear 64 64 200)) (rotate 200 BEAR-IMG))

; (define (bear-image b) b) ;stub

; copy template from Bear
(define (bear-image b)
  (rotate (bear-rotation b) BEAR-IMG))


;; ListOfBear Int Int MouseEvent -> ListOfBear
;; add a new bear to ListOfBear with 0 rotation at mouse pixel coordinates clicked by user
(check-expect (new-bear empty 24 24 "button-down")
              (cons (make-bear 24 24 0) empty)) ; check bear addition with existing empty list
(check-expect (new-bear (cons (make-bear 24 24 0) empty) 68 68 "button-down")
              (cons (make-bear 68 68 0) (cons (make-bear 24 24 0) empty))) ; check bear addition with existing bear in list
(check-expect (new-bear empty 68 68 "button-up") empty) ; check for incorrect button input

; (define (new-bear lob x y me) lob) ;stub

#;
(define (handle-mouse ws x y me)
  (cond [(mouse=? me "button-down") (... ws x y)]
        [else
         (... ws x y)]))

(define (new-bear lob x y me)
  (cond [(mouse=? me "button-down") (cons (make-bear x y 0) lob)]
        [else lob]))
