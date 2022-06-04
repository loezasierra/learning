#lang racket

(require 2htdp/image)

;; fold-dir-starter.rkt

; 
; In this exercise you will be need to remember the following DDs 
; for an image organizer.
; 


;; =================
;; Data definitions:

(define-struct dir (name sub-dirs images))
;; Dir is (make-dir String ListOfDir ListOfImage)
;; interp. An directory in the organizer, with a name, a list
;;         of sub-dirs and a list of images.

;; ListOfDir is one of:
;;  - empty
;;  - (cons Dir ListOfDir)
;; interp. A list of directories, this represents the sub-directories of
;;         a directory.

(define (fn-for-dir d)
  (... (dir-name d)     ; string
       (fn-for-lod (dir-sub-dirs d))
       (fn-for-loi (dir-images d))))

(define (fn-for-lod lod)
  (cond [(empty? lod) ...]
        [else
         (... (fn-for-dir (first lod))
              (fn-for-lod (rest lod)))]))

;; ListOfImage is one of:
;;  - empty
;;  - (cons Image ListOfImage)
;; interp. a list of images, this represents the sub-images of a directory.
;; NOTE: Image is a primitive type, but ListOfImage is not.

(define (fn-for-loi loi)
  (cond [(empty? loi) ...]
        [else
         (... (first loi)  ; image
              (fn-for-loi (rest loi)))]))

(define I1 (square 10 "solid" "red"))
(define I2 (square 12 "solid" "green"))
(define I3 (rectangle 13 14 "solid" "blue"))
(define D4 (make-dir "D4" empty (list I1 I2)))
(define D5 (make-dir "D5" empty (list I3)))
(define D6 (make-dir "D6" (list D4 D5) empty))

;; =================
;; Functions:

; 
; PROBLEM A:
; 
; Design an abstract fold function for Dir called fold-dir. 
; 



;; (String Y Z -> X) (X Y -> Y) (Image Z -> Z) Y Z Dir -> X
;; Abstract fold function for Dir
(check-expect (local [(define (list-image name lod loi) (append lod loi))]
                (fold-dir list-image append cons empty empty D6))
              (list I1 I2 I3)) ; list images in dir
(check-expect (local [(define (list-name name lod loi) (cons name lod))]
                (fold-dir list-name append cons empty empty D6))
              (list "D6" "D4" "D5")) ; list dir names

(define (fold-dir c-dir c-lod c-loi b-lod b-loi d)
  (local [(define (fn-for-dir d)
            (c-dir (dir-name d)     ; string
                   (fn-for-lod (dir-sub-dirs d))
                   (fn-for-loi (dir-images d))))

          (define (fn-for-lod lod)
            (cond [(empty? lod) b-lod]
                  [else 
                   (c-lod (fn-for-dir (first lod))
                          (fn-for-lod (rest lod)))]))

          (define (fn-for-loi loi)
            (cond [(empty? loi) b-loi]
                  [else
                   (c-loi (first loi)  ; image
                          (fn-for-loi (rest loi)))]))]
    (fn-for-dir d)))


; 
; PROBLEM B:
; 
; Design a function that consumes a Dir and produces the number of 
; images in the directory and its sub-directories. 
; Use the fold-dir abstract function.
; 



;; Dir -> Int
;; return number of images in Dir
(check-expect (image-count D5) 1)
(check-expect (image-count D6) 3)

; (define (image-count d) 0) ;stub

(define (image-count d)
  (local [(define (count-image i n) (+ 1 n))
          (define (add-counts name lod loi) (+ lod loi))]
    (fold-dir add-counts + count-image 0 0 d)))


; 
; PROBLEM C:
; 
; Design a function that consumes a Dir and a String. The function looks in
; dir and all its sub-directories for a directory with the given name. If it
; finds such a directory it should produce true, if not it should produce false. 
; Use the fold-dir abstract function.
; 



;; String Dir -> Bool
;; return true if String matches directory name in Dir
(check-expect (name-in-dir "D9" D6) false)
(check-expect (name-in-dir "D4" D6) true)
(check-expect (name-in-dir "D6" D6) true)

; (define (name-in-dir s d) false)

(define (name-in-dir s d)
  (local [(define (name-matches? name lod loi) 
            (or (string=? s name) lod))
          (define (orr b1 b2) (or b1 b2))]
    (fold-dir name-matches? orr cons false empty d)))


; 
; PROBLEM D:
; 
; Is fold-dir really the best way to code the function from part C? Why or 
; why not?
; 


;; fold-dir forces us to search the whole tree.
;; the best way to form the function would be to use an if to combine the else statement in fn-for-lod
;; that way we can return true early if we find a match and not have to search the whole directory
