#lang racket

(require 2htdp/image)

;; tuition-graph-starter.rkt  (just the problem statements)

; 
; PROBLEM:
; 
; Eva is trying to decide where to go to university. One important factor for her is 
; tuition costs. Eva is a visual thinker, and has taken Systematic Program Design, 
; so she decides to design a program that will help her visualize the costs at 
; different schools. She decides to start simply, knowing she can revise her design
; later.
; 
; The information she has so far is the names of some schools as well as their 
; international student tuition costs. She would like to be able to represent that
; information in bar charts like this one:
; 
; 
;         .
;         
; (A) Design data definitions to represent the information Eva has.
; (B) Design a function that consumes information about schools and their
;     tuition and produces a bar chart.
; (C) Design a function that consumes information about schools and produces
;     the school with the lowest international student tuition.
; 



;; ===============
;; Constants

(define FONT-SIZE 24)
(define FONT-COLOR "black")

(define Y-SCALE 1/200)
(define BAR-WIDTH 30)
(define BAR-COLOR "Light Sky Blue")

;; ================
;; Data definitions


(define-struct school (name tuition))
;; School is (make-school String Integer)
;; interp. name is the school's name, tuition is the international student's tuition in USD

(define Harvard (make-school "Harvard" 54002))
(define Yale (make-school "Yale" 57700))
(define Stanford (make-school "Stanford" 56169))

(define (fn-for-school s)
  (... (school-name s)
       (school-tuition s)))

;; Template rules used:
;; - compound: (make-school String Natural)


;; ListofSchool is one of:
;; - empty
;; (cons School ListOfSchool)
;; interp. a list of schools
(define LOS1 empty) ; an empty list
(define LOS2 (cons Harvard empty)) ; single element list
(define LOS3 (cons Harvard (cons Yale (cons Stanford empty)))) ; 3 element list

(define (fn-for-los los)
  (cond [(empty? los) (...)]
        [else
         (... (fn-for-school (first los))
              (fn-for-los (rest los)))]))

;; Template rules used:
;; - one of: 2 cases
;; - atmoic distinct: empty
;; - compound: (cons School ListOfSchool)
;;
;; - self-reference: (rest los)


;; ================
;; Functions:


;; ListOfSchool -> Image
;; produce bar chart showing names and tuitions of schools
(check-expect (chart empty) (square 0 "solid" "white")) ; check empty image if list is empty
(check-expect (chart (cons Harvard empty))
              (beside/align "bottom" (overlay/align "center" "bottom"
                                                    (rotate 90 (text (school-name Harvard) FONT-SIZE FONT-COLOR))
                                                    (rectangle BAR-WIDTH (* Y-SCALE (school-tuition Harvard)) "outline" "black")
                                                    (rectangle BAR-WIDTH (* Y-SCALE (school-tuition Harvard)) "solid" BAR-COLOR))
                            (square 0 "solid" "white")))
(check-expect (chart (cons Yale (cons Harvard empty)))
              (beside/align "bottom" (overlay/align "center" "bottom"
                                                    (rotate 90 (text (school-name Yale) FONT-SIZE FONT-COLOR))
                                                    (rectangle BAR-WIDTH (* Y-SCALE (school-tuition Yale)) "outline" "black")
                                                    (rectangle BAR-WIDTH (* Y-SCALE (school-tuition Yale)) "solid" BAR-COLOR))
                            (overlay/align "center" "bottom"
                                           (rotate 90 (text (school-name Harvard) FONT-SIZE FONT-COLOR))
                                           (rectangle BAR-WIDTH (* Y-SCALE (school-tuition Harvard)) "outline" "black")
                                           (rectangle BAR-WIDTH (* Y-SCALE (school-tuition Harvard)) "solid" BAR-COLOR))
                            (square 0 "solid" "white")))

; (define (chart los) (square 0 "solid" "white")) ;stub

; copy template from ListOfSchool
(define (chart los)
  (cond [(empty? los) (square 0 "solid" "white")]
        [else
         (beside/align "bottom"
                       (make-bar (first los))
                       (chart (rest los)))]))


;; School -> Image
;; produce bar for a single school in a bar chart

(check-expect (make-bar Harvard)
  (overlay/align "center" "bottom"
                 (rotate 90 (text (school-name Harvard) FONT-SIZE FONT-COLOR))
                 (rectangle BAR-WIDTH (* Y-SCALE (school-tuition Harvard)) "outline" "black")
                 (rectangle BAR-WIDTH (* Y-SCALE (school-tuition Harvard)) "solid" BAR-COLOR)))

; (define (make-bar s) (square 0 "solid" "white")) ;stub

; copy template from School
(define (make-bar s)
  (overlay/align "center" "bottom"
                 (rotate 90 (text (school-name s) FONT-SIZE FONT-COLOR))
                 (rectangle BAR-WIDTH (* Y-SCALE (school-tuition s)) "outline" "black")
                 (rectangle BAR-WIDTH (* Y-SCALE (school-tuition s)) "solid" BAR-COLOR)))
