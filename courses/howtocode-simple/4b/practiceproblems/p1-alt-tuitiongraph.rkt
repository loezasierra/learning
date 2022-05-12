#lang racket

(require 2htdp/image)

;; alternative-tuition-graph-starter.rkt

; 
; Consider the following alternative type comment for Eva's school tuition 
; information program. Note that this is just a single type, with no reference, 
; but it captures all the same information as the two types solution in the 
; videos.
; 
; (define-struct school (name tuition next))
; ;; School is one of:
; ;;  - false
; ;;  - (make-school String Natural School)
; ;; interp. an arbitrary number of schools, where for each school we have its
; ;;         name and its tuition in USD
; 
; (A) Confirm for yourself that this is a well-formed self-referential data 
;     definition.
; 
; (B) Complete the data definition making sure to define all the same examples as 
;     for ListOfSchool in the videos.
; 
; (C) Design the chart function that consumes School. Save yourself time by 
;     simply copying the tests over from the original version of chart.
; 
; (D) Compare the two versions of chart. Which do you prefer? Why?
; 


; Answers
; (A) Is a well formed self-referential data definition.
;     Contains base case and a case with self reference.
; 
; (D) I prefer the version with two different data definitions.
;     I think it is better when functions and data are separated like that.
;     It offers more safety and less confusion.


;; =====================================================
;; Constants


(define FONT-SIZE 24)
(define FONT-COLOR "black")

(define Y-SCALE 1/200)
(define BAR-WIDTH 30)
(define BAR-COLOR "Light Sky Blue")


;; =====================================================
;; Data definitions


(define-struct school (name tuition next))
;; School is one of:
;;  - false
;;  - (make-school String Natural School)
;; interp. an arbitrary number of schools, where for each school we have its
;;         name and its tuition in USD

(define S1 false) ; no schools
(define S2 (make-school "Harvard" 54002 false)) ; single school
(define S3 (make-school "Harvard" 54002
                        (make-school "Yale" 57700 false))) ; 2 schools

(define (fn-for-school s)
  (cond [(false? s) (...)]
        [else (... (school-name s)
                   (school-tuition s)
                   (fn-for-school (school-next s)))]))

;; Template rules used:
;; - one of: 2 cases
;; - atomc distinct: false
;; - compound: (make-school String Natural School)
;; - self reference: (school-next s) is School


;; ========================================================
;; Functions:


;; ListOfSchool -> Image
;; produce bar chart showing names and tuitions of schools
(check-expect (chart false) (square 0 "solid" "white")) ; check empty image if list is empty
(check-expect (chart S2)
              (beside/align "bottom" (overlay/align "center" "bottom"
                                                    (rotate 90 (text (school-name S2) FONT-SIZE FONT-COLOR))
                                                    (rectangle BAR-WIDTH (* Y-SCALE (school-tuition S2)) "outline" "black")
                                                    (rectangle BAR-WIDTH (* Y-SCALE (school-tuition S2)) "solid" BAR-COLOR))
                            (square 0 "solid" "white")))
(check-expect (chart S3)
              (beside/align "bottom" (overlay/align "center" "bottom"
                                                    (rotate 90 (text (school-name S3) FONT-SIZE FONT-COLOR))
                                                    (rectangle BAR-WIDTH (* Y-SCALE (school-tuition S3)) "outline" "black")
                                                    (rectangle BAR-WIDTH (* Y-SCALE (school-tuition S3)) "solid" BAR-COLOR))
                            (overlay/align "center" "bottom"
                                           (rotate 90 (text (school-name (school-next S3)) FONT-SIZE FONT-COLOR))
                                           (rectangle BAR-WIDTH (* Y-SCALE (school-tuition (school-next S3))) "outline" "black")
                                           (rectangle BAR-WIDTH (* Y-SCALE (school-tuition (school-next S3))) "solid" BAR-COLOR))
                            (square 0 "solid" "white")))

; (define (chart los) (square 0 "solid" "white")) ;stub

; copy template from School

(define (chart s)
  (cond [(false? s) (square 0 "solid" "white")]
        [else (beside/align "bottom"
                            (overlay/align "center" "bottom"
                                           (rotate 90 (text (school-name s) FONT-SIZE FONT-COLOR))
                                           (rectangle BAR-WIDTH (* Y-SCALE (school-tuition s)) "outline" "black")
                                           (rectangle BAR-WIDTH (* Y-SCALE (school-tuition s)) "solid" BAR-COLOR))
                            (chart (school-next s)))]))
