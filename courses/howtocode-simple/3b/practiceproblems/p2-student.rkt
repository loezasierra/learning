#lang racket

;; student-starter.rkt

;; =================
;; Data definitions:

; 
; PROBLEM A:
; 
; Design a data definition to help a teacher organize their next field trip. 
; On the trip, lunch must be provided for all students. For each student, track 
; their name, their grade (from 1 to 12), and whether or not they have allergies.
; 


(define-struct student (name grade allergy))
;; Student is (make-student String Integer[1, 12] Bool)
;; interp. as (make-student name grade allergy) where
;;         name is student's name
;;         grade is student's grade from 1 to 12
;;         allergy is true if student has allergies
(define S1 (make-student "John" 5 false))   ; John is in 5th grade and has no allergies
(define S2 (make-student "Rachel" 12 true)) ; Rachel is in 12th grade and has allergies
(define S3 (make-student "Claire" 3 true))  ; Claire is in 3rd grade and has allergies
#;
(define (fn-for-student s)
  (... (student-name s)       ; string
       (student-grade s)      ; integer[1, 12]
       (student-allergy s)))  ; bool

;; Function templates used:
;; - compound: 3 fields

;; =================
;; Functions:

; 
; PROBLEM B:
; 
; To plan for the field trip, if students are in grade 6 or below, the teacher 
; is responsible for keeping track of their allergies. If a student has allergies, 
; and is in a qualifying grade, their name should be added to a special list. 
; Design a function to produce true if a student name should be added to this list.
; 


;; Student -> Bool
;; return true if student is in grade 6 or below and has allergies
(check-expect (addtolist S1) false)
(check-expect (addtolist S2) false)
(check-expect (addtolist S3) true)

; (define (addtolist s) false) ;stub

; copy template from Student
(define (addtolist s)
  (and (<= (student-grade s) 6)
       (student-allergy s)))
