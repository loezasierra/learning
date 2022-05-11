;; Produces a list of student cards

;; ================================
;; Data definitions

(define-struct student (name id))
;; Student is (make-student String Integer)
;; interp. as a student with name and student id
(define S1 (make-student "Eva" 3124))
(define S2 (make-student "John" 7839))

#;
(define (fn-for-student s)
  (... (student-name s)
       (student-id s)))


;; ListOfStudent is one of:
;; - empty
;; - (cons Student ListOfStudent)
;; interp. a list of students
(define LOS1 empty) ; an empty list
(define LOS2 (cons S1 empty)) ; a list with 1 element
(define LOS3 (cons S1 (cons S2 empty))) ; a list with 2 elements

#;
(define (fn-for-los los)
  (cond [(empty? los) (...)]
        [else
         (... (fn-for-student (first los))
              (fn-for-los (rest los)))]))


;; =============================
;; Functions

;; ListOfStudent -> List
;; returns a list of student cards where each student card 
;; contains the name and ID of student
(check-expect (card-list empty) empty) ; check base case
(check-expect (card-list (cons S1 empty))
              (cons (string-append (student-name S1) " " (number->string (student-id S1))) empty)) ; check single element list
(check-expect (card-list (cons S2 (cons S1 empty)))
              (cons (string-append (student-name S2) " " (number->string (student-id S2)))
                    (cons (string-append (student-name S1) " " (number->string (student-id S1))) empty))) ; check 2 element list

; (define (card-list los) los) ;stub

; copy template from ListOfStudent
(define (card-list los)
  (cond [(empty? los) empty]
        [else
         (cons (student-card (first los))
               (card-list (rest los)))]))


;; Student -> String
;; returns a student in student card form where (make-student "Eva" 3124) -> "Eva 3124"
(check-expect (student-card S1) (string-append (student-name S1) " " (number->string (student-id S1))))
(check-expect (student-card S2) (string-append (student-name S2) " " (number->string (student-id S2))))

; (define (student-card S1) "") ;stub

; copy template from Student
(define (student-card s)
  (string-append (student-name s)
                 " "
                 (number->string (student-id s))))
