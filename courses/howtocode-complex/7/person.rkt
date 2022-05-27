#lang racket

(define-struct person (name age children))
;; Person is (make-person String Natural ListOfPerson)
;; interp. a person with first name, age and a list of their children

;; ListOfPerson is one of:
;; - empty
;; - (cons Person ListOfPerson)
;; interp. a list of persons

(define P1 (make-person "N1" 5 empty))
(define P2 (make-person "N2" 25 (list P1)))
(define P3 (make-person "N3" 15 empty))
(define P4 (make-person "N4" 45 (list P2 P3)))

(define (fon-for-person p)
  (... (person-name p) ; String
       (person-age p)  ; Integer
       (fn-for-lop (person-children p))))

(define (fn-for-lop lop)
  (cond [(empty? lop) (...)]
        [else
         (... (fn-for-person (first lop))
              (fn-for-lop (rest lop))
              )]))



;; Person -> ListOfString
;; ListOfPerson -> ListOfString
;; return a list of the names of the persons under 20
(check-expect (names-under-20 P1) (list "N1"))
(check-expect (names-under-20-of-list empty) empty)
(check-expect (names-under-20 P2) (list "N1"))
(check-expect (names-under-20 P4) (list "N1" "N3"))
(check-expect (names-under-20 (make-person "Oldie" 74 empty)) empty)

; (define (names-under-20 p) (list "")) ;stub
; (define (names-under-20-of-list lop) (list "")) ;stub

(define (names-under-20 p)
  (if (< (person-age p) 20)
      (cons (person-name p) (names-under-20-of-list (person-children p)))
      (names-under-20-of-list (person-children p))))

(define (names-under-20-of-list lop)
  (cond [(empty? lop) empty]
        [else
         (append (names-under-20 (first lop))
                 (names-under-20-of-list (rest lop))
                 )]))
