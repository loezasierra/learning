
;; same-house-as-parent-v1.rkt

; 
; PROBLEM:
; 
; In the Harry Potter movies, it is very important which of the four houses a
; wizard is placed in when they are at Hogwarts. This is so important that in 
; most families multiple generations of wizards are all placed in the same family. 
; 
; Design a representation of wizard family trees that includes, for each wizard,
; their name, the house they were placed in at Hogwarts and their children. We
; encourage you to get real information for wizard families from: 
;    http://harrypotter.wikia.com/wiki/Main_Page
; 
; The reason we do this is that designing programs often involves collection
; domain information from a variety of sources and representing it in the program
; as constants of some form. So this problem illustrates a fairly common scenario.
; 
; That said, for reasons having to do entirely with making things fit on the
; screen in later videos, we are going to use the following wizard family tree,
; in which wizards and houses both have 1 letter names. (Sigh)
; 
; 


(define-struct wizard (name house children))
;; Wizard is (make-wizard String, String, (listof Wizard))
;; interp. A wizard family tree

(define Wa (make-wizard "A" "S" empty))
(define Wb (make-wizard "B" "G" empty))
(define Wc (make-wizard "C" "R" empty))
(define Wd (make-wizard "D" "H" empty))
(define We (make-wizard "E" "R" empty))
(define Wf (make-wizard "F" "R" (list Wb)))
(define Wg (make-wizard "G" "S" (list Wa)))
(define Wh (make-wizard "H" "S" (list Wc Wd)))
(define Wi (make-wizard "I" "H" empty))
(define Wj (make-wizard "J" "R" (list We Wf Wg)))
(define Wk (make-wizard "K" "G" (list Wh Wi Wj)))

(define (fn-for-wizard w)
  (local [(define (fn-for-wizard w)
            (... (wizard-name w)
                 (wizard-house w)
                 (fn-for-low (wizard-children w))))

          (define (fn-for-low low)
            (cond [(empty? low) (...)]
                  [else
                   (... (fn-for-wizard (first low))
                        (fn-for-low (rest low)))]))]
    (fn-for-wizard w)))

; 
; PROBLEM:
; 
; Design a function that consumes a wizard and produces the names of every 
; wizard in the tree that was placed in the same house as their immediate
; parent. 
; 



;; Wizard -> (listof String)
;; return a list of names of every wizard that was placed in the same house as their immediate parent
(check-expect (same-house Wh) empty)
(check-expect (same-house Wg) (list "A"))
(check-expect (same-house Wk) (list "E" "F" "A"))

(define (same-house w)
  ;; parent-house is String; name of the immediate parent's house ("" for root)

  ;; (same-house Wj) ; outer call
  ;; ((fn-for-wizard Wj "") empty)
  ;; ((fn-for-wizard We "R") (list "E"))
  ;; ((fn-for-wizard Wf "R") (list "E" "F"))
  ;; ((fn-for-wizard Wb "R") (list "E" "F"))
  ;; ((fn-for-wizard Wg "R") (list "E" "F"))
  ;; ((fn-for-wizard Wa "S") (list "E" "F" "A"))
  (local [(define (fn-for-wizard w parent-house)
            (if (string=? parent-house (wizard-house w))
                (cons (wizard-name w)
                      (fn-for-low (wizard-children w)
                                  (wizard-house w)))
                (fn-for-low (wizard-children w)
                            (wizard-house w))))

          (define (fn-for-low low parent-house)
            (cond [(empty? low) empty]
                  [else
                   (append (fn-for-wizard (first low) parent-house)
                           (fn-for-low (rest low) parent-house))]))]
    (fn-for-wizard w "")))


; 
; PROBLEM:
; 
; Design a function that consumes a wizard and produces the number of wizards 
; in that tree (including the root). Your function should be tail recursive.
; 



;; Wizard -> Integer
;; returns the number of wizards in the tree (including the root)
(check-expect (count Wa) 1)
(check-expect (count Wk) 11)

(define (count w)
  ;; rsf is Integer; number of wizards seen so far
  ;todo is (listof Wizard) ; wizards still needed to vist with fn-for-wizard
  ;; (count Wk)
  ;; (fn-for-wizard Wk 0)
  ;; (fn-for-wizard Wh 1)
  ;; (fn-for-wizard Wc 2)
  (local [(define (fn-for-wizard w todo rsf) 
            (fn-for-low (append (wizard-children w) todo)
                        (add1 rsf)))

          (define (fn-for-low low rsf)
            (cond [(empty? low) rsf]
                  [else
                   (fn-for-wizard (first low) (rest low) rsf)]))]
    (fn-for-wizard w empty 0)))


; 
; PROBLEM:
; 
; Design a new function definition for same-house-as-parent that is tail 
; recursive. You will need a worklist accumulator.
; 
; 



;; Wizard -> (listof String)
;; return a list of names of every wizard that was placed in the same house as their immediate parent
(check-expect (same-house-as-parent Wh) empty)
(check-expect (same-house-as-parent Wg) (list "A"))
(check-expect (same-house-as-parent Wk) (list "E" "F" "A"))

;; todo is (listof Wizard); worklist accumulator
;; rsf  is (listof String); results so far accumulator

(define (same-house-as-parent w)
  (local [(define-struct wandph (w ph))
          ;; Wizard and ParentHouse is (make-wandph Wizard String)
          ;; interp. Wizard and Parent House passed into fn-for-wizard

          (define (fn-for-wizard w ph todo rsf)
            (fn-for-low (append (map (λ (child) (make-wandph child (wizard-house w))) (wizard-children w))
                                todo)
                        (if (string=? (wizard-house w) ph)
                            (append rsf (cons (wizard-name w) empty))
                            rsf)))

          (define (fn-for-low todo rsf)
            (cond [(empty? todo) rsf]
                  [else
                   (fn-for-wizard (wandph-w (first todo)) (wandph-ph (first todo)) (rest todo) rsf)]))]
    (fn-for-wizard w "" empty empty)))

