#lang racket

;; hp-family-tree-starter.rkt

; In this problem set you will represent information about descendant family 
; trees from Harry Potter and design functions that operate on those trees.
; 
; To make your task much easier we suggest two things:
;   - you only need a DESCENDANT family tree
;   - read through this entire problem set carefully to see what information 
;     the functions below are going to need. Design your data definitions to
;     only represent that information.
;   - you can find all the information you need by looking at the individual 
;     character pages like the one we point you to for Arthur Weasley.
; 


; PROBLEM 1:
; 
; Design a data definition that represents a family tree from the Harry Potter 
; wiki, which contains all necessary information for the other problems.  You 
; will use this data definition throughout the rest of the homework.
; 


;; Data Definitions

(define-struct character (name patronus wands children))
;; Character is (make-character String String ListOfWand ListOfCharacter)
;; interp. a character in Harry Potter with a name, patronus, list of wands, and children
;;         if patronus is "", the patronus for this character is missing
;;         if wands is empty, the wands for this character is missing
;;         if children is empty, this character has no children

;; ListOfCharacter is one of:
;; - empty
;; - (cons Character ListOfCharacter)
;; interp. a list of Harry Potter characters

;; Example definitions at end of data definitions

(define (fn-for-character c)
  (... (character-name c)
       (character-patronus c)
       (fn-for-low (character-wands c))
       (fn-for-loc (character-children c))))

(define (fn-for-loc loc)
  (cond [(empty? loc) (...)]
        [else
         (... (fn-for-character (first loc))
              (fn-for-loc (rest loc))
              )]))


(define-struct wand (wood core))
;; Wand is (make-wand String String)
;; interp. a wand with a wood material and a core material
;;         if wood is "", the wood material of the wand is unknown
;;         if core is "", the core material of the wand is unknown

;; Example definitions at end of data definitions

(define (fn-for-wand w)
  (... (wand-wood w)
       (wand-core w)))

;; ListOfWand is one of:
;; - empty
;; - (cons Wand ListOfWand)
;; interp. a list of wands

;; Example definitions at end of data definitions

(define (fn-for-low low)
  (cond [(empty? low) (...)]
        [else
         (... (fn-for-wand (first low))
              (fn-for-low (rest low))
              )]))

; PROBLEM 2: 
; 
; Define a constant named ARTHUR that represents the descendant family tree for 
; Arthur Weasley. You can find all the infomation you need by starting 
; at: http://harrypotter.wikia.com/wiki/Arthur_Weasley.
; 
; You must include all of Arthur's children and these grandchildren: Lily, 
; Victoire, Albus, James.
; 
; 
; Note that on the Potter wiki you will find a lot of information. But for some 
; people some of the information may be missing. Enter that information with a 
; special value of "" (the empty string) meaning it is not present. Don't forget
; this special value when writing your interp.
; 

; .


(define VICTOIRE (make-character "Victoire" "" empty empty))
(define LILY (make-character "Lily" "" empty empty))
(define ALBUS-W (make-wand "cherry" ""))
(define ALBUS (make-character "Albus" "" (list ALBUS-W) empty))
(define UNKW (make-wand "" "")) ; wand of unknown wood and core
(define JAMES (make-character "James" "" (list UNKW) empty))

(define BILL (make-character "Bill" "Non-corporeal" (list UNKW) (list VICTOIRE)))
(define CHARLIE-WS (list (make-wand "ash" "unicorn tail hair") UNKW)) ; list of Charlie's wands
(define CHARLIE (make-character "Charlie" "Non-corporeal" CHARLIE-WS empty))
(define PERCY (make-character "Percy" "Non-corporeal" (list UNKW) empty))
(define FRED (make-character "Fred" "Magpie" (list UNKW) empty))
(define GEORGE (make-character "George" "Magpie" (list UNKW) empty))
(define RON-WS (list (make-wand "ash" "unicorn tail hair") (make-wand "willow" "unicorn tail hair") (make-wand "chestnut" "dragon heartstring"))) ; list of Ron's wands
(define RON (make-character "Ron" "Jack Russell terrier" RON-WS empty))
(define GINNY-W (make-wand "yew" "")) ; Ginny's wand
(define GINNY (make-character "Ginny" "Horse" (list GINNY-W) (list LILY ALBUS JAMES)))

(define ARTHUR (make-character "Arthur" "Weasel" (list UNKW) (list BILL CHARLIE PERCY FRED GEORGE RON GINNY)))


; PROBLEM 3:
; 
; Design a function that produces a pair list (i.e. list of two-element lists)
; of every person in the tree and his or her patronus. For example, assuming 
; that HARRY is a tree representing Harry Potter and that he has no children
; (even though we know he does) the result would be: (list (list "Harry" "Stag")).
; 
; You must use ARTHUR as one of your examples.
; 



;; Character -> ListofListofStrings
;; ListOfCharacter -> ListofListofStrings
;; return a pair list for every person in the family tree and their patronus
(check-expect (char-patron VICTOIRE) (list (list "Victoire" "")))
(check-expect (char-patron-of-list empty) empty)
(check-expect (char-patron GINNY) (list (list "Ginny" "Horse") (list "Lily" "") (list "Albus" "") (list "James" "")))
(check-expect (char-patron-of-list (list LILY ALBUS JAMES)) (list (list "Lily" "") (list "Albus" "") (list "James" "")))
(check-expect (char-patron ARTHUR) (list (list "Arthur" "Weasel")
                                         (list "Bill" "Non-corporeal") (list "Victoire" "")
                                         (list "Charlie" "Non-corporeal")
                                         (list "Percy" "Non-corporeal")
                                         (list "Fred" "Magpie")
                                         (list "George" "Magpie")
                                         (list "Ron" "Jack Russell terrier")
                                         (list "Ginny" "Horse") (list "Lily" "") (list "Albus" "") (list "James" "")))

; (define (char-patron c) (list empty)) ;stub
; (define (char-patron-of-list loc) (list empty)) ;stub

(define (char-patron c)
  (cons (list (character-name c) (character-patronus c))
        (char-patron-of-list (character-children c))))

(define (char-patron-of-list loc)
  (cond [(empty? loc) empty]
        [else
         (append (char-patron (first loc))
                 (char-patron-of-list (rest loc))
                 )]))


; PROBLEM 4:
; 
; Design a function that produces the names of every person in a given tree 
; whose wands are made of a given material. 
; 
; You must use ARTHUR as one of your examples.
; 



;; Character String -> ListOfString
;; ListOfCharacter String -> ListOfString
;; return a list of names of every person in Character tree whose wands are made of String material
(check-expect (match-wand VICTOIRE "ash") empty)
(check-expect (match-wand-in-list empty "willow") empty)
(check-expect (match-wand ALBUS "ash") empty)
(check-expect (match-wand ALBUS "cherry") (list "Albus"))
(check-expect (match-wand GINNY "chestnut") empty)
(check-expect (match-wand GINNY "yew") (list "Ginny"))
(check-expect (match-wand GINNY "cherry") (list "Albus"))
(check-expect (match-wand-in-list (list LILY ALBUS JAMES) "cherry") (list "Albus"))
(check-expect (match-wand GINNY "") (list "Ginny" "Albus" "James"))
(check-expect (match-wand ARTHUR "Maple") empty)
(check-expect (match-wand ARTHUR "ash") (list "Charlie" "Ron"))
(check-expect (match-wand ARTHUR "unicorn tail hair") (list "Charlie" "Ron"))

; (define (match-wand c mat) (list empty)) ;stub
; (define (match-wand-in-list loc mat) (list empty)) ;stub

(define (match-wand c mat)
  (if (wands-match? mat (character-wands c))
      (cons (character-name c) (match-wand-in-list (character-children c) mat))
      (append (match-wand-in-list (character-children c) mat) empty)))

(define (match-wand-in-list loc mat)
  (cond [(empty? loc) empty]
        [else
         (append (match-wand (first loc) mat)
                 (match-wand-in-list (rest loc) mat)
                 )]))


;; String ListOfWand -> Bool
;; return true if String in ListOfWand
(check-expect (wands-match? "ash" empty) false)
(check-expect (wands-match? "ash" (list UNKW)) false)
(check-expect (wands-match? "" (list UNKW)) true)
(check-expect (wands-match? "yew" (list GINNY-W)) true)
(check-expect (wands-match? "yew" RON-WS) false)
(check-expect (wands-match? "willow" RON-WS) true)
(check-expect (wands-match? "dragon heartstring" RON-WS) true)

; (define (wands-match? mat low) false) ;stub

(define (wands-match? s low)
  (cond [(empty? low) false]
        [else
         (if (wand-match? s (first low))
             true
             (wands-match? s (rest low))
             )]))


;; String Wand -> Bool
;; return true if String matches either wood or core material
(check-expect (wand-match? "" (make-wand "ash" "magic")) false)
(check-expect (wand-match? "" UNKW) true)
(check-expect (wand-match? "ash" (make-wand "ash" "magic")) true)
(check-expect (wand-match? "magic" (make-wand "ash" "magic")) true)

; (define (wand-match? s w) false)

(define (wand-match? s w)
  (cond [(string=? s (wand-wood w)) true]
        [(string=? s (wand-core w)) true]
        [else false]))
