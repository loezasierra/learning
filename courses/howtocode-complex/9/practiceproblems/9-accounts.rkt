#lang racket

;; accounts-starter.rkt

(define-struct node (id name bal l r))
;; Accounts is one of:
;;  - false
;;  - (make-node Natural String Integer Accounts Accounts)
;; interp. a collection of bank accounts
;;   false represents an empty collection of accounts.
;;   (make-node id name bal l r) is a non-empty collection of accounts such that:
;;    - id is an account identification number (and BST key)
;;    - name is the account holder's name
;;    - bal is the account balance in dollars CAD 
;;    - l and r are further collections of accounts
;; INVARIANT: for a given node:
;;     id is > all ids in its l(eft)  child
;;     id is < all ids in its r(ight) child
;;     the same id never appears twice in the collection

(define ACT0 false)
(define ACT1 (make-node 1 "Mr. Rogers"  22 false false))
(define ACT4 (make-node 4 "Mrs. Doubtfire"  -3
                        false
                        (make-node 7 "Mr. Natural" 13 false false)))
(define ACT3 (make-node 3 "Miss Marple"  600 ACT1 ACT4))
(define ACT42 
  (make-node 42 "Mr. Mom" -79
             (make-node 27 "Mr. Selatcia" 40 
                        (make-node 14 "Mr. Impossible" -9 false false)
                        false)
             (make-node 50 "Miss 604"  16 false false)))
(define ACT10 (make-node 10 "Dr. No" 84 ACT3 ACT42))

; .


#;
(define (fn-for-act act)
  (cond [(false? act) (...)]
        [else
         (... (node-id act)
              (node-name act)
              (node-bal act)
              (fn-for-act (node-l act))
              (fn-for-act (node-r act)))]))


; PROBLEM 1:
; 
; Design an abstract function (including signature, purpose, and tests) 
; to simplify the remove-debtors and remove-profs functions defined below.
; 
; Now re-define the original remove-debtors and remove-profs functions 
; to use your abstract function. Remember, the signature and tests should 
; not change from the original functions.


;; Accounts -> Accounts
;; remove all accounts with a negative balance
(check-expect (remove-debtors (make-node 1 "Mr. Rogers" 22 false false)) 
              (make-node 1 "Mr. Rogers" 22 false false))

(check-expect (remove-debtors (make-node 14 "Mr. Impossible" -9 false false))
              false)

(check-expect (remove-debtors
               (make-node 27 "Mr. Selatcia" 40
                          (make-node 14 "Mr. Impossible" -9 false false)
                          false))
              (make-node 27 "Mr. Selatcia" 40 false false))

(check-expect (remove-debtors 
               (make-node 4 "Mrs. Doubtfire" -3
                          false 
                          (make-node 7 "Mr. Natural" 13 false false)))
              (make-node 7 "Mr. Natural" 13 false false))


(define (remove-debtors act) (local [(define (debtor? act) (negative? (node-bal act)))]
                               (remove-act debtor? act)))


;; Accounts -> Accounts
;; Remove all professors' accounts.  
(check-expect (remove-profs (make-node 27 "Mr. Smith" 100000 false false)) 
              (make-node 27 "Mr. Smith" 100000 false false))
(check-expect (remove-profs (make-node 44 "Prof. Longhair" 2 false false)) false)
(check-expect (remove-profs (make-node 67 "Mrs. Dash" 3000
                                       (make-node 9 "Prof. Booty" -60 false false)
                                       false))
              (make-node 67 "Mrs. Dash" 3000 false false))
(check-expect (remove-profs 
               (make-node 97 "Prof. X" 7
                          false 
                          (make-node 112 "Ms. Magazine" 467 false false)))
              (make-node 112 "Ms. Magazine" 467 false false))


(define (remove-profs act) (local [(define (prof? act) (has-prefix? "Prof." (node-name act)))]
                             (remove-act prof? act)))


;; (Account -> Bool) Accounts -> Accounts
;; remove all accounts where q returns true
(define (remove-69 act) (= 69 (node-id act))) ; test helper. Return true if node id is 69
(check-expect (remove-act remove-69 ACT1) ACT1)
(check-expect (remove-act remove-69 (make-node 69 "Memer" 420 false false)) false)
(check-expect (remove-act remove-69 (make-node 64 "Doge" 808
                                               (make-node 20 "Chocolate" 99 false false)
                                               (make-node 69 "Memer" 420 false false)))
              (make-node 64 "Doge" 808
                         (make-node 20 "Chocolate" 99 false false)
                         false))

(define (remove-act q act)
  (cond [(false? act) false]
        [else
         (if (q act)
             (join (remove-act q (node-l act))
                   (remove-act q (node-r act)))
             (make-node (node-id act)
                        (node-name act)
                        (node-bal act)
                        (remove-act q (node-l act))
                        (remove-act q (node-r act))))]))



;; String String -> Boolean
;; Determine whether pre is a prefix of str.
(check-expect (has-prefix? "" "rock") true)
(check-expect (has-prefix? "rock" "rockabilly") true)
(check-expect (has-prefix? "blues" "rhythm and blues") false)

(define (has-prefix? pre str)
  (string=? pre (substring str 0 (string-length pre))))

;; Accounts Accounts -> Accounts
;; Combine two Accounts's into one
;; ASSUMPTION: all ids in act1 are less than the ids in act2
(check-expect (join ACT42 false) ACT42)
(check-expect (join false ACT42) ACT42)
(check-expect (join ACT1 ACT4) 
              (make-node 4 "Mrs. Doubtfire" -3
                         ACT1
                         (make-node 7 "Mr. Natural" 13 false false)))
(check-expect (join ACT3 ACT42) 
              (make-node 42 "Mr. Mom" -79
                         (make-node 27 "Mr. Selatcia" 40
                                    (make-node 14 "Mr. Impossible" -9
                                               ACT3
                                               false)
                                    false)
                         (make-node 50 "Miss 604" 16 false false)))

(define (join act1 act2)
  (cond [(false? act2) act1]
        [else
         (make-node (node-id act2) 
                    (node-name act2)
                    (node-bal act2)
                    (join act1 (node-l act2))
                    (node-r act2))]))


; PROBLEM 2:
; 
; Using your new abstract function, design a function that removes from a given
; BST any account where the name of the account holder has an odd number of
; characters.  Call it remove-odd-characters.



;; Accounts -> Accounts
;; remove accounts where the name of the account holder has an odd number of characters
(check-expect (remove-odd-characters (make-node 1 "John" 64 false false)) (make-node 1 "John" 64 false false))
(check-expect (remove-odd-characters (make-node 2 "Joe" 64 false false)) false)
(check-expect (remove-odd-characters (make-node 5 "Mary" 20
                                                (make-node 2 "Joe" 64 false false)
                                                (make-node 9 "Alex" 97 false false)))
              (make-node 5 "Mary" 20
                         false
                         (make-node 9 "Alex" 97 false false)))

(define (remove-odd-characters act)
  (local [(define (odd-characters? act)
            (odd? (string-length (node-name act))))]
    (remove-act odd-characters? act)))


; Problem 3:
; 
; Design an abstract fold function for Accounts called fold-act. 
; 
; Use fold-act to design a function called charge-fee that decrements
; the balance of every account in a given collection by the monthly fee of 3 CAD.



;; (Natural String Int X X -> X) X Accounts -> X
;; Abstract fold function for Accounts
(check-expect (local [(define (list-acc-num id name bal l r) (cons id (append l r)))]
                (fold-act list-acc-num empty ACT10))
              (list 10 3 1 4 7 42 27 14 50)) ; list account numbers

(define (fold-act fn b act)
  (cond [(false? act) b]
        [else
         (fn (node-id act)
             (node-name act)
             (node-bal act)
             (fold-act fn b (node-l act))
             (fold-act fn b (node-r act)))]))


;; Accounts -> Accounts
;; Decrement each account balance by 3
(check-expect (charge-fee false) false)
(check-expect (charge-fee (make-node 5 "Mary" 77 false false))
              (make-node 5 "Mary" 74 false false))
(check-expect (charge-fee (make-node 5 "Mary" 77
                                     (make-node 1 "John" 28 false false)
                                     (make-node 9 "Joseph" 54 false false)))
              (make-node 5 "Mary" 74
                         (make-node 1 "John" 25 false false)
                         (make-node 9 "Joseph" 51 false false)))

(define (charge-fee act)
  (local [(define (charge-act id name bal l r) (make-node id name (- bal 3) l r))]
    (fold-act charge-act false act)))


; PROBLEM 4:
; 
; Suppose you needed to design a function to look up an account based on its ID.
; Would it be better to design the function using fold-act, or to design the
; function using the fn-for-acts template?  Briefly justify your answer.


;; Better to use fn-for-acts template.
;; The fold function forces you to search the whole node of accounts,
;; only returning T or F once it gets to the end.
;; With fn-for-acts you could create a function that exits early if true.
