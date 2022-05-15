#lang racket

;; lookup-in-bst-starter.rkt

; 
; Consider the following data definition for a binary search tree: 
; 


;; Data definitions:

(define-struct node (key val l r))
;; A BST (Binary Search Tree) is one of:
;;  - false
;;  - (make-node Integer String BST BST)
;; interp. false means no BST, or empty BST
;;         key is the node key
;;         val is the node val
;;         l and r are left and right subtrees
;; INVARIANT: for a given node:
;;     key is > all keys in its l(eft)  child
;;     key is < all keys in its r(ight) child
;;     the same key never appears twice in the tree
; .

(define BST0 false)
(define BST1 (make-node 1 "abc" false false))
(define BST4 (make-node 4 "dcj" false (make-node 7 "ruf" false false)))
(define BST3 (make-node 3 "ilk" BST1 BST4))
(define BST42 
  (make-node 42 "ily"
             (make-node 27 "wit" (make-node 14 "olp" false false) false)
             (make-node 50 "dug" false false)))
(define BST10
  (make-node 10 "why" BST3 BST42))
#;
(define (fn-for-bst t)
  (cond [(false? t) (...)]
        [else
         (... (node-key t)    ;Integer
              (node-val t)    ;String
              (fn-for-bst (node-l t))
              (fn-for-bst (node-r t)))]))

;; Template rules used:
;;  - one of: 2 cases
;;  - atomic-distinct: false
;;  - compound: (make-node Integer String BST BST)
;;  - self reference: (node-l t) has type BST
;;  - self reference: (node-r t) has type BST

;; Functions:


; PROBLEM:
; 
; Complete the design of the lookup-key function below. Note that because this is a search function 
; it will sometimes 'fail'. This happens if it is called with an key that does not exist in the BST
; it is provided. If this happens the function should produce false. The signature for such a function
; is written in a special way as shown below.
; 

; .

;; BST Natural -> String or false
;; Try to find node with given key, if found produce value; if not found produce false.
(check-expect (lookup-key BST0  99) false) ; check base case
(check-expect (lookup-key BST1   1) "abc") ; check key found
(check-expect (lookup-key BST1   0) false) ; check false left branch
(check-expect (lookup-key BST1  99) false) ; check false right branch
(check-expect (lookup-key BST10  1) "abc") ; check L L
(check-expect (lookup-key BST10  4) "dcj") ; check L R
(check-expect (lookup-key BST10 27) "wit") ; check R L
(check-expect (lookup-key BST10 50) "dug") ; check R R

; (define (lookup-key t k) "") ;stub

; copy template from BST & additional atomic parameter k
(define (lookup-key t k)
  (cond [(false? t) false]
        [else
         (cond [(= k (node-key t)) (node-val t)]
               [(< k (node-key t)) ; should we go left?
                     (lookup-key (node-l t) k)]
               [(> k (node-key t)) ; should we go right?
                     (lookup-key (node-r t) k)])]))