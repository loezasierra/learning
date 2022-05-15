#lang racket

;; insert-starter.rkt

; 
; PROBLEM:
; 
; Design a function that consumes an Integer, String and BST, and adds a node
; that has the given key and value to the tree. The node should be inserted in 
; the proper place in the tree. The function can assume there is not already 
; an entry for that number in the tree. The function should produce the new BST.
; 
; Do not worry about keeping the tree balanced. We will come back to this later.
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

(define BST0 false)
(define BST1 (make-node 1 "abc" false false))
(define BST4 (make-node 4 "dcj" false (make-node 7 "ruf" false false)))
(define BST3 (make-node 3 "ilk" BST1 BST4))
(define BST42 
  (make-node 42 "ily"
             (make-node 27 "wit" (make-node 14 "olp" false false) false)
             false))
(define BST10 (make-node 10 "why" BST3 BST42))


; .

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


;; Integer String BST -> BST
;; add a node of (Integer String) into the proper place in BST
(check-expect (add-node 2 "bit" BST0) (make-node 2 "bit" false false)) ; check base case - inserting into empty tree
(check-expect (add-node 2 "bit" (make-node 7 "tst" false false)) (make-node 7 "tst"
                                                                            (make-node 2 "bit" false false)
                                                                            false)) ; check inserting into left of single node
(check-expect (add-node 9 "foo" (make-node 7 "tst" false false)) (make-node 7 "tst"
                                                                            false
                                                                            (make-node 9 "foo" false false))) ; check inserting into right of single node
(check-expect (add-node 2 "bit" BST3) (make-node 3 "ilk"
                                                 (make-node 1 "abc" false (make-node 2 "bit" false false))
                                                 BST4)) ; check inserting into right of deep node
(check-expect (add-node 5 "bar" BST3) (make-node 3 "ilk"
                                                 BST1
                                                 (make-node 4 "dcj" false
                                                            (make-node 7 "ruf" (make-node 5 "bar" false false) false)))) ; check inserting into left of deep node


; (define (add-node i s t) t) ;stub

(define (add-node i s t)
  (cond [(false? t) (make-node i s false false)]
        [else
         (cond [(< i (node-key t))
                (make-node
                 (node-key t)
                 (node-val t)
                 (add-node i s (node-l t))
                 (node-r t))] ; go left if greater than current node
               [else
                (make-node
                 (node-key t)
                 (node-val t)
                 (node-l t)
                 (add-node i s (node-r t)))]) ; go right if smaller than current node
         ]))
