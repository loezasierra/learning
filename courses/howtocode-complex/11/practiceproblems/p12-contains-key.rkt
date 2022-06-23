#lang racket

;; bt-contains-tr-starter.rkt

; Problem:
; 
; Starting with the following data definition for a binary tree (not a binary search tree) 
; design a tail-recursive function called contains? that consumes a key and a binary tree 
; and produces true if the tree contains the key.
; 


(define-struct node (k v l r))
;; BT is one of:
;;  - false
;;  - (make-node Integer String BT BT)
;; Interp. A binary tree, each node has a key, value and 2 children
(define BT1 false)
(define BT2 (make-node 1 "a"
                       (make-node 6 "f"
                                  (make-node 4 "d" false false)
                                  false)
                       (make-node 7 "g" false false)))


;; Integer BT -> Bool
;; return true if BT contains key
(check-expect (contains? 9 BT2) false)
(check-expect (contains? 1 BT2) true)
(check-expect (contains? 4 BT2) true)

(define (contains? key bt0)
  ;; found is bool; true if node has matched key
  ;; (contains? 4 BT2)
  ;; (contains? BT2)
  ;; (contains? (make-node 1 "a") false)
  ;; (contains? (make-node 6 "f") false)
  ;; ...

  ;; todo is worklist accumulator
  (local [(define (contains? bt found todo)
            (cond [(false? bt) found]
                  [else
                   (fn-for-bts (or (equal? (node-k bt) key) found)
                               (append (list (node-l bt) (node-r bt)) todo))]))
          (define (fn-for-bts found todo)
            (cond [(empty? todo) found]
                  [else
                   (contains? (first todo) found (rest todo))]))]
    (contains? bt0 false empty)))
