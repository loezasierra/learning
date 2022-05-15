#lang racket

;; render-bst-w-lines-starter.rkt

(require 2htdp/image)

; PROBLEM:
; 
; Given the following data definition for a binary search tree,
; design a function that consumes a bst and produces a SIMPLE 
; rendering of that bst including lines between nodes and their 
; subnodes.
; 
; To help you get started, we've added some sketches below of 
; one way you could include lines to a bst.


;; Constants

(define TEXT-SIZE  14)
(define TEXT-COLOR "BLACK")

(define KEY-VAL-SEPARATOR ":")

(define MTTREE (rectangle 20 1 "solid" "white"))


(define VSPACE (rectangle 1 10 "solid" "white"))
(define HSPACE (rectangle 16 1 "solid" "white"))


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
(define BST7 (make-node 7 "ruf" false false)) 
(define BST4 (make-node 4 "dcj" false (make-node 7 "ruf" false false)))
(define BST3 (make-node 3 "ilk" BST1 BST4))
(define BST42 
  (make-node 42 "ily"
             (make-node 27 "wit" (make-node 14 "olp" false false) false)
             (make-node 50 "dug" false false)))
(define BST10
  (make-node 10 "why" BST3 BST42))
(define BST100 
  (make-node 100 "large" BST10 false))
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
;; BST -> Image
;; produces a simple image of a binary search tree
(check-expect (bst-img BST0) MTTREE) ; check base case
(check-expect (bst-img BST4) (above (text (string-append "4" KEY-VAL-SEPARATOR "dcj")
                                          TEXT-SIZE
                                          TEXT-COLOR)
                                    (add-line (rectangle
                                               (+ (image-width MTTREE)
                                                  (image-width HSPACE)
                                                  (image-width (text (string-append "7" KEY-VAL-SEPARATOR "ruf") TEXT-SIZE TEXT-COLOR)))
                                               (/ (+ (image-width MTTREE)
                                                     (image-width HSPACE)
                                                     (image-width (text (string-append "7" KEY-VAL-SEPARATOR "ruf") TEXT-SIZE TEXT-COLOR))) 4)
                                               "solid"
                                               "white")
                                              (/ (+ (image-width MTTREE)
                                                    (image-width HSPACE)
                                                    (image-width (text (string-append "7" KEY-VAL-SEPARATOR "ruf") TEXT-SIZE TEXT-COLOR))) 2)
                                              0
                                              (+ (image-width MTTREE)
                                                 (image-width HSPACE)
                                                 (/ (image-width (text (string-append "7" KEY-VAL-SEPARATOR "ruf") TEXT-SIZE TEXT-COLOR)) 2))
                                              (/ (+ (image-width MTTREE)
                                                    (image-width HSPACE)
                                                    (image-width (text (string-append "7" KEY-VAL-SEPARATOR "ruf") TEXT-SIZE TEXT-COLOR))) 4)
                                              "black")
                                    (beside MTTREE
                                            HSPACE
                                            (text (string-append "7" KEY-VAL-SEPARATOR "ruf")
                                                  TEXT-SIZE
                                                  TEXT-COLOR))))
(check-expect (bst-img BST3) (above (text (string-append "3" KEY-VAL-SEPARATOR "ilk")
                                          TEXT-SIZE
                                          TEXT-COLOR)
                                    (add-line (add-line (rectangle
                                                         (+ (image-width (text (string-append "1" KEY-VAL-SEPARATOR "abc") TEXT-SIZE TEXT-COLOR))
                                                            (image-width HSPACE)
                                                            (image-width (bst-img BST4)))
                                                         (/ (+ (image-width (text (string-append "1" KEY-VAL-SEPARATOR "abc") TEXT-SIZE TEXT-COLOR))
                                                               (image-width HSPACE)
                                                               (image-width (bst-img BST4))) 4)
                                                         "solid"
                                                         "white")
                                                        (/ (+ (image-width (text (string-append "1" KEY-VAL-SEPARATOR "abc") TEXT-SIZE TEXT-COLOR))
                                                              (image-width HSPACE)
                                                              (image-width (bst-img BST4))) 2)
                                                        0
                                                        (/ (image-width (text (string-append "1" KEY-VAL-SEPARATOR "abc") TEXT-SIZE TEXT-COLOR)) 2)
                                                        (/ (+ (image-width (text (string-append "1" KEY-VAL-SEPARATOR "abc") TEXT-SIZE TEXT-COLOR))
                                                              (image-width HSPACE)
                                                              (image-width (bst-img BST4))) 4)
                                                        "black")
                                              (/ (+ (image-width (text (string-append "1" KEY-VAL-SEPARATOR "abc") TEXT-SIZE TEXT-COLOR))
                                                    (image-width HSPACE)
                                                    (image-width (bst-img BST4))) 2)
                                              0
                                              (+ (image-width (text (string-append "1" KEY-VAL-SEPARATOR "abc") TEXT-SIZE TEXT-COLOR))
                                                 (image-width HSPACE)
                                                 (/ (image-width (bst-img BST4)) 2))
                                              (/ (+ (image-width (text (string-append "1" KEY-VAL-SEPARATOR "abc") TEXT-SIZE TEXT-COLOR))
                                                    (image-width HSPACE)
                                                    (image-width (bst-img BST4))) 4)
                                              "black")
                                    (beside (text (string-append "1" KEY-VAL-SEPARATOR "abc")
                                                  TEXT-SIZE
                                                  TEXT-COLOR)
                                            HSPACE
                                            (bst-img BST4))))

; (define (bst-img bst) MTTREE) ;stub

(define (bst-img t)
  (cond [(false? t) MTTREE]
        [else
         (above (keyval-text (node-key t) (node-val t))
                (draw-lines (bst-img (node-l t)) (bst-img (node-r t)))
                (beside (bst-img (node-l t))
                        HSPACE
                        (bst-img (node-r t))))]))


;; Int String -> Image
;; produce an image of a key:value pair as text
(check-expect (keyval-text 1 "abc") (text (string-append "1" KEY-VAL-SEPARATOR "abc") TEXT-SIZE TEXT-COLOR))
(check-expect (keyval-text 3 "ilk") (text (string-append "3" KEY-VAL-SEPARATOR "ilk") TEXT-SIZE TEXT-COLOR))

; (define (keyval-text i s) empty-image) ;stub

(define (keyval-text i s)
  (text (string-append (number->string i) KEY-VAL-SEPARATOR s) TEXT-SIZE TEXT-COLOR))


;; Img Img -> Img
;; produce lines connecting top bts node to children nodes
(check-expect (draw-lines MTTREE (text (string-append "7" KEY-VAL-SEPARATOR "ruf") TEXT-SIZE TEXT-COLOR))
              (add-line (rectangle
                         (+ (image-width MTTREE)
                            (image-width HSPACE)
                            (image-width (text (string-append "7" KEY-VAL-SEPARATOR "ruf") TEXT-SIZE TEXT-COLOR)))
                         (/ (+ (image-width MTTREE)
                               (image-width HSPACE)
                               (image-width (text (string-append "7" KEY-VAL-SEPARATOR "ruf") TEXT-SIZE TEXT-COLOR))) 4)
                         "solid"
                         "white")
                        (/ (+ (image-width MTTREE)
                              (image-width HSPACE)
                              (image-width (text (string-append "7" KEY-VAL-SEPARATOR "ruf") TEXT-SIZE TEXT-COLOR))) 2)
                        0
                        (+ (image-width MTTREE)
                           (image-width HSPACE)
                           (/ (image-width (text (string-append "7" KEY-VAL-SEPARATOR "ruf") TEXT-SIZE TEXT-COLOR)) 2))
                        (/ (+ (image-width MTTREE)
                              (image-width HSPACE)
                              (image-width (text (string-append "7" KEY-VAL-SEPARATOR "ruf") TEXT-SIZE TEXT-COLOR))) 4)
                        "black"))
(check-expect (draw-lines (text (string-append "1" KEY-VAL-SEPARATOR "abc") TEXT-SIZE TEXT-COLOR) MTTREE)
              (add-line (rectangle
                         (+ (image-width (text (string-append "1" KEY-VAL-SEPARATOR "abc") TEXT-SIZE TEXT-COLOR))
                            (image-width HSPACE)
                            (image-width MTTREE))
                         (/ (+ (image-width (text (string-append "1" KEY-VAL-SEPARATOR "abc") TEXT-SIZE TEXT-COLOR))
                               (image-width HSPACE)
                               (image-width MTTREE)) 4)
                         "solid"
                         "white")
                        (/ (+ (image-width (text (string-append "1" KEY-VAL-SEPARATOR "abc") TEXT-SIZE TEXT-COLOR))
                              (image-width HSPACE)
                              (image-width MTTREE)) 2)
                        0
                        (/ (image-width (text (string-append "1" KEY-VAL-SEPARATOR "abc") TEXT-SIZE TEXT-COLOR)) 2)
                        (/ (+ (image-width (text (string-append "1" KEY-VAL-SEPARATOR "abc") TEXT-SIZE TEXT-COLOR))
                              (image-width HSPACE)
                              (image-width MTTREE)) 4)
                        "black"))
(check-expect (draw-lines (text (string-append "1" KEY-VAL-SEPARATOR "abc") TEXT-SIZE TEXT-COLOR) (text (string-append "7" KEY-VAL-SEPARATOR "ruf") TEXT-SIZE TEXT-COLOR))
              (add-line (add-line (rectangle
                                   (+ (image-width (text (string-append "1" KEY-VAL-SEPARATOR "abc") TEXT-SIZE TEXT-COLOR))
                                      (image-width HSPACE)
                                      (image-width (text (string-append "7" KEY-VAL-SEPARATOR "ruf") TEXT-SIZE TEXT-COLOR)))
                                   (/ (+ (image-width (text (string-append "1" KEY-VAL-SEPARATOR "abc") TEXT-SIZE TEXT-COLOR))
                                         (image-width HSPACE)
                                         (image-width (text (string-append "7" KEY-VAL-SEPARATOR "ruf") TEXT-SIZE TEXT-COLOR))) 4)
                                   "solid"
                                   "white")
                                  (/ (+ (image-width (text (string-append "1" KEY-VAL-SEPARATOR "abc") TEXT-SIZE TEXT-COLOR))
                                        (image-width HSPACE)
                                        (image-width (text (string-append "7" KEY-VAL-SEPARATOR "ruf") TEXT-SIZE TEXT-COLOR))) 2)
                                  0
                                  (/ (image-width (text (string-append "1" KEY-VAL-SEPARATOR "abc") TEXT-SIZE TEXT-COLOR)) 2)
                                  (/ (+ (image-width (text (string-append "1" KEY-VAL-SEPARATOR "abc") TEXT-SIZE TEXT-COLOR))
                                        (image-width HSPACE)
                                        (image-width (text (string-append "7" KEY-VAL-SEPARATOR "ruf") TEXT-SIZE TEXT-COLOR))) 4)
                                  "black")
                        (/ (+ (image-width (text (string-append "1" KEY-VAL-SEPARATOR "abc") TEXT-SIZE TEXT-COLOR))
                              (image-width HSPACE)
                              (image-width (text (string-append "7" KEY-VAL-SEPARATOR "ruf") TEXT-SIZE TEXT-COLOR))) 2)
                        0
                        (+ (image-width (text (string-append "1" KEY-VAL-SEPARATOR "abc") TEXT-SIZE TEXT-COLOR))
                           (image-width HSPACE)
                           (/ (image-width (text (string-append "7" KEY-VAL-SEPARATOR "ruf") TEXT-SIZE TEXT-COLOR)) 2))
                        (/ (+ (image-width (text (string-append "1" KEY-VAL-SEPARATOR "abc") TEXT-SIZE TEXT-COLOR))
                              (image-width HSPACE)
                              (image-width (text (string-append "7" KEY-VAL-SEPARATOR "ruf") TEXT-SIZE TEXT-COLOR))) 4)
                        "black"))

; (define (draw-lines img1 img2) empty-image)

(define (draw-lines img1 img2)
  (cond [(and (image=? img1 MTTREE) (image=? img2 MTTREE)) empty-image] ; draw no lines
        [(image=? img1 MTTREE)
         (draw-right-line (draw-rect img1 img2) img1 img2)] ; add right line
        [(image=? img2 MTTREE)
         (draw-left-line (draw-rect img1 img2) img1 img2)] ; add left line
        [else
         (draw-left-line (draw-right-line (draw-rect img1 img2) img1 img2) img1 img2)])) ; draw both lines


;; Img Img -> Image
;; draw rectangle for draw-lines based on images
(check-expect (draw-rect (text (string-append "1" KEY-VAL-SEPARATOR "abc") TEXT-SIZE TEXT-COLOR)
                         (text (string-append "7" KEY-VAL-SEPARATOR "ruf") TEXT-SIZE TEXT-COLOR))
              (rectangle
               (+ (image-width (text (string-append "1" KEY-VAL-SEPARATOR "abc") TEXT-SIZE TEXT-COLOR))
                  (image-width HSPACE)
                  (image-width (text (string-append "7" KEY-VAL-SEPARATOR "ruf") TEXT-SIZE TEXT-COLOR)))
               (/ (+ (image-width (text (string-append "1" KEY-VAL-SEPARATOR "abc") TEXT-SIZE TEXT-COLOR))
                     (image-width HSPACE)
                     (image-width (text (string-append "7" KEY-VAL-SEPARATOR "ruf") TEXT-SIZE TEXT-COLOR))) 4)
               "solid"
               "white"))

(define (draw-rect img1 img2)
  (rectangle
   (+ (image-width img1)
      (image-width HSPACE)
      (image-width img2))
   (/ (+ (image-width img1)
         (image-width HSPACE)
         (image-width img2)) 4)
   "solid"
   "white"))


;; Img Img Img -> Image
;; draw left line for draw-lines on top of rect based on img1 and img2
(check-expect (draw-left-line (rectangle
                               (+ (image-width (text (string-append "1" KEY-VAL-SEPARATOR "abc") TEXT-SIZE TEXT-COLOR))
                                  (image-width HSPACE)
                                  (image-width MTTREE))
                               (/ (+ (image-width (text (string-append "1" KEY-VAL-SEPARATOR "abc") TEXT-SIZE TEXT-COLOR))
                                     (image-width HSPACE)
                                     (image-width MTTREE)) 4)
                               "solid"
                               "white")
                              (text (string-append "1" KEY-VAL-SEPARATOR "abc") TEXT-SIZE TEXT-COLOR)
                              MTTREE)
              (add-line (rectangle
                         (+ (image-width (text (string-append "1" KEY-VAL-SEPARATOR "abc") TEXT-SIZE TEXT-COLOR))
                            (image-width HSPACE)
                            (image-width MTTREE))
                         (/ (+ (image-width (text (string-append "1" KEY-VAL-SEPARATOR "abc") TEXT-SIZE TEXT-COLOR))
                               (image-width HSPACE)
                               (image-width MTTREE)) 4)
                         "solid"
                         "white")
                        (/ (+ (image-width (text (string-append "1" KEY-VAL-SEPARATOR "abc") TEXT-SIZE TEXT-COLOR))
                              (image-width HSPACE)
                              (image-width MTTREE)) 2)
                        0
                        (/ (image-width (text (string-append "1" KEY-VAL-SEPARATOR "abc") TEXT-SIZE TEXT-COLOR)) 2)
                        (/ (+ (image-width (text (string-append "1" KEY-VAL-SEPARATOR "abc") TEXT-SIZE TEXT-COLOR))
                              (image-width HSPACE)
                              (image-width MTTREE)) 4)
                        "black"))

; (define (draw-left-line rect img1 img2) empty-image) ;stub
(define (draw-left-line rect img1 img2)
  (add-line rect
            (/ (+ (image-width img1)
                  (image-width HSPACE)
                  (image-width img2)) 2)
            0
            (/ (image-width img1) 2)
            (/ (+ (image-width img1)
                  (image-width HSPACE)
                  (image-width img2)) 4)
            "black"))


;; Img Img Img -> Image
;; draw right line for draw-lines on top of rect based on img1 and img2
(check-expect (draw-right-line (rectangle
                                (+ (image-width MTTREE)
                                   (image-width HSPACE)
                                   (image-width (text (string-append "7" KEY-VAL-SEPARATOR "ruf") TEXT-SIZE TEXT-COLOR)))
                                (/ (+ (image-width MTTREE)
                                      (image-width HSPACE)
                                      (image-width (text (string-append "7" KEY-VAL-SEPARATOR "ruf") TEXT-SIZE TEXT-COLOR))) 4)
                                "solid"
                                "white")
                               MTTREE
                               (text (string-append "7" KEY-VAL-SEPARATOR "ruf") TEXT-SIZE TEXT-COLOR))
              (add-line (rectangle
                         (+ (image-width MTTREE)
                            (image-width HSPACE)
                            (image-width (text (string-append "7" KEY-VAL-SEPARATOR "ruf") TEXT-SIZE TEXT-COLOR)))
                         (/ (+ (image-width MTTREE)
                               (image-width HSPACE)
                               (image-width (text (string-append "7" KEY-VAL-SEPARATOR "ruf") TEXT-SIZE TEXT-COLOR))) 4)
                         "solid"
                         "white")
                        (/ (+ (image-width MTTREE)
                              (image-width HSPACE)
                              (image-width (text (string-append "7" KEY-VAL-SEPARATOR "ruf") TEXT-SIZE TEXT-COLOR))) 2)
                        0
                        (+ (image-width MTTREE)
                           (image-width HSPACE)
                           (/ (image-width (text (string-append "7" KEY-VAL-SEPARATOR "ruf") TEXT-SIZE TEXT-COLOR)) 2))
                        (/ (+ (image-width MTTREE)
                              (image-width HSPACE)
                              (image-width (text (string-append "7" KEY-VAL-SEPARATOR "ruf") TEXT-SIZE TEXT-COLOR))) 4)
                        "black"))

; (define (draw-right-line rect img1 img2) empty-image) ;stub

(define (draw-right-line rect img1 img2)
  (add-line rect
            (/ (+ (image-width img1)
                  (image-width HSPACE)
                  (image-width img2)) 2)
            0
            (+ (image-width img1)
               (image-width HSPACE)
               (/ (image-width img2) 2))
            (/ (+ (image-width img1)
                  (image-width HSPACE)
                  (image-width img2)) 4)
            "black"))


; 
; Here is a sketch of one way the lines could work. What 
; this sketch does is allows us to see the structure of
; the functions pretty clearly. We'll have one helper for
; the key value image, and one helper to draw the lines.
; Each of those produces a rectangular image of course.
; 
; .
; 
; And here is a sketch of the helper that draws the lines:
; .  
; where lw means width of left subtree image and
;       rw means width of right subtree image

