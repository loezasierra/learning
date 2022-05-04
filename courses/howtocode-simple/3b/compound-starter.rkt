#lang racket

;; compound-starter.rkt

; 
; PROBLEM:
; 
; Design a data definition to represent hockey players, including both 
; their first and last names.
; 


(define-struct player (fn ln))
;; Player is (make-player String String)
;; interp. (make-player fn ln) is a hockey player where
;;          fn is first name
;;          ln is last name
(define P1 (make-player "Wayne" "Gretzky"))

(define (fn-for-player p)
  (... (player-fn p)   ; String
       (player-ln p))) ; String

;; Template rules used:
;; - Compound: 2 fields
