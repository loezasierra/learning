#lang racket

;; demolish-starter.rkt

;; =================
;; Data definitions:

; 
; PROBLEM A:
; 
; You are assigned to develop a system that will classify 
; buildings in downtown Vancouver based on how old they are. 
; According to city guidelines, there are three different classification levels:
; new, old, and heritage.
; 
; Design a data definition to represent these classification levels. 
; Call it BuildingStatus.
; 


;; BuildingStatus is one of:
;; - "new"
;; - "old"
;; - "heritage"
;; interp. classification of building based on its age

;; examples are redundant for enumerations
#;
(define (fn-for-buildingstatus bs)
  (cond [(string=? "new" bs)      (...)]
        [(string=? "old" bs)      (...)]
        [(string=? "heritage" bs) (...)]))

;; Template rules used:
;; - one of: 3 cases
;; - atomic distinct: "new"
;; - atomic distinct: "old"
;; - atomic distinct: "heritage"

;; =================
;; Functions:

; 
; PROBLEM B:
; 
; The city wants to demolish all buildings classified as "old". 
; You are hired to design a function called demolish? 
; that determines whether a building should be torn down or not.
; 


; BuildingStatus -> bool
; returns true if BuildingStatus is "old"
; (define (demolish? bs) false);stub

(check-expect (demolish? "new")      false)
(check-expect (demolish? "old")      true)
(check-expect (demolish? "heritage") false)

; copy template from BuildingStatus
(define (demolish? bs)
  (string=? "old" bs))