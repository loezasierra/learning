#lang racket

(define (block color) (square 20 "solid" color))

(above (beside (block "blue") (block "yellow"))
       (beside (block "yellow") (block "blue")))