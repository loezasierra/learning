#lang racket

;; movie-starter.rkt

;; =================
;; Data definitions:

; 
; PROBLEM A:
; 
; Design a data definition to represent a movie, including  
; title, budget, and year released.
; 
; To help you to create some examples, find some interesting movie facts below: 
; "Titanic" - budget: 200000000 released: 1997
; "Avatar" - budget: 237000000 released: 2009
; "The Avengers" - budget: 220000000 released: 2012
; 
; However, feel free to resarch more on your own!
; 


(define-struct movie (title year budget))
;; Movie is (make-movie String Integer Integer)
;; interp. (make-movie title year budget) is movie where:
;;          title is movie title
;;          year is year the movie released
;;          budget is the movie budget
(define M1 (make-movie "Titanic"      1997 200000000))
(define M2 (make-movie "Avatar"       2009 237000000))
(define M3 (make-movie "The Avengers" 2012 220000000))
#;
(define (fn-for-movie m)
  (... (movie-title m)    ; String
       (movie-year m)     ; Integer
       (movie-budget m))) ; Integer

;; Template rules used:
;; - compound: 3 fields

;; =================
;; Functions:

; 
; PROBLEM B:
; 
; You have a list of movies you want to watch, but you like to watch your 
; rentals in chronological order. Design a function that consumes two movies 
; and produces the title of the most recently released movie.
; 
; Note that the rule for templating a function that consumes two compound data 
; parameters is for the template to include all the selectors for both 
; parameters.
; 


;; Movie Movie -> Movie-title
;; Given 2 movies, return the most recently released movie
(check-expect (newer-movie M1 M2) "Avatar")
(check-expect (newer-movie M2 M3) "The Avengers")
(check-expect (newer-movie M3 M1) "The Avengers")

; (define (newer-movie movie1 movie2) "Bambi") ; stub

#;
(define (newer-movie movie1 movie2) ; template
  (... (movie-title movie1)    ; string
       (movie-year movie1)     ; integer
       (movie-budget movie1)   ; integer
       (movie-title movie2)    ; string
       (movie-year movie2)     ; integer
       (movie-budget movie2))) ; integer

(define (newer-movie movie1 movie2)
  (if (> (movie-year movie1) (movie-year movie2))
      (movie-title movie1)
      (movie-title movie2)))