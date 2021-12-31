#lang typed/racket

;; ADVENT OF CODE 2021
;; qxu21
;; DAY 7

(require "library.rkt")

(define crabs-in (read-in-csv-numbers "day7.txt"))
(define LIMIT (apply max crabs-in))

(: fuel-cost : (Listof Integer) Integer -> Integer)
(define (fuel-cost crabs pos)
  (foldl
   (lambda ([crab : Integer] [fuel-acc : Integer])
     (+ fuel-acc (abs (- crab pos))))
   0 crabs))

; closed form cheesed via desmos
; f(x) = (1/2)(x^2 + x)
(: fuel-cost-tri : (Listof Integer) Integer -> Integer)
(define (fuel-cost-tri crabs pos)
  (foldl
   (lambda ([crab : Integer] [fuel-acc : Integer])
     (let
         ([side (abs (- crab pos))])
       (+ fuel-acc (round (* 1/2 (+ (expt side 2) side))))))
   0 crabs))

(: optimal-fuel-cost : ((Listof Integer) Integer -> Integer) (Listof Integer) -> Integer)
(define (optimal-fuel-cost cost-fn crabs)
  (apply
   min
   (map
    (lambda ([pos : Integer]) (cost-fn crabs pos))
    (range LIMIT))))

(display "PART 1\n")
(optimal-fuel-cost fuel-cost crabs-in)
(display "PART 2\n")
(optimal-fuel-cost fuel-cost-tri crabs-in)