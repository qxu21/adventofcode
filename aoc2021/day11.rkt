#lang typed/racket

;; ADVENT OF CODE 2021
;; qxu21
;; DAY 11

(require "library.rkt")

(define-type Oct-Map (Vectorof Integer))

(: mp : Oct-Map)
(define mp
  (strings-to-int-vector
   (list-from-file
    (open-input-file "day11.txt"))))

(define SIDE 10)
(define AREA 100)

(define-struct Coord
  ([x : Integer]
   [y : Integer])
  #:transparent)

(: get-on-map : Oct-Map Coord -> Integer)
(define (get-on-map mp co)
  (vector-ref
   mp
   (+ (Coord-x co) (* SIDE (Coord-y co)))))

(: set-on-map : Oct-Map Coord Integer -> Oct-Map)
(define (set-on-map mp co v)
  (let
      ([co-i (+ (* SIDE (Coord-y co)) (Coord-x co))])
    (build-vector
     AREA
     (lambda ([i : Integer])
       (if (= i co-i) v (vector-ref mp i))))))

(: get-neighbor-coords : Coord -> (Listof Coord))
(define (get-neighbor-coords co)
  (match co
    [(Coord x y)
     (let
         ([x0? (= x 0)]
          [y0? (= y 0)]
          [xside? (= x (sub1 SIDE))]
          [yside? (= y (sub1 SIDE))])
       (append
        (if x0? '() (list (make-Coord (sub1 x) y)))                       ; left
        (if y0? '() (list (make-Coord x (sub1 y))))                       ; up
        (if xside? '() (list (make-Coord (add1 x) y)))                    ; right
        (if yside? '() (list (make-Coord x (add1 y))))                    ; down
        (if (or x0? y0?) '() (list (make-Coord (sub1 x) (sub1 y))))       ; up-left
        (if (or xside? y0?) '() (list (make-Coord (add1 x) (sub1 y))))    ; up-right
        (if (or xside? yside?) '() (list (make-Coord (add1 x) (add1 y)))) ; down-right
        (if (or x0? yside?) '() (list (make-Coord (sub1 x) (add1 y))))    ; down-left
        ))]))

(: get-neighbor-vals : Oct-Map Coord -> (Listof Integer))
(define (get-neighbor-vals mp co)
  (map
   (lambda ([i : Coord]) (get-on-map mp i))
   (get-neighbor-coords co)))

; parameters reversed for foldl compatibility
(: boost-octopus : Coord Oct-Map -> Oct-Map)
(define (boost-octopus co mp)
  (let
      ([current (get-on-map mp co)])
    (match current
      [0 mp] ; if it's 0, it's already flashed, and the reset happens at end of turn, so don't inc
      [9 (flash-coord mp co)] ; if it's 9, this will make it flash
      [_ (set-on-map mp co (add1 current))]))) ; otherwise just inc it
 
(: flash-coord : Oct-Map Coord -> Oct-Map)
(define (flash-coord init-mp init-co)
  ; boost all neighbors of the flashing octopus
  ; and set this octopus to zero in the process
  (foldl
   boost-octopus
   (set-on-map init-mp init-co 0)
   (get-neighbor-coords init-co)))

(: run-n-turns : Turn Integer -> Turn)
(define (run-n-turns t n)
  (if
   (= n 0)
   t
   (let
       ([upd-mp (do-turn (Turn-mp t))])
     (run-n-turns
      (make-Turn
       upd-mp
       (+ (Turn-n-flashes t) (count-flashes upd-mp)))
      (sub1 n)))))

(define-struct Turn
  ([mp : Oct-Map]
   [n-flashes : Integer]))

(: count-flashes : Oct-Map -> Integer)
(define (count-flashes mp)
  (vector-fold
   (lambda ([v : Integer] [acc : Integer])
     (if (= v 0) (add1 acc) acc))
   0 mp))

(: do-turn : Oct-Map -> Oct-Map)
(define (do-turn init-mp)
  (foldl
   (lambda ([y : Integer] [acc : Oct-Map])
     (foldl
      (lambda ([x : Integer] [mp : Oct-Map])
        (let
            ([co (make-Coord x y)])
          (if
           (> (get-on-map mp co) 9)
           (flash-coord mp co)
           mp)))
      acc
      (range SIDE)))
   (vector-map add1 init-mp)
   (range SIDE)))

(: do-n-turns-diagnostic : Oct-Map Integer -> Void)
(define (do-n-turns-diagnostic mp n)
  (if
   (= n 0)
   (void)
   (let
       ([upd-mp (do-turn mp)])
     (begin
       (show-square-vector upd-mp SIDE)
       (display "FLASHES: ")
       (displayln (count-flashes upd-mp))
       (do-n-turns-diagnostic upd-mp (sub1 n))))))

(: run-until-all-flash : Oct-Map Integer -> Integer)
(define (run-until-all-flash mp n-turns)
  (if
   (vector-fold
    (lambda ([oct : Integer] [all-zero? : Boolean])
      (and all-zero? (= oct 0)))
    #t mp)
   n-turns
   (run-until-all-flash
    (do-turn mp)
    (add1 n-turns))))

(displayln "PART 1")
(Turn-n-flashes
 (run-n-turns (make-Turn mp 0) 100))
(displayln "PART 2")
(run-until-all-flash mp 0)

;(do-n-turns-diagnostic mp 5)
  
  