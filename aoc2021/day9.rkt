#lang typed/racket

;; ADVENT OF CODE 2021
;; qxu21
;; DAY 9

(require "library.rkt")

(define-type Height-Map (Vectorof Integer))
(define list-in (list-from-file (open-input-file "day9.txt")))
(define SIDE (string-length (first list-in)))
(define AREA (expt SIDE 2))

(define-struct Coord
  ([x : Integer]
   [y : Integer])
  #:transparent)

(: strings-to-vector : (Listof String) -> Height-Map)
(define (strings-to-vector li)
  (list->vector
   (map
    char->int
    (apply append
     (map string->list li)))))

(: get-on-map : Height-Map Coord -> Integer)
(define (get-on-map mp co)
  (vector-ref
   mp
   (+ (Coord-x co) (* SIDE (Coord-y co)))))

(: get-neighbor-coords : Coord -> (Listof Coord))
(define (get-neighbor-coords co)
  (match co
    [(Coord x y)
     (append
      (if (= x 0) '() (list (make-Coord (sub1 x) y)))
      (if (= y 0) '() (list (make-Coord x (sub1 y))))
      (if (= x (sub1 SIDE)) '() (list (make-Coord (add1 x) y)))
      (if (= y (sub1 SIDE)) '() (list (make-Coord x (add1 y)))))]))

(: get-neighbor-vals : Height-Map Coord -> (Listof Integer))
(define (get-neighbor-vals mp co)
  (map
   (lambda ([i : Coord]) (get-on-map mp i))
   (get-neighbor-coords co)))

(: calculate-risk-levels : Height-Map -> Integer)
(define (calculate-risk-levels mp)
  (foldl
   (lambda ([y : Integer] [acc : Integer])
     (foldl
      (lambda ([x : Integer] [risk-level : Integer])
        (let*
            ([co (make-Coord x y)]
             [current (get-on-map mp co)])
          (if
           (andmap
            (lambda ([neighbor : Integer])
              (< current neighbor))
            (get-neighbor-vals mp co))
           (+ risk-level current 1)
           risk-level)))
      acc
      (range SIDE)))
   0
   (range SIDE)))

(: get-low-points : Height-Map -> (Listof Coord))
(define (get-low-points mp)
  (foldl
   (lambda ([y : Integer] [acc : (Listof Coord)])
     (foldl
      (lambda ([x : Integer] [low-points : (Listof Coord)])
        (let*
            ([co (make-Coord x y)]
             [current (get-on-map mp co)])
          (if
           (andmap
            (lambda ([neighbor : Integer])
              (< current neighbor))
            (get-neighbor-vals mp co))
           (cons co low-points)
           low-points)))
      acc
      (range SIDE)))
   '()
   (range SIDE)))

(: calculate-basin-size : Height-Map Coord -> Integer)
(define (calculate-basin-size mp co)
  (cbs-aux mp (set co) (set) 0))

(: show-coord : Coord -> Void)
(define (show-coord co)
  (display "( ")
  (display (Coord-x co))
  (display ", ")
  (display (Coord-y co))
  (display ")"))

(: cbs-aux : Height-Map (Setof Coord) (Setof Coord) Integer -> Integer)
(define (cbs-aux mp frontier basin n)
  (if
   (set-empty? frontier)
   n
   (let*
       ([co (set-first frontier)])
     (cbs-aux
      mp
      ; put new items at the end of the frontier
      (set-union
       (set-rest frontier)
       (list->set
        (filter
         ; filter to neigbors not already in the basin or height 9
         (lambda ([crd : Coord])
           (and
            (not (set-member? basin crd))
            (not (= (get-on-map mp crd) 9))))
         (get-neighbor-coords co))))
      ; put co into the basin
      (set-add basin co)
      ; count n
      (add1 n)))))
  
(: calculate-all-basin-sizes : Height-Map -> (Listof Integer))
(define (calculate-all-basin-sizes mp)
  (map
   (lambda ([low-point : Coord]) (calculate-basin-size mp low-point))
   (get-low-points mp)))

(: three-largest : (Listof Integer) -> (Vectorof Integer))
(define (three-largest basin-sizes)
  (foldl
   ; (BIGGEST 2NDBIGGEST 3RDBIGGEST)
   (lambda ([v : Integer] [biggest-so-far : (Vectorof Integer)])
     (cond
       [(> v (vector-ref biggest-so-far 0))
        (vector-immutable
         v
         (vector-ref biggest-so-far 0)
         (vector-ref biggest-so-far 1))]
       [(> v (vector-ref biggest-so-far 1))
        (vector-immutable
         (vector-ref biggest-so-far 0)
         v
         (vector-ref biggest-so-far 1))]
       [(> v (vector-ref biggest-so-far 2))
        (vector-immutable
         (vector-ref biggest-so-far 0)
         (vector-ref biggest-so-far 1)
         v)]
       [else biggest-so-far]))
   (list->vector
    (sort (take basin-sizes 3) <))
   (list-tail basin-sizes 3)))

(: mult-vector-elements : (Vectorof Integer) -> Integer)
(define (mult-vector-elements v)
  (vector-fold * 1 v))

  
(define mp (strings-to-vector list-in))

(display "PART 1\n")
(calculate-risk-levels mp)
(display "PART 2\n")
(mult-vector-elements (three-largest (calculate-all-basin-sizes mp)))