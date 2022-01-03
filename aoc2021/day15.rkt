#lang typed/racket

;; ADVENT OF CODE 2021
;; qxu21
;; DAY 15

(require "library.rkt")
(require (prefix-in bqueue: pfds/queue/bankers)) ; i'm surprised queue isn't a builtin

(define-type Int-Map (2-Map Integer))

(define-struct D-Map
  ([mp : Int-Map]
   [visited : (Setof Coord)]
   [frontier : (bqueue:Queue Coord)]))

(define inlist
  (list-from-file
   (open-input-file "day15.txt")))

(define SIDE (string-length (first inlist)))
(define AREA (expt SIDE 2))

(: risk-mp : Int-Map)
(define risk-mp (strings-to-2-Map inlist))

(: get-neighbor-coords : Coord Integer -> (Listof Coord))
(define (get-neighbor-coords co side)
  (match co
    [(Coord x y)
     (let
         ([x0? (= x 0)]
          [y0? (= y 0)]
          [xside? (= x (sub1 side))]
          [yside? (= y (sub1 side))])
       (append
        (if x0? '() (list (make-Coord (sub1 x) y)))                       ; left
        (if y0? '() (list (make-Coord x (sub1 y))))                       ; up
        (if xside? '() (list (make-Coord (add1 x) y)))                    ; right
        (if yside? '() (list (make-Coord x (add1 y))))                    ; down
        ))]))

(: get-unvisited-neighbor-coords : D-Map Coord Integer -> (Listof Coord))
(define (get-unvisited-neighbor-coords dmp co side)
  (filter
   (lambda ([c : Coord]) (not (set-member? (D-Map-visited dmp) co)))
   (get-neighbor-coords co side)))
   
  
; so this is basically asking for an implementation of dijkstra's algorithm.
; i've decided that looking that up and implementing it is a better idea than
; trying to remember it myself

(: dijkstra : D-Map Int-Map -> Integer)
(define (dijkstra dmp risk)
  (let*
      ([side (2-Map-side risk)]
       [bottom-right (make-Coord (sub1 side) (sub1 side))])
   (if
    (set-member? (D-Map-visited dmp) bottom-right)
    (get-on-map (D-Map-mp dmp) bottom-right)
    (dijkstra (visit-node risk dmp) risk))))
     

(: build-d-map : Integer -> D-Map)
(define (build-d-map side)
  (make-D-Map
   (set-on-map
    (make-2-Map
     ; (add1 (* (expt side 2) 9)) will be greater than the cost to trek
     ; through an entire map of 9s
     (make-vector (expt side 2) (add1 (* (expt side 2) 9)))
     side)
    (make-Coord 0 0)
    0)  
   (set)
   (bqueue:queue (make-Coord 0 0)))) ;seed first node on the frontier

; step 3
(: visit-node : Int-Map D-Map -> D-Map)
(define (visit-node risk dmp)
  (let*
      ([init-co (bqueue:head (D-Map-frontier dmp))]
       [new-frontier (get-unvisited-neighbor-coords dmp init-co (2-Map-side risk))])
    (make-D-Map
     (foldl
      (lambda ([co : Coord] [cost : Int-Map])
        (let
            ([cost-thru-me
              (+ (get-on-map risk co)
                 (get-on-map cost init-co))])
          (if
           ; if the cost to get there through here
           ; is less than the marked cost there
           (< cost-thru-me (get-on-map cost co))
           (set-on-map cost co cost-thru-me)
           cost)))
      (D-Map-mp dmp)
      new-frontier)
     ; add this coord to visited
     (set-add (D-Map-visited dmp) init-co)
     ; and remove it from the frontier
     (foldl (inst bqueue:enqueue Coord) (bqueue:tail (D-Map-frontier dmp)) new-frontier))))

(displayln "PART 1")
(dijkstra (build-d-map (2-Map-side risk-mp)) risk-mp)
   
                       