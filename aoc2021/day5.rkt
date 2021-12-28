#lang typed/racket

;; ADVENT OF CODE 2021
;; qxu21
;; DAY 5

(require "library.rkt")

(define SIDE 1000)
(define AREA (expt SIDE 2))

(define-struct Point
  ([x : Integer] [y : Integer]))

(define-struct Line-Seg
  ([start : Point]
   [end : Point]))

(: list-to-line-segs : (Listof String) -> (Listof Line-Seg))
(define (list-to-line-segs li)
  (map
   (lambda ([s : String])
     (let*
         ([spl (string-split s)]
          [p1 (map string->integer (string-split (list-ref spl 0) ","))]
          [p2 (map string->integer (string-split (list-ref spl 2) ","))])
       (make-Line-Seg
        (make-Point (first p1) (second p1))
        (make-Point (first p2) (second p2)))))
   li))

(define line-segs
  (list-to-line-segs
   (list-from-file
    (open-input-file "day5.txt"))))

(: show-point : Point -> Void)
(define (show-point p)
  (begin
    (display "( ")
    (display (Point-x p))
    (display ", ")
    (display (Point-y p))
    (display ") ")))

(: show-line-segs : (Listof Line-Seg) -> Void)
(define (show-line-segs li)
  (foldl
   (lambda ([l : Line-Seg] [_ : Void])
     (begin
       (show-point (Line-Seg-start l))
       (display " -> ")
       (show-point (Line-Seg-end l))
       (display "\n")))
   (void)
   li))

(: horiz? : Line-Seg -> Boolean)
(define (horiz? l)
  (match l
    [(Line-Seg (Point x1 y1) (Point x2 y2))
     (= y1 y2)]))

(: vert? : Line-Seg -> Boolean)
(define (vert? l)
  (match l
    [(Line-Seg (Point x1 y1) (Point x2 y2))
     (= x1 x2)]))

(define-struct Location
  ([l : Point]
   [height : Integer]))

;(: in-seg? : Line-Seg Point -> Boolean)
;(define (in-seg? l p)
;  (match* (l p)
;    [((Line-Seg (Point x1 y1) (Point x2 y2)) (Point x y))
;     (and
;      (<= x (min x1 x2))
;      (

(: get-point : (All (A) Point (Mutable-Vectorof A) -> A))
(define (get-point p v)
  (vector-ref v (+ (Point-x p) (* (Point-y p) SIDE))))

(: set-point! : (All (A) Point (Mutable-Vectorof A) A -> Void))
(define (set-point! p v in)
  (vector-set! v (+ (Point-x p) (* (Point-y p) SIDE)) in))

(: inc-point! : Point (Mutable-Vectorof Integer) -> Void)
(define (inc-point! p v)
  (set-point! p v (+ (get-point p v) 1)))

(: draw-line-segs : (Listof Line-Seg) (Mutable-Vectorof Integer) Boolean -> Void)
(define (draw-line-segs line-segs board draw-diags?)
  (for ([seg line-segs])
    (match seg
      [(Line-Seg (Point x1 y1) (Point x2 y2))
       (cond
         [(= y1 y2) ; horizontal
          (for ([x (range (min x1 x2) (+ (max x1 x2) 1))])
            (inc-point! (make-Point x y1) board))]
         [(= x1 x2) ; vertical
          (for ([y (range (min y1 y2) (+ (max y1 y2) 1))])
            (inc-point! (make-Point x1 y) board))]
         [draw-diags?     ; diagonal
          (let
              ([ltr-seg   ; a seg that goes left to right
                (if
                 (> x1 x2)
                 (make-Line-Seg (make-Point x2 y2) (make-Point x1 y1))
                 seg)])
            (match ltr-seg
              [(Line-Seg (Point lx ly) (Point rx ry))
               (if
                (< ly ry)
                ; line goes up
                (for ([x (range lx (+ rx 1))]
                      [y (range ly (+ ry 1))])
                  (inc-point! (make-Point x y) board))
                ; line goes down
                (for ([x (range lx (+ rx 1))]
                      [y (range ly (- ry 1) -1)])
                  (inc-point! (make-Point x y) board)))]))]
         [else (void)])])))

(define straight-lines-board (make-vector AREA 0))
(draw-line-segs line-segs straight-lines-board #f)

(define all-lines-board (make-vector AREA 0))
(draw-line-segs line-segs all-lines-board #t)

(: count-greq-2 : (Vectorof Integer) -> Integer)
(define (count-greq-2 v)
  (vector-fold
   (lambda ([val : Integer] [acc : Integer])
     (if (>= val 2) (+ acc 1) acc))
   0
   v))

(display "PART 1 (should be 7644)\n")
(count-greq-2 straight-lines-board)
(display "\nPART 2\n")
(count-greq-2 all-lines-board)