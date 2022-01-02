#lang typed/racket

;; ADVENT OF CODE 2021
;; qxu21
;; DAY 13

(require "library.rkt")
(define SIDE 10)
(define AREA 100)

(define-struct Coord
  ([x : Integer]
   [y : Integer])
  #:transparent)

(: show-coord : Coord -> Void)
(define (show-coord p)
  (begin
    (display "( ")
    (display (Coord-x p))
    (display ", ")
    (display (Coord-y p))
    (display ") ")))

(: strings-to-coords : (Listof String) -> (Listof Coord))
(define (strings-to-coords li)
  (filter-map
   (lambda ([s : String])
     (and
      (string-contains? s ",")
      (let
          ([spl (map string->integer (string-split s ","))])
        (make-Coord
         (first spl)
         (second spl)))))
   li))

(: fold-over-x : (Listof Coord) Integer -> (Listof Coord))
(define (fold-over-x li crease)
  (remove-duplicates
   (map
    (lambda ([c : Coord])
      (if
       (< (Coord-x c) crease)
       c
       (make-Coord (- (* 2 crease) (Coord-x c)) (Coord-y c))))
    li)))

(: fold-over-y : (Listof Coord) Integer -> (Listof Coord))
(define (fold-over-y li crease)
  (remove-duplicates
   (map
    (lambda ([c : Coord])
      (if
       (< (Coord-y c) crease)
       c
       (make-Coord (Coord-x c) (- (* 2 crease) (Coord-y c)))))
    li)))

(define-struct Fold
  ([over-x? : Boolean]
   [crease : Integer]))

(: parse-folds : (Listof String) -> (Listof Fold))
(define (parse-folds li)
  (filter-map
   (lambda ([s : String])
     (and
      (string-contains? s "=")
      (make-Fold
       (string-contains? s "x")
       (string->integer (second (string-split s "="))))))
   li))

(: execute-folds : (Listof Coord) (Listof Fold) -> (Listof Coord))
(define (execute-folds init-coords folds)
  (foldl ; haha
   (lambda ([f : Fold] [p : (Listof Coord)])
     (if
      (Fold-over-x? f)
      (fold-over-x p (Fold-crease f))
      (fold-over-y p (Fold-crease f))))
   init-coords folds))

(: print-coords : (Listof Coord) -> Void)
(define (print-coords li)
  (let*
      ([xlim (add1 (apply max (map Coord-x li)))]
       [ylim (add1 (apply max (map Coord-y li)))])
    (for ([y (range ylim)])
      (begin
        (for ([x (range xlim)])
          (if
           (member (make-Coord x y) li)
           (display "#")
           (display ".")))
        (display "\n")))))


(define inlist
  (list-from-file
   (open-input-file "day13.txt")))

(define coords (strings-to-coords inlist))
(define folds (parse-folds inlist))

(displayln "PART 1")
(length (fold-over-x coords 655)) ; yes, hardcoded
(displayln "PART 2")
(print-coords (execute-folds coords folds))