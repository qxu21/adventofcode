#lang typed/racket

;; ADVENT OF CODE 2021
;; qxu21
;; FAILED SOLUTION TO DAY 13

;; observe, an hour of going down the wrong path

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

(define-struct Paper
  ([mp : (Vectorof Boolean)]
   [x : Integer]
   [y : Integer]))

(: get-on-paper : Paper Coord -> Boolean)
(define (get-on-paper pa co)
  (vector-ref
   (Paper-mp pa)
   (+ (Coord-x co) (* (Paper-x pa) (Coord-y co)))))

;(: set-on-map : Paper Coord Boolean -> Paper)
;(define (set-on-map pa co v)
;  (let
;      ([co-i (+ (* SIDE (Coord-y co)) (Coord-x co))])
;    (build-vector
;     AREA
;     (lambda ([i : Integer])
;       (if (= i co-i) v (vector-ref mp i))))))

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

(: build-paper-f : (Listof Coord) -> Paper)
(define (build-paper-f li)
  (let
      ([xlim (apply max (map Coord-x li))]
       [ylim (apply max (map Coord-y li))])
    (make-Paper
     (foldl
      (lambda ([c : Coord] [mp : (Vectorof Boolean)])
        (build-vector
         (vector-length mp)
         (lambda ([i : Integer])
           (call-with-values
            (lambda () (quotient/remainder i xlim))
            (lambda ([i-y : Integer] [i-x : Integer])
              (if (equal? c (make-Coord i-x i-y)) #t (vector-ref mp i)))))))
      (ann (make-vector (* xlim ylim) #f) (Vectorof Boolean))
      li)
     xlim ylim)))

(: build-paper : (Listof Coord) -> Paper)
(define (build-paper li)
  (let*
      ([xlim (add1 (apply max (map Coord-x li)))]
       [ylim (add1 (apply max (map Coord-y li)))]
       [v : (Vectorof Boolean) (make-vector (* xlim ylim) #f)])
    (make-Paper
     (begin
       (for-each
        (lambda ([c : Coord])
            (vector-set! v (+ (Coord-x c) (* xlim (Coord-y c))) #t))
        li)
       v)
     xlim ylim)))

(: i-to-coord : Integer Integer -> Coord)
(define (i-to-coord xlim i)
  (make-Coord
   (quotient i xlim)
   (remainder i xlim)))

; let's assume all folds are in half

; fold like a book
(: book-fold : Paper -> Paper)
(define (book-fold pa)
  (match pa
    [(Paper init-mp init-xlim ylim)
     (let*
         ([xlim (quotient init-xlim 2)])
   (make-Paper
    (build-vector
     (* xlim ylim)
     (lambda ([i : Integer])
       (let
           ([c (i-to-coord xlim i)])
         (or
          (get-on-paper pa c)
          (get-on-paper
           pa
           (make-Coord (Coord-x c) (- init-xlim (Coord-y c) 1)))))))
    xlim ylim))]))

(: count-dots : Paper -> Integer)
(define (count-dots pa) (vector-count identity (Paper-mp pa)))
  

(define coordsli
  (strings-to-coords
   (list-from-file
    (open-input-file "day13.txt"))))

(define init-paper (build-paper coordsli))
(count-dots (book-fold init-paper))