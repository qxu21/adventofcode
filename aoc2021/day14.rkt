#lang typed/racket

;; ADVENT OF CODE 2021
;; qxu21
;; DAY 14

(require "library.rkt")

(define-struct Instruction
  ([left : Char]
   [right : Char]
   [insert : Char]))

(define-type Instructions (HashTable (Pairof Char Char) Char))

(: list-to-instructions : (Listof String) -> Instructions)
(define (list-to-instructions li)
  (foldl
   (lambda ([s : String] [ins : Instructions])
     (let
         ([cs (string->list s)])
       ; AB -> C
       ; 0123456
       (hash-set
        ins
        (cons (list-ref cs 0) (list-ref cs 1))
        (list-ref cs 6))))
   (ann (hash) Instructions)
   li))

(: polymerize-1 : (Listof Char) Instructions -> (Listof Char))
(define (polymerize-1 polymer ins)
  (match polymer
    ['() '()]
    [(cons a '()) (cons a '())]
    [(cons a (cons b tail))
     (append
      (list a)
      (polymerize-pair-1 (cons a b) ins)
      (polymerize-1 (cons b tail) ins))]))

(: polymerize-pair-1 : (Pairof Char Char) Instructions -> (Listof Char))
(define (polymerize-pair-1 p ins)
  (if
   (hash-has-key? ins p)
   (list (hash-ref ins p))
   '()))

; new struct for part 2
(define-type Poly2 (HashTable (Pairof Char Char) Integer))
(define-type Pair2 (Pairof (Pairof Char Char) Integer))

(: polymerize-2 : Poly2 Instructions -> Poly2)
(define (polymerize-2 polymer ins)
  (foldl
   (lambda ([pair : Pair2] [np : Poly2])
     (let
         ([insert (polymerize-pair-2 (car pair) ins)])
       (if
        insert
        (foldl
         (lambda ([p : Pair2] [acc : Poly2])
           (merge-into-hash acc (car p) (cdr p)))
         np
         ; the mitosis
         (list (cons (cons (caar pair) insert) (cdr pair))
               (cons (cons insert (cdar pair)) (cdr pair))))
        (merge-into-hash np (car pair) (cdr pair)))))
   (ann (hash) Poly2)
   (hash->list polymer)))
        

(: polymerize-pair-2 : (Pairof Char Char) Instructions -> (U Char #f))
(define (polymerize-pair-2 p ins)
  (if
   (hash-has-key? ins p)
   (hash-ref ins p)
   #f))

(: polymerize-n-times : (All (A) (A Instructions -> A)
                                       A Instructions Integer -> A))
(define (polymerize-n-times fn polymer ins n)
  (foldl
   (lambda ([_ : Integer] [p : A]) (fn p ins))
   polymer
   (range n)))

; count(most common element) - count(least common element)
(: score-1 : (Listof Char) -> Integer)
(define (score-1 li)
  (let
      ([freqs (histogram li)])
    (- (apply max (hash-values freqs)) (apply min (hash-values freqs)))))

(: histogram-poly2 : Poly2 -> (HashTable Char Integer))
(define (histogram-poly2 poly)
  (foldl
   (lambda ([pair : Pair2] [acc : (HashTable Char Integer)])
     ; only count the first letter of the pair
     (merge-into-hash acc (caar pair) (cdr pair)))
   (ann (hash) (HashTable Char Integer))
   (hash->list poly)))

(: hash-range : (All (A) (HashTable A Integer) -> Integer))
(define (hash-range h)
  (- (apply max (hash-values h)) (apply min (hash-values h))))

(: make-poly2 : (Listof Char) -> Poly2)
(define (make-poly2 li)
  (match li
    ['() (hash)]
    [(cons final '()) (hash (cons final #\_) 1)]
    [(cons a (cons b rest))
     (merge-into-hash
      (make-poly2 (cons b rest))
      (cons a b)
      1)]))

(define listin
  (list-from-file
   (open-input-file "day14.txt")))

(define init-polymer (string->list (car listin)))
(define init-instructions (list-to-instructions (cddr listin)))

(displayln "PART 1")
(score-1 (polymerize-n-times polymerize-1 init-polymer init-instructions 10))
(displayln "VERIFYING PART 2 ALGORITHMS, SHOULD BE IDENTICAL TO PART 1")
(hash-range (histogram-poly2 (polymerize-n-times polymerize-2 (make-poly2 init-polymer) init-instructions 10)))
(displayln "PART 2")
(hash-range (histogram-poly2 (polymerize-n-times polymerize-2 (make-poly2 init-polymer) init-instructions 40)))
