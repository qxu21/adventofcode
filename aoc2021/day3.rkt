#lang typed/racket

(require "library.rkt")
(require racket/fixnum)

(define bit-size 12) ; my input has 12-bit numbers
(define ns (integer-list-from-file-base (open-input-file "day3.txt") 2))
(define len (length ns))

(: count-ones : (Listof Integer) -> (Vectorof Integer))
(define (count-ones li)
  (foldl
   (lambda
       ([n : Integer]
        [v : (Vectorof Integer)])
     (build-vector
      bit-size
      (lambda ([i : Integer])
        (if
         (bitwise-bit-set? n i)
         (+ (vector-ref v i) 1)
         (vector-ref v i)))))
   (make-vector bit-size 0)
   li))

(: gamma : (Vectorof Integer) -> Natural)
(define (gamma v)
  (foldl
   (lambda
       ([i : Integer]
        [res : Natural])
     (if
      (> (vector-ref v i) (/ len 2))
      (bitwise-ior (arithmetic-shift res 1) 1)
      (arithmetic-shift res 1)))
   0
   (reverse (range bit-size)))) ; have to do this reverse due to endianness

(: ones : -> Natural)
(define (ones)
  (foldl
   (lambda
       ([i : Integer]
        [res : Natural])
     (bitwise-ior (arithmetic-shift res 1) 1))
   0
   (range bit-size)))

(define input-gamma
  (gamma
   (count-ones ns)))
(define epsilon
  (bitwise-and
   (bitwise-not input-gamma)
   (ones)))
(display "PART 1\nGAMMA: ")
(display input-gamma)
(display "\nEPSILON: ")
(display epsilon)
(display "\nGAMMA * EPSILON: ")
(* input-gamma epsilon)

                   