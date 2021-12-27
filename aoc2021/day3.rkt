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

(: find-o2-co2 : (Listof Integer) Boolean -> Integer)
(define (find-o2-co2 li o2?)
  (let
      ([res (find-o2-co2-aux li (- bit-size 1) o2?)])
    (if
     (integer? res)
     res
     (begin
       (display res)
       (raise "aux never finished!")))))

(: find-o2-co2-aux : (Listof Integer) Integer Boolean -> (U Integer (Listof Integer)))
(define (find-o2-co2-aux li i o2?)
  (let*
      ([most-common
        (>=              ; in the event of a tie, make it 1
         (foldl
          (lambda
              ([v : Integer] [acc : Integer])
            (if
             (bitwise-bit-set? v i)
             (+ acc 1)
             acc))
          0
          li)
         (/ (length li) 2)
         )]
                
       [filtered
        (filter
         (lambda ([v : Integer])    ;predicate: is the ith bit set?
           (boolean=?
            (bitwise-bit-set? v i)
            (if o2? most-common (not most-common))))
         li)])
    (match filtered
      [(cons i '()) i]
      [_
       (if (= i 0)
           filtered
           (find-o2-co2-aux filtered (- i 1) o2?))])))

(display "\nPART 2\nO2: ")
(define o2 (find-o2-co2 ns #t))
(display o2)
(display "\nCO2: ")
(define co2 (find-o2-co2 ns #f))
(display co2)
(display "\nPRODUCT: ")
(* o2 co2)