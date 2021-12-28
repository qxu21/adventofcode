#lang typed/racket

;; ADVENT OF CODE 2021
;; qxu21

(require typed/test-engine/racket-tests)

(: list-from-file : Input-Port -> (Listof String))
(define (list-from-file in)
  (for/list ([l (in-lines in)]) l))

(: string->integer-base : String Integer -> Integer)
(define (string->integer-base s b)
  (let
        ([n (string->number s b)])
      (if
       (and n (exact-integer? n))
       n
       (raise "Non-integer in input!"))))

(: string->integer : String -> Integer)
(define (string->integer s)
  (string->integer-base s 10))
  
(: integer-list-from-file-base : Input-Port Integer -> (Listof Integer))
(define (integer-list-from-file-base in b)
  (map
   (lambda ([s : String])
     (string->integer-base s b))
   (list-from-file in)))

(: integer-list-from-file : Input-Port -> (Listof Integer))
(define (integer-list-from-file in)
  (integer-list-from-file-base in 10))

(: vector-fold : (All (A B) (A B -> B) B (Vectorof A) -> B))
(define (vector-fold fn init vec)
  (vector-fold-aux fn init vec 0))

(: vector-fold-aux : (All (A B) (A B -> B) B (Vectorof A) Natural -> B))
(define (vector-fold-aux fn acc vec i)
  (if
   (= i (vector-length vec))
   acc
   (vector-fold-aux
    fn
    (fn (vector-ref vec i) acc)
    vec
    (+ i 1))))

(define test-vec '#(1 2 3 4 5))
(check-expect (vector-fold + 1 test-vec) 16)
(check-expect (vector-fold * 2 test-vec) 240)

(provide
 list-from-file
 string->integer
 integer-list-from-file
 integer-list-from-file-base
 vector-fold)