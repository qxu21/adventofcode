#lang typed/racket

;; ADVENT OF CODE 2021
;; qxu21

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


(provide
 list-from-file
 string->integer
 integer-list-from-file
 integer-list-from-file-base)