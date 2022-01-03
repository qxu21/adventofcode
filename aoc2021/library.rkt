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

; DISTINCT FROM char->integer builtin
; will return garbage
(: char->int : Char -> Integer)
(define (char->int c)
  (- (char->integer c) 48))
  
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

(: strings-to-int-vector : (Listof String) -> (Vectorof Integer))
(define (strings-to-int-vector li)
  (list->vector
   (map
    char->int
    (apply append
     (map string->list li)))))

(define test-vec '#(1 2 3 4 5))
(check-expect (vector-fold + 1 test-vec) 16)
(check-expect (vector-fold * 2 test-vec) 240)

; foldl-i, feeding in the index
(: foldl-i : (All (A B) (A B Integer -> B) B (Listof A) -> B))
(define (foldl-i fn acc li)
  (foldl-i-aux fn acc li 0))


(: foldl-i-aux : (All (A B) (A B Integer -> B) B (Listof A) Integer -> B))
(define (foldl-i-aux fn acc li i)
  (match li
    ['() acc]
    [(cons head tail)
     (foldl-i-aux
      fn
      (fn head acc i)
      tail
      (+ i 1))]))

(check-expect
 (foldl-i
  (lambda ([a : Integer] [acc : Integer] [i : Integer])
    (+ a acc i))
  0
  '(0 1 2 3 4 5))
 30)
     

; special case, where the file is one line long
(: read-in-csv-numbers : String -> (Listof Integer))
(define (read-in-csv-numbers filename)
  (map string->integer
       (string-split
        (let
            ([line (read-line (open-input-file filename))])
          (if
           (eof-object? line)
           (raise "empty file")
           line))
          ",")))

(: show-square-vector : (All (A) (Vectorof A) Integer -> Void))
(define (show-square-vector v side)
  (for ([y (range side)])
    (begin
      (display "[")
      (for ([x (range side)])
        (begin
          (display (vector-ref v (+ (* side y) x)))
          (display " ")))
      (displayln "]"))))

; filter2 - use partition

(: histogram : (All (A) (Listof A) -> (HashTable A Integer)))
(define (histogram li)
  (foldl
   (lambda ([item : A] [table : (HashTable A Integer)])
     (if
      (hash-has-key? table item)
      (hash-set table item (add1 (hash-ref table item)))
      (hash-set table item 1)))
   (ann (hash) (HashTable A Integer))
   li))

; ONLY USE THIS IF YOU'RE SURE THERE ARE NO COLLISIONS
(: reverse-hash-table : (All (K V) (HashTable K V) -> (HashTable V K)))
(define (reverse-hash-table h)
  (if
   (check-duplicates (hash-values h))
   (raise "colliding list passed to reverse-hash-table")
   (foldl
    (lambda ([key : K] [nh : (HashTable V K)])
      (let
          ([val (hash-ref h key)])
        (hash-set nh val key)))
    (ann (hash) (HashTable V K))
    (hash-keys h))))

(: merge-into-hash : (All (K) (HashTable K Integer) K Integer -> (HashTable K Integer)))
(define (merge-into-hash h key count)
  (if
   (hash-has-key? h key)
   (hash-set h key (+ (hash-ref h key) count))
   (hash-set h key count)))

(provide
 list-from-file
 string->integer
 integer-list-from-file
 integer-list-from-file-base
 vector-fold
 read-in-csv-numbers
 foldl-i
 char->int
 strings-to-int-vector
 show-square-vector
 histogram
 merge-into-hash)