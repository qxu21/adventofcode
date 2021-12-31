#lang typed/racket

;; ADVENT OF CODE 2021
;; qxu21
;; DAY 6

(require "library.rkt")

; special case, where the file is one line long
(: read-in-lanternfish : String -> (Listof Integer))
(define (read-in-lanternfish filename)
  (map string->integer
       (string-split
        (let
            ([line (read-line (open-input-file filename))])
          (if
           (eof-object? line)
           (raise "empty file")
           line))
          ",")))

(: add-fish-to-school : Integer (Listof (Pairof Integer Integer)) -> (Listof (Pairof Integer Integer)))
(define (add-fish-to-school fish school)
  (match school
    ['() (cons (cons fish 1) '())]
    [(cons p rest)
     (if
      (= (car p) fish)
      (cons
       (cons (car p) (+ (cdr p) 1))
       rest)
      (cons p (add-fish-to-school fish rest)))]))
         
; lanternfish - (DAYS QUANTITY)
(: condense-lanternfish : (Listof Integer) -> (Listof (Pairof Integer Integer)))
(define (condense-lanternfish li)
  (foldl add-fish-to-school '() li))

(: spawn-for-n-turns : (Listof (Pairof Integer Integer)) Integer -> (Listof (Pairof Integer Integer)))
(define (spawn-for-n-turns li turns-left)
  (let*
      ([new-spawns
        (foldl
         (lambda ([group : (Pairof Integer Integer)] [spawns : Integer])
           (if (= (car group) 0)
               (+ spawns (cdr group))
               spawns))
         0 li)]
       [new-list
        (map
        (lambda ([group : (Pairof Integer Integer)])
          (cons
           (if
            (= (car group) 0)
            6
            (- (car group) 1))
           (cdr group)))
        li)])
    (if
     (= turns-left 0)
     li
     (spawn-for-n-turns
      (if (= new-spawns 0)
          new-list
          (cons (cons 8 new-spawns) new-list))
      (- turns-left 1)))))

(: count-fish : (Listof (Pairof Integer Integer)) -> Integer)
(define (count-fish li)
  (foldl
   (lambda ([group : (Pairof Integer Integer)] [count : Integer])
     (+ count (cdr group)))
   0 li))

(define start-list (read-in-lanternfish "day6.txt"))
(define condensed-list (condense-lanternfish start-list))
(display "PART 1 (should be 391888)\n")
(count-fish (spawn-for-n-turns condensed-list 80))
(display "PART 2\n")
(count-fish (spawn-for-n-turns condensed-list 256))
