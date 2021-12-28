#lang typed/racket

;; ADVENT OF CODE 2021
;; qxu21
;; DAY 5

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

(: spawn-for-n-turns : (Listof Integer) Integer -> (Listof Integer))
(define (spawn-for-n-turns li turns-left)
  (let*
      ([new-spawns
        (foldl
         (lambda ([fish : Integer] [spawns : Integer])
           (if (= fish 0)
               (+ spawns 1)
               spawns))
         0 li)])
    (if
     (= turns-left 0)
     li
     (spawn-for-n-turns
      (append
       (map
        (lambda ([fish : Integer])
          (if (= fish 0) 6 (- fish 1)))
        li)
       (make-list new-spawns 8))
      (- turns-left 1)))))

(define start (read-in-lanternfish "day6.txt"))
(display "PART 1 (should be 391888)\n")
(length (spawn-for-n-turns start 80))
(display "PART 2\n")
(length (spawn-for-n-turns start 256))
