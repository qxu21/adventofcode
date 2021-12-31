#lang typed/racket

;; ADVENT OF CODE 2021
;; qxu21
;; DAY 10

(require "library.rkt")

; 0 for nonillegal lines
(: line-illegality-score : (Listof Char) -> Integer)
(define (line-illegality-score li)
  (lis-aux li '()))

(: opener? : Char -> Boolean)
(define (opener? ch)
  (set-member? (set #\( #\[ #\{ #\<) ch))

(: score : Char -> Integer)
(define (score c)
  (match c
    [#\) 3]
    [#\] 57]
    [#\} 1197]
    [#\> 25137]))

(: closes? : Char Char -> Boolean)
(define (closes? open close)
  (match* (open close)
    [(#\( #\)) #t]
    [(#\[ #\]) #t]
    [(#\{ #\}) #t]
    [(#\< #\>) #t]
    [(_ _) #f]))

(: lis-aux : (Listof Char) (Listof Char) -> Integer)
(define (lis-aux li unclosed)
  (match li
    ['() 0]
    [(cons c tail)
     (if
      (opener? c)
      (lis-aux tail (cons c unclosed))
      (match unclosed
        ['() (raise "trying to close a fully closed line")]
        [(cons openc unclosedtail)
         (if
          (closes? openc c)
          (lis-aux tail unclosedtail)
          (score c))]))]))

(: sum-illegality-scores : (Listof String) -> Integer)
(define (sum-illegality-scores li)
  (foldl
   (lambda ([s : String] [scores : Integer])
     (+ scores (line-illegality-score (string->list s))))
   0 li))

; returns 0 for a corrupted line
; be sure to filter these from the result
(: completion-score : (Listof Char) -> Integer)
(define (completion-score li)
  (cs-aux li '()))

(: cs-aux : (Listof Char) (Listof Char) -> Integer)
(define (cs-aux li unclosed)
  (match li
    ['() (calc-completion-score unclosed)]
    [(cons c tail)
     (if
      (opener? c)
      (cs-aux tail (cons c unclosed))
      (match unclosed
        ['() (raise "trying to close a fully closed line")]
        [(cons openc unclosedtail)
         (if
          (closes? openc c)
          (cs-aux tail unclosedtail)
          0)]))]))

(: completion-for-char : Char -> Integer)
(define (completion-for-char c)
  (match c
    [#\( 1]
    [#\[ 2]
    [#\{ 3]
    [#\< 4]
    [_ (raise "malformed character")]))
     

(: calc-completion-score : (Listof Char) -> Integer)
(define (calc-completion-score li)
  (foldl
   (lambda ([c : Char] [score : Integer])
     (+ (completion-for-char c) (* 5 score)))
   0 li))

(: median-completion-score : (Listof String) -> Integer)
(define (median-completion-score li)
  (let
   ([scores
     (filter
      ; filter out zeroes
      (lambda ([i : Integer]) (not (= i 0)))
      ; from the sorted completion scores
      (sort
       (map
        completion-score
        (map string->list li))
       <))])
   ; hah, i thought i needed ceiling, but i needed floor
   (list-ref scores (floor (/ (length scores) 2)))))

(define in-lines
  (list-from-file
   (open-input-file "day10.txt")))
(display "PART 1\n")
(sum-illegality-scores in-lines)
(display "PART 2\n")
(median-completion-score in-lines)

     
      