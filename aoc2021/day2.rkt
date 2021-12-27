#lang typed/racket

;; ADVENT OF CODE 2021
;; qxu21
;; DAY 2

(: list-from-file : Input-Port -> (Listof String))
(define (list-from-file in)
  (for/list ([l (in-lines in)]) l))

(define-type Direction (U 'Forward 'Up 'Down))

(define-struct Instruction
  ([dir : Direction]
   [q : Integer]))

(: string->natural : String -> Natural)
(define (string->natural s)
  (let
        ([n (string->number s)])
      (if
       (and n (exact-nonnegative-integer? n))
       n
       (raise "Non-natural number in input!"))))

(: string->integer : String -> Integer)
(define (string->integer s)
  (let
        ([n (string->number s)])
      (if
       (and n (exact-integer? n))
       n
       (raise "Non-integer in input!"))))

(: strings-to-instructions : (Listof String) -> (Listof Instruction))
(define (strings-to-instructions li)
  (map
   (lambda ([s : String])
     (let
         ([spl (string-split s)]) ; split on whitespace by default
       (make-Instruction
        (match (first spl)
          ["forward" 'Forward]
          ["down" 'Down]
          ["up" 'Up])
        (string->integer (second spl)))))
   li))

; first number is x, forward progress
; second number is y, depth
(: instructions-to-final : (Listof Instruction) -> (Pairof Integer Integer))
(define (instructions-to-final li)
  (foldl
   (lambda
       ([i : Instruction]
        [co : (Pairof Integer Integer)])
     (cons
      (match i
        [(Instruction 'Forward n) (+ (car co) n)]
        [_ (car co)])
      (match i
        [(Instruction 'Up n) (- (cdr co) n)]
        [(Instruction 'Down n) (+ (cdr co) n)]
        [_ (cdr co)])))
   (cons 0 0)
   li))

(define instructions-list
  (strings-to-instructions
    (list-from-file
     (open-input-file "day2.txt"))))

(define final-coords
  (instructions-to-final instructions-list))

(display "PART 1\n")
(* (car final-coords) (cdr final-coords))

; i'm bad at struct names
(define-struct Sub-Helm
  ([x : Integer]
   [y : Integer]
   [aim : Integer]))

; first number is x, forward progress
; second number is y, depth
(: aim-instructions-to-final : (Listof Instruction) -> Sub-Helm)
(define (aim-instructions-to-final li)
  (foldl
   (lambda
       ([i : Instruction]
        [h : Sub-Helm])
     (make-Sub-Helm
      (match i
        [(Instruction 'Forward n) (+ (Sub-Helm-x h) n)]
        [_ (Sub-Helm-x h)])
      (match i
        [(Instruction 'Forward n) (+ (* (Sub-Helm-aim h) n) (Sub-Helm-y h))]
        [_ (Sub-Helm-y h)])
      (match i
        [(Instruction 'Up n) (- (Sub-Helm-aim h) n)]
        [(Instruction 'Down n) (+ (Sub-Helm-aim h) n)]
        [_ (Sub-Helm-aim h)])))
   (make-Sub-Helm 0 0 0)
   li))

(display "PART 2\n")
(define final-aim (aim-instructions-to-final instructions-list))
(* (Sub-Helm-x final-aim) (Sub-Helm-y final-aim))