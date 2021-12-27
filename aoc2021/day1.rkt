#lang typed/racket

;; ADVENT OF CODE 2021
;; qxu21
;; DAY 1 PART 1

(: list-from-file : Input-Port -> (Listof Natural))
(define (list-from-file in)
  (for/list ([l (in-lines in)])
    (let
        ([n (string->number l)])
      (if
       (and n (exact-nonnegative-integer? n))
       n
       (raise "Non-natural number in input!")))))

(: count-steps-down : (Listof Natural) -> Natural)
(define (count-steps-down li)
  (car
   (foldl
    ; accumulator is a (Pairof Natural Natural)
    ; where the first value is the number of steps down
    ; and the second is the previous value
    (lambda (
             [el : Natural]
             [acc : (Pairof Natural Natural)])
      (cons
       (if (> el (cdr acc))
           (+ (car acc) 1)
           (car acc))
       el))
    (cons 0 (car li)) ;start with +infinity
    (cdr li))))

; PART 1
(display "PART 1\n")
(count-steps-down
 (list-from-file
  (open-input-file "day1.txt")))

(define-struct 3-Acc
  ([n-1 : Natural]
   [n-2 : Natural]
   [last-sum : Natural]
   [a : Natural]))

(: count-steps-down-3 : (Listof Natural) -> Natural)
(define (count-steps-down-3 li)
  (3-Acc-a
   (foldl
    (lambda (
             [el : Natural]
             [acc : 3-Acc])
      (let
          ([new-sum
            (+
             (3-Acc-n-1 acc)
             (3-Acc-n-2 acc)
             el)])
        (make-3-Acc
         el                ; new n-1 is n
         (3-Acc-n-1 acc)   ; new n-2 is n-1
         new-sum
         (if (> new-sum (3-Acc-last-sum acc))
             (+ (3-Acc-a acc) 1)
             (3-Acc-a acc)))))
    ; [A] [A/B] [A/B] [B]
    (make-3-Acc
     (list-ref li 1)                               ; n-1 is the second element
     (list-ref li 2)                               ; n-2 is the third element
     (+ (car li) (list-ref li 1) (list-ref li 2))  ; the first window
     0)                                            ; start accumulating at 0
    (list-tail li 3))))

; PART 2
(display "PART 2\n")
(count-steps-down-3
 (list-from-file
  (open-input-file "day1.txt")))