#lang typed/racket

;; ADVENT OF CODE 2021
;; qxu21
;; DAY 4

(require "library.rkt")

(define-struct Bingo-Num
  ([n : Integer]
   [called : Boolean]))

(define-struct Bingo
  ([calls : (Listof Integer)]
   [boards : (Listof (Vectorof Bingo-Num))]))

(define in (open-input-file "day4.txt"))
(define BOARD-SIZE 5)
(define BOARD-AREA 25)

(: setup-bingo : (Listof String) -> Bingo)
(define (setup-bingo li)
  (match li
    [(cons head tail)
     (make-Bingo
      (map string->integer (string-split head ","))
      (process-boards tail))]))

(: process-boards : (Listof String) -> (Listof (Vectorof Bingo-Num)))
(define (process-boards li)
  (match li
    ['() '()]
    [_
     (cons
      (vectorize-board (list-tail (take li 6) 1))
      (process-boards (list-tail li 6)))]))

(: create-bingo-num : Integer -> Bingo-Num)
(define (create-bingo-num i)
  (make-Bingo-Num i #f))

(: vectorize-board : (Listof String) -> (Vectorof Bingo-Num))
(define (vectorize-board ss)
  (list->vector (map create-bingo-num (map string->integer (apply append (map string-split ss))))))

(define game (setup-bingo (list-from-file in)))

(: board-folder : (Vectorof Bingo-Num) Integer Boolean -> Boolean)
(define (board-folder board bni res)
  (and res (Bingo-Num-called (vector-ref board bni))))

(: rows : -> (Listof (Listof Integer)))
(define (rows)
  (map
   (lambda ([i : Integer])
     (range (* i BOARD-SIZE) (* (+ i 1) BOARD-SIZE)))
   (range BOARD-SIZE)))

(: cols : -> (Listof (Listof Integer)))
(define (cols)
  (map
   (lambda ([i : Integer])
     (range i BOARD-AREA BOARD-SIZE))
   (range BOARD-SIZE)))

(: diags : -> (Listof (Listof Integer)))
(define (diags)
  (cons
   (map                       ; top left to bottom right
    (lambda ([i : Integer])
      (+ (* BOARD-SIZE i) i))
    (range BOARD-SIZE))
   (cons
    (map                       ; top right to bottom left
     (lambda ([i : Integer])
       (+ (* BOARD-SIZE i) (- BOARD-SIZE i 1)))
     (range BOARD-SIZE))
    '())))

; lines is a global variable
; removing diags
(: lines : (Listof (Listof Integer)))
(define lines (append (rows) (cols)))


(: test-board : (Vectorof Bingo-Num) -> Boolean)
(define (test-board board)
  (ormap
   (lambda ([line : (Listof Integer)])
     (andmap
      (lambda ([pos : Integer])
        (Bingo-Num-called (vector-ref board pos)))
      line))
   lines))

(: check-for-victory : (Listof (Vectorof Bingo-Num)) -> (U #f (Vectorof Bingo-Num)))
(define (check-for-victory boards)
  (match boards
    ['() #f]
    [(cons b rest)
     (or
      (if (test-board b) b #f)
      (check-for-victory rest))]))

(: mark-number : (Listof (Vectorof Bingo-Num)) Integer -> (Listof (Vectorof Bingo-Num)))
(define (mark-number boards n)
  (map
   (lambda ([board : (Vectorof Bingo-Num)])
     (vector-map
      (lambda ([bn : Bingo-Num])
        (if
         (= (Bingo-Num-n bn) n)
         (make-Bingo-Num n #t)
         bn))
      board))
   boards))

; gets the score BEFORE multiplying by the winning call
(: get-score : (Vectorof Bingo-Num) -> Integer)
(define (get-score board)
  (vector-fold
   (lambda
       ([bn : Bingo-Num]
        [acc : Integer])
     (match bn
       [(Bingo-Num n called)
        (if (not called)
            (+ acc n)
            acc)]))
   0
   board))

(: show-board : (Vectorof Bingo-Num) -> Void)
(define (show-board board)
  (foldl
   (lambda ([row : (Listof Bingo-Num)]
            [acc : Void])
     (begin
       (foldl
        (lambda ([bn : Bingo-Num]
                 [v : Void])
          (display
           (string-append
            "( "
            (number->string (Bingo-Num-n bn))
            " "
            (if (Bingo-Num-called bn) "#t" "#f")
            ") ")))
        (void)
        row)
       (display "\n")))
   (void)
   (map
    (lambda ([li : (Listof Integer)])
      (map
       (lambda ([i : Integer]) (vector-ref board i))
       li))
    (rows))))

(: show-all-boards : (Listof (Vectorof Bingo-Num)) -> Void)
(define (show-all-boards boards)
  (foldl
   (lambda
       ([board : (Vectorof Bingo-Num)]
        [v : Void])
     (show-board board))
   (void)
   boards))
      
  
(: play-bingo : Bingo -> Integer)
(define (play-bingo b)
  ; call a number
  (match b
    [(Bingo '() _) (raise "called all numbers, no wins")]
    [(Bingo calls boards)
     (let*
         ([call (first calls)]
          [new-b
           (make-Bingo
            (rest calls)
            (mark-number boards call))]
          [winner (check-for-victory (Bingo-boards new-b))])
           (if
            winner
            (begin
              (show-board winner)
              (* (get-score winner) call))
            (play-bingo new-b)))]))

(: play-to-lose : Bingo -> Integer)
(define (play-to-lose b)
  ; call a number
  (match b
    [(Bingo '() _) (raise "called all numbers, no wins")]
    [(Bingo calls boards)
     (let*
         ([call (first calls)]
          [marked-boards (mark-number boards call)]
          [unwon-boards
           (filter
            (lambda ([bo : (Vectorof Bingo-Num)]) (not (test-board bo)))
            marked-boards)]
          [new-b
           (make-Bingo
            (rest calls)
            unwon-boards)])
           (if
            (= (length unwon-boards) 0)
            (begin
              (display (length marked-boards))
              (display "\n")
              (display (length unwon-boards))
              (display "\n")
              (* (get-score (first marked-boards)) call)) ; assuming only one board makes it to the end
            (play-to-lose new-b)))]))

(display "PART 1\n")
(play-bingo game)
(display "\nPART 2\n")
(play-to-lose game)
  
  