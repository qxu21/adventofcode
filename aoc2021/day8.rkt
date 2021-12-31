#lang typed/racket

;; ADVENT OF CODE 2021
;; qxu21
;; DAY 8

(require "library.rkt")

(define-struct Combo-S
  ([digits : (Listof String)]
   [output : (Listof String)]))

(: list-to-combos-s : (Listof String) -> (Listof Combo-S))
(define (list-to-combos-s li)
  (let
      ([spls
        (map
         (lambda ([line : String])
           (string-split line " | "))
         li)])
    (map
     (lambda ([spl : (Listof String)])
       (make-Combo-S
        (string-split (first spl))
        (string-split (second spl))))
     spls)))

(: show-combo-s : Combo-S -> Void)
(define (show-combo-s c)
  (begin
    (display "(Digits: ")
    (for ([d (Combo-S-digits c)])
      (display d)
      (display " "))
    (display "Output: ")
    (for ([d (Combo-S-output c)])
      (display d)
      (display " "))
    (display ")\n")))

(define in-lines
  (list-from-file
    (open-input-file "day8.txt")))

(define combos-s (list-to-combos-s in-lines))

(: count-1478-in-outputs : (Listof Combo-S) -> Integer)
(define (count-1478-in-outputs li)
  (foldl
   (lambda
       ([c : Combo-S] [count : Integer])
     (+
      (foldl
       (lambda ([o : String] [n : Integer])
         ;           n of segments for:  1 4 7 8
         (if (member (string-length o) '(2 4 3 7)) (+ n 1) n))
       0 (Combo-S-output c)) count))
   0 li))

(display "PART 1\n")
(count-1478-in-outputs combos-s)

; OBSERVATIONS
; 1 is 2 long, contains c,f
; 4 is 4 long, containc c,f,b,d
; 7 is 3 long, contains c,f,a
; 8 is 7 long, contains all seven (useless)

;                                                                                 IDENTIFIED
; [find-1478] numbers identified by unique seg count (method given by puzzle)     1,4,7,8
; the segment in 7 that is not in 4 is a                                          a
; 1 can identify c and f -> c/f
; 4 can identify b and d by subtracting c/f -> b/d
; out of the six-segment numbers 6, 9, 0, they will all share one of c/f,
;   and one of them will be missing the other one of c/f.
;   that number is 6, and the missing segment is c, the shared segment is f       6,c,f
; find the six-segment value that is missing one of b or d.
;   this value is 0, the missing segment is d, the segment is has is b            0,b,d
; find the five-segment value that is a complete subset of 6.
;   this value is 5, and the segment it lacks is e                                5,e
; identify g by elimination                                                       g

(define-type Seg (U 'a 'b 'c 'd 'e 'f 'g))
(define-type Digit (Setof Seg))

(define-struct Combo
  ([digits : (Listof Digit)]
   [output : (Listof Digit)]))

(: char-to-seg : Char -> Seg)
(define (char-to-seg c)
  (match c
    [#\a 'a]
    [#\b 'b]
    [#\c 'c]
    [#\d 'd]
    [#\e 'e]
    [#\f 'f]
    [#\g 'g]
    [_ (raise "invalid character passed")]))

(: string-to-digits : String -> Digit)
(define (string-to-digits s)
  (list->set (map char-to-seg (string->list s))))

(: list-to-combos : (Listof String) -> (Listof Combo))
(define (list-to-combos li)
  (let
      ([spls
        (map
         (lambda ([line : String])
           (string-split line " | "))
         li)])
    (map
     (lambda ([spl : (Listof String)])
       (make-Combo
        (map string-to-digits (string-split (first spl)))
        (map string-to-digits (string-split (second spl)))))
     spls)))

(: show-combo : Combo -> Void)
(define (show-combo c)
  (begin
    (display "(Digits: ")
    (for ([d (Combo-digits c)])
      (display d)
      (display " "))
    (display "Output: ")
    (for ([d (Combo-output c)])
      (display d)
      (display " "))
    (display ")\n")))


(define-type Decrypt-Key (HashTable Digit Integer))
(define-type Encrypt-Key (HashTable Integer Digit))
(define-type Seg-Map     (HashTable Seg Seg))

(define-struct Decrypt-Tools
  ([dkey : Decrypt-Key]
   [ekey : Encrypt-Key]
   [mp  : Seg-Map]))

(: show-decrypt-tools : Decrypt-Tools -> Void)
(define (show-decrypt-tools tools)
  (begin
    (display "-- DECRYPTION TOOLS --\n")
    (display "DECRYPTION KEY: ")
    (displayln (Decrypt-Tools-dkey tools))
    (display "ENCRYPTION KEY: ")
    (displayln (Decrypt-Tools-ekey tools))
    (display "SEGMENT MAP: ")
    (displayln (Decrypt-Tools-mp tools))
    (display "----------------------")))
    

;(: build-decrypt-key : Combo -> Decrypt-Key)
;(define (build-decrypt-key c)
;  (let*
;      ([digits (Combo-digits c)])
;   (find-1478 digits)))

; set both encrypt and decrypt keys
(: set-tools : Decrypt-Tools Digit Integer -> Decrypt-Tools)
(define (set-tools tools digit i)
  (match tools
    [(Decrypt-Tools dkey ekey mp)
     (make-Decrypt-Tools
      (hash-set dkey digit i)
      (hash-set ekey i digit)
      mp)]))

(: set-tools-map : Decrypt-Tools Seg Seg -> Decrypt-Tools)
(define (set-tools-map tools from to)
  (match tools
    [(Decrypt-Tools dkey ekey mp)
     (make-Decrypt-Tools
      dkey
      ekey
      (hash-set mp from to))]))

; create a new hashtable, with 1,4,7,8 found and set
(: find-1478 : (Listof Digit) -> Decrypt-Tools)
(define (find-1478 li)
  (foldl
   (lambda
       ([digit : Digit]
        [tools : Decrypt-Tools])
     (match (set-count digit)
       [2 (set-tools tools digit 1)]
       [4 (set-tools tools digit 4)]
       [3 (set-tools tools digit 7)]
       [7 (set-tools tools digit 8)]
       [_ tools]))
   (make-Decrypt-Tools
    (hash)
    (hash)
    (hash))
   li))

(: decrypt-digit : Decrypt-Tools Digit -> Integer)
(define (decrypt-digit tools digit)
  (hash-ref (Decrypt-Tools-dkey tools) digit))

(: encrypt-int : Decrypt-Tools Integer -> Digit)
(define (encrypt-int tools i)
  (hash-ref (Decrypt-Tools-ekey tools) i))

; the segment in 7 that is not in 4 is a
(: identify-a : Decrypt-Tools -> Decrypt-Tools)
(define (identify-a tools)
  (set-tools-map
   tools
   (set-first ; key: first (and only) value of subtracting 4 from 7
     (set-subtract (encrypt-int tools 7) (encrypt-int tools 4)))
   'a))

; 1 can identify c and f -> c/f
; out of the six-segment numbers 6, 9, 0, they will all share one of c/f,
;   and one of them will be missing the other one of c/f.
;   that number is 6, and the missing segment is c, the shared segment is f
(: identify-6cf : (Listof Digit) Decrypt-Tools -> Decrypt-Tools)
(define (identify-6cf digits tools)
  (let*
      ([one (hash-ref (Decrypt-Tools-ekey tools) 1)]
       [six
        (match
            (findf
             ; where the intersect is only one seg rather than two
             (lambda ([digit : Digit])
               (= (set-count (set-intersect one digit)) 1))
             ; only over the 6-segment digits
             (filter
              (lambda ([digit : Digit])
                (= (set-count digit) 6))
              digits))
          [#f (raise "could not find 6")]
          [x x])]
       [f (set-first (set-intersect one six))]
       [c (set-first (set-remove one f))])
    (set-tools-map
     (set-tools-map
      (set-tools tools six 6)
      f 'f)
     c 'c)))

; find the five-segment value that is a complete subset of 6.
;   this value is 5, and the segment it lacks is e                                5,e

(: identify-5e : (Listof Digit) Decrypt-Tools -> Decrypt-Tools)
(define (identify-5e digits tools)
  (let*
      ([six (encrypt-int tools 6)]
       [five
        (match
            (findf
             ; is digit a subset of 6?
             (lambda ([digit : Digit])
               (subset? digit six))
             ; only over the 5-segment digits
             (filter
              (lambda ([digit : Digit])
                (= (set-count digit) 5))
              digits))
          [#f (raise "could not find 5")]
          [x x])])
    (set-tools-map
     (set-tools tools five 5)
     (set-first (set-subtract six five))
     'e)))
  
; 4 can identify b and d by subtracting c/f -> b/d
; find the six-segment value that is missing one of b or d.
;   this value is 0, the missing segment is d, the segment it has is b            0,b,d
(: identify-0bd : (Listof Digit) Decrypt-Tools -> Decrypt-Tools)
(define (identify-0bd digits tools)
  (let*
      ; grab bd by subtracting 1's segs from 4
      ([bd
        (set-subtract
         (encrypt-int tools 4)
         (encrypt-int tools 1))]
       [zero
        (match
            (findf
             ; where the intersect is only one seg rather than two
             (lambda ([digit : Digit])
               (= (set-count (set-intersect bd digit)) 1))
             ; only over the 6-segment digits
             (filter
              (lambda ([digit : Digit])
                (= (set-count digit) 6))
              digits))
          [#f (raise "could not find 0")]
          [x x])]
       ; the segment it has is b, the missing segment is d
       [b (set-first (set-intersect bd zero))]
       [d (set-first (set-remove bd b))])
    (set-tools-map
     (set-tools-map
      (set-tools tools zero 0)
      b 'b)
     d 'd)))

(define ALL-SEGS (set 'a 'b 'c 'd 'e 'f 'g))

; identify g by elimination
(: identify-g : Decrypt-Tools -> Decrypt-Tools)
(define (identify-g tools)
  (set-tools-map
   tools
   (set-first
    (set-subtract
     ALL-SEGS
     (list->set (hash-keys (Decrypt-Tools-mp tools)))))
   'g))

(: fill-in-239 : (Listof Digit) Decrypt-Tools -> Decrypt-Tools)
(define (fill-in-239 digits tools)
  (foldl
   (lambda ([d : Digit] [t : Decrypt-Tools])
     (if
      (hash-has-key? (Decrypt-Tools-dkey t) d)
      t
      (let*
          ([decrypted-digit : Digit
            (list->set
             (set-map
              d
              (lambda ([se : Seg])
                (hash-ref (Decrypt-Tools-mp t) se))))]
           [true-val : Integer
            (cond
              [(equal? decrypted-digit (set 'a 'c 'd 'e 'g)) 2]
              [(equal? decrypted-digit (set 'a 'c 'd 'f 'g)) 3]
              [(equal? decrypted-digit (set 'a 'b 'c 'd 'f 'g)) 9]
              [else (raise "couldn't associate with 2,3,9")])])
        (set-tools t d true-val))))
   tools
   digits))
     
(: decrypt-output : (Listof Digit) Decrypt-Tools -> Integer)
(define (decrypt-output output tools)
  ; foldr's moment of glory
  (foldl
   (lambda ([d : Digit] [acc : Integer])
     (+
      (* acc 10)
      (hash-ref (Decrypt-Tools-dkey tools) d)))
   0 output))

(: decrypt-combo : Combo -> Integer)
(define (decrypt-combo c)
  (match c
    [(Combo digits output)
     ; this is gonna look horrendous if i don't use let*
     ; sorry racket
     (let*
         ([1478-tools (find-1478 digits)]
          [a-tools (identify-a 1478-tools)]
          [6cf-tools (identify-6cf digits a-tools)]
          [5e-tools (identify-5e digits 6cf-tools)]
          [0bd-tools (identify-0bd digits 5e-tools)]
          [g-tools (identify-g 0bd-tools)]
          [full-tools (fill-in-239 digits g-tools)])
       (decrypt-output output full-tools))]))

(: sum-outputs : (Listof Combo) -> Integer)
(define (sum-outputs li)
  (foldl
   (lambda ([c : Combo] [acc : Integer])
     (+ acc (decrypt-combo c)))
   0 li))
                        
         

(define combos (list-to-combos in-lines))
;(define demo-combo (Combo-digits (sixth combos)))
;(define 1478-tools (find-1478 demo-combo))
;(show-decrypt-tools 1478-tools)
;(define 1478a-tools (identify-a 1478-tools))
;(show-decrypt-tools 1478a-tools)
;(define 1478a6cf-tools (identify-6cf demo-combo 1478a-tools))
;(show-decrypt-tools 1478a6cf-tools)
;;(define 1478a6cf5e-tools (identify-5e demo-combo 1478a6cf-tools))
;(show-decrypt-tools 1478a6cf5e-tools)
;(define 0bd-tools (identify-0bd demo-combo 1478a6cf5e-tools))
;(show-decrypt-tools 0bd-tools)
;(define g-tools (identify-g 0bd-tools))
;(show-decrypt-tools g-tools)
;(define full-tools (fill-in-239 demo-combo g-tools))
;(show-decrypt-tools full-tools)
;(decrypt-output (Combo-output (sixth combos)) full-tools)

(display "PART 2\n")
;(define demo-combo (sixth combos))
;(decrypt-combo demo-combo)

(sum-outputs combos)
   