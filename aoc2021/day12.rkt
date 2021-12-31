#lang typed/racket

;; ADVENT OF CODE 2021
;; qxu21
;; DAY 11

(require "library.rkt")

(define-type Cave-Map (HashTable String (Listof String)))

(: add-one-way-connection : Cave-Map String String -> Cave-Map)
(define (add-one-way-connection mp start end)
  (hash-set mp start (cons end (if (hash-has-key? mp start) (hash-ref mp start) '()))))
  

(: add-connection : Cave-Map String String -> Cave-Map)
(define (add-connection mp start end)
  (cond
   [(or
     (string=? start "start")
     (string=? end "end"))
    (add-one-way-connection mp start end)]
   [(or
     (string=? end "start")
     (string=? start "end"))
    (add-one-way-connection mp end start)]
   [else
    (add-one-way-connection
     (add-one-way-connection mp start end)
     end start)]))

(: strings-to-cave-map : (Listof String) -> Cave-Map)
(define (strings-to-cave-map li)
  (let
      ([empty-map : Cave-Map (hash)])
    (foldl
     (lambda
         ([s : String] [mp : Cave-Map])
       (let
           ([endpoints (string-split s "-")])
         (add-connection mp (first endpoints) (second endpoints))))
     empty-map
     li)))

; a path will be organized (cons end (cons 2nd-to-last (cons-3rd-to-last ...)))
; in other words, right to left
(define-type Path (Listof String))

(: big-cave? : String -> Boolean)
(define (big-cave? cave)
  (char-upper-case? (string-ref cave 0)))


; start with (("start")) - one path with one step
(: all-paths-through-map : Cave-Map (Cave-Map (Listof Path) -> (Listof Path)) -> (Listof Path))
(define (all-paths-through-map mp adver)
  (aptm-aux mp adver (list (list "start"))))

(: aptm-aux : Cave-Map (Cave-Map (Listof Path) -> (Listof Path)) (Listof Path) -> (Listof Path))
(define (aptm-aux mp adver paths)
  (if
   (all-paths-finished? paths)
   paths
   (aptm-aux mp adver (adver mp paths))))

(: all-paths-finished? : (Listof Path) -> Boolean)
(define (all-paths-finished? paths)
  (andmap
   (lambda ([p : Path]) (string=? (first p) "end"))
   paths))

; advance all paths by one step if they haven't ended
(: advance-paths-1 : Cave-Map (Listof Path) -> (Listof Path))
(define (advance-paths-1 mp paths)
  (apply
   append
   ; path -> list of new paths
   (map
    (lambda ([p : Path])
      (if
       (string=? (first p) "end")
       (list p)
       (map
        (lambda ([valid-cave : String])
          (cons valid-cave p))
        ; if there's no valid way to the end,
        ; this filter will go to '()
        (filter
         ; either a big cave, or not visited before
         (lambda ([cave : String])
           (or
            (big-cave? cave)
            (not (member cave p))))
         (hash-ref mp (first p))))))
    paths)))

; has one small cave been visited twice?
(: small-2x? : Path -> Boolean)
(define (small-2x? p)
  ; this is cursed but (if ... #t #f) would actually give me an aneurysm
  (not
   (not
    (check-duplicates
     (filter (lambda ([c : String]) (not (big-cave? c))) p)))))

; advance all paths by one step if they haven't ended
(: advance-paths-2 : Cave-Map (Listof Path) -> (Listof Path))
(define (advance-paths-2 mp paths)
  (apply
   append
   ; path -> list of new paths
   (map
    (lambda ([p : Path])
      (if
       (string=? (first p) "end")
       (list p)
       (map
        (lambda ([valid-cave : String])
          (cons valid-cave p))
        ; if there's no valid way to the end,
        ; this filter will go to '()
        (filter
         ; new cave validity:
         ; either a big cave, or not visited before, or no duplicate small caves
         (lambda ([cave : String])
           (or
            (big-cave? cave)
            (not (member cave p))
            (not (small-2x? p)))) 
         (hash-ref mp (first p))))))
    paths)))
  

(define mp
  (strings-to-cave-map
   (list-from-file
    (open-input-file "day12.txt"))))

(displayln "PART 1")
(length (all-paths-through-map mp advance-paths-1))
(displayln "PART 2")
(length (all-paths-through-map mp advance-paths-2))