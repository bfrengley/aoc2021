#lang racket

(require "util.rkt")

(struct board (grid nums) #:mutable #:transparent)

(define (read-board in)
  ; read in the 5x5 grid of numbers into a vector of (mutable) vectors
  (define (read-grid)
    (for/vector #:length 5 ([row in])
      (list->vector (map string->number (string-split row)))))

  ; create a map of (n . (x . y)) pairs from the grid to save time locating numbers later
  (define (create-nums grid)
    (for*/hash ([x board-indexes]
                [y board-indexes])
      (values (vector-ref (vector-ref grid y) x) (cons x y))))

  (let ([grid (read-grid)])
    (board grid (create-nums grid))))

(define board-indexes '(0 1 2 3 4))

; get the value in the grid at (x, y)
(define (board-ref board x y)
  (vector-ref (vector-ref (board-grid board) y) x))

; get whether the grid position (x, y) has already been marked as drawn
(define (board-cell-marked? board x y)
  (not (board-ref board x y)))

; checks whether the board has been won based on the row/column/diagonal that (x, y) falls in
(define (board-won? board x y)
  (or
   ; all cells in the row are marked
   (for/and ([y- board-indexes])
     (board-cell-marked? board x y-))
   ; all cells in the column are marked
   (for/and ([x- board-indexes])
     (board-cell-marked? board x- y))
   (and
    ; this is on a diagonal
    (= x y)
    ; all cells in the diagonal are marked
    (for/and ([i board-indexes])
      (board-cell-marked? board i i)))))

; mark a number as drawn
; returns the board if marking the number completed it, and #f otherwise
(define (board-mark! board n)
  (let ([pos (hash-ref (board-nums board) n #f)]
        [grid (board-grid board)])
    (and
     ; if the board hasn't already won and doesn't contain n, it can't have won here
     pos
     ; it contained n, so mark the cell containing n and check if it won
     (match-let ([(cons x y) pos])
       ; delete the number from the grid for easier victory checking
       (vector-set! (vector-ref grid y) x #f)
       (board-won? board x y))
     ; if it won, return the board itself
     board)))

(define (board-sum board)
  (for*/sum ([x board-indexes]
             [y board-indexes])
    (or (board-ref board x y) 0)))

(define (part-1)
  (with-day 4
    (λ (in)
      (let ([drawn-nums (map string->number (string-split (read-line in) ","))]
            [boards (for/list ([l (in-lines in)])
                      (read-board (in-lines in)))])
        (for*/or ([n drawn-nums]
                  [b boards])
          ; if marking off this number completes the board, return the sum * n (which is truthy)
          (and (board-mark! b n)
               (* n (board-sum b))))))))

(define (part-2)
  (with-day 4
    (λ (in)
      (let* ([drawn-nums (map string->number (string-split (read-line in) ","))]
             [boards (for/list ([l (in-lines in)])
                       (read-board (in-lines in)))]
             ; fold down to the last winner by using the boards that haven't won as the accumulator
             [last-winner (for/fold ([bs boards]
                                     ; at this point there's only one board left
                                     #:result (first bs))
                                    ([n drawn-nums]
                                     ; quit once we only have one board left
                                     #:break (empty? (rest bs)))
                            (filter (λ (b) (not (board-mark! b n))) bs))])
        ; I can't be bothered tracking which number we were up to, so just try them all again
        (for*/or ([n drawn-nums])
          ; if marking off this number completes the board, return the sum * n (which is truthy)
          (and (board-mark! last-winner n)
               (* n (board-sum last-winner))))))))

(module+ main
  (printf "Part 1: ~a\n" (part-1))
  (printf "Part 2: ~a\n" (part-2)))
