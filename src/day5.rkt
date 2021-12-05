#lang racket

(require "util.rkt")

(struct pos (x y) #:transparent)

(define (string->point str)
  (match-let ([(list x y) (string-split str ",")])
    (pos (string->number x) (string->number y))))

(struct line (start end) #:transparent)

(define (string->line str)
  (match-let ([(list start end) (string-split str " -> ")])
    (line (string->point start) (string->point end))))

(define (line-horiz? line)
  (= (pos-y (line-start line)) (pos-y (line-end line))))

(define (line-vert? line)
  (= (pos-x (line-start line)) (pos-x (line-end line))))

(define (line-straight? line)
  (or (line-horiz? line) (line-vert? line)))

(define (line->list line)
  (define (coord-stream from to)
    (cond [(= from to) (in-cycle (stream from))]
          [(> from to) (in-inclusive-range from to -1)]
          [else        (in-inclusive-range from to)]))

  (let* ([start-x  (pos-x (line-start line))]
         [end-x    (pos-x (line-end line))]
         [start-y  (pos-y (line-start line))]
         [end-y    (pos-y (line-end line))]
         [x-stream (coord-stream start-x end-x)]
         [y-stream (coord-stream start-y end-y)])
    (for/list ([x x-stream]
               [y y-stream])
      (pos x y))))

(define (count-repeated-points in #:allow-diag? [allow-diag? false])
  (for*/fold ([points (make-immutable-hash)]
              #:result (sequence-count (λ (pt v) (>= v 2)) points))
             ([str (in-lines in)]
              [line (in-value (string->line str))]
              #:when (or allow-diag? (line-straight? line))
              [pt (line->list line)])
    (hash-update points pt (curry + 1) 0)))

(define (part-1)
  (with-day 5 count-repeated-points))

(define (part-2)
  (with-day 5 (curry count-repeated-points #:allow-diag? true)))

(module+ main
  (printf "Part 1: ~a\n" (part-1))
  (printf "Part 2: ~a\n" (part-2)))
