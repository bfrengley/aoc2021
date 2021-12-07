#lang racket

(require
  "util.rkt"
  (only-in racket/fixnum most-positive-fixnum most-negative-fixnum))

(define (parse line)
  (for/fold ([crabs (make-immutable-hasheq)]
             [left  (most-positive-fixnum)]
             [right (most-negative-fixnum)])
            ([pos (string-split line ",")])
    (let ([p (string->number pos)])
      (values (hash-update crabs p (curry + 1) 0)
              (min p left)
              (max p right)))))

(define (find-middle crabs left right [cost-fn linear-cost])
  (for/fold ([best (most-positive-fixnum)])
            ([p (in-inclusive-range left right)])
    (min best
         (for/sum ([(pos n) crabs])
           (* n (cost-fn pos p))))))

(define linear-cost (compose1 abs -))

(define (non-linear-cost pos target)
  (let ([dist (abs (- target pos))])
    ; the slow naive way
    ; (for/sum ([d (in-inclusive-range 1 dist)])
    ;   d)

    ; the fast maths way
    (/ (* dist (+ dist 1)) 2)))

(module+ main
  (with-day 7
    (λ (in)
      (let*-values ([(crabs left right) (parse (read-line in))]
                    [(part-1) (find-middle crabs left right)]
                    [(part-2) (find-middle crabs left right non-linear-cost)])
        (printf "Part 1: ~a\n" part-1)
        (printf "Part 2: ~a\n" part-2)))))
