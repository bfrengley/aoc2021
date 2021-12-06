#lang racket

(require "util.rkt")

(define (run-cycle fish)
  (for/hash ([i (in-inclusive-range 0 8)])
    (values
     i
     (case i
       ; new spawns
       [(8) (hash-ref fish 0 0)]
       ; first generation 6 dayers and 0 dayer wrap
       [(6) (+ (hash-ref fish 0 0) (hash-ref fish 7 0))]
       ; everyone else just decrements
       [else (hash-ref fish (+ i 1) 0)]))))

(define (run-cycles fish n)
  (if (= 0 n)
      fish
      (run-cycles (run-cycle fish) (- n 1))))

(define (parse line)
  (for/fold ([fish (make-immutable-hasheq)])
            ([day (string-split line ",")])
    (hash-update fish (string->number day) (curry + 1) 0)))

(module+ main
  (with-day 6
    (λ (in)
      (let* ([fish (parse (read-line in))]
             [part-1 (for/sum ([(_ v) (run-cycles fish 80)]) v)]
             [part-2 (for/sum ([(_ v) (run-cycles fish 256)]) v)])
        (printf "Part 1: ~a\n" part-1)
        (printf "Part 2: ~a\n" part-2)))))
