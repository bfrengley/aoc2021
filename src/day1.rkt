#lang racket

(require
  "util.rkt"
  (only-in srfi/1 zip))

(define (parse in)
  (map string->number (port->lines in)))

(define (part-1)
  (with-day 1
    (λ (in)
      (let* ([depths (parse in)]
             [pairs (zip depths (rest depths))])
        (count (λ (ds) (apply < ds)) pairs)))))

(define (part-2)
  (with-day 1
    (λ (in)
      (let* ([depths (parse in)]
             [pairs (zip depths (drop depths 3))])
        (count (λ (ds) (apply < ds)) pairs)))))

(module* main #f
  (printf "Part 1: ~a\n" (part-1))
  (printf "Part 2: ~a\n" (part-2)))
