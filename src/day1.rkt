#lang racket

(require "util.rkt")

(define (parse in)
  (stream-map string->number (sequence->stream (in-lines in))))

(define (part-1)
  (with-day 1
    (λ (in)
      (let* ([depths (parse in)]
             [pairs (stream-zip depths (stream-rest depths))])
        (stream-count (λ (ds) (apply < ds)) pairs)))))

(define (part-2)
  (with-day 1
    (λ (in)
      (let* ([depths (parse in)]
             [pairs (stream-zip depths (stream-tail depths 3))])
        (stream-count (λ (ds) (apply < ds)) pairs)))))

(module* main #f
  (printf "Part 1: ~a\n" (part-1))
  (printf "Part 2: ~a\n" (part-2)))
