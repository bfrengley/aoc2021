#lang racket

(require "util.rkt")

(define (parse-line ln)
  (match-let ([(list dir n) (string-split ln)])
    (cons
     (string->symbol (substring dir 0 1))
     (string->number n))))

(define (part-1)
  (with-day 2
    (λ (in)
      (for/fold ([depth 0]
                 [dist 0]
                 #:result (* depth dist))
                ([vals (sequence-map parse-line (in-lines in))])
        (match vals
          [(cons 'f n) (values depth (+ dist n))]
          [(cons 'd n) (values (+ depth n) dist)]
          [(cons 'u n) (values (- depth n) dist)])))))

(define (part-2)
  (with-day 2
    (λ (in)
      (for/fold ([depth 0]
                 [dist 0]
                 [aim 0]
                 #:result (* depth dist))
                ([vals (sequence-map parse-line (in-lines in))])
        (match vals
          [(cons 'f n) (values (+ depth (* aim n)) (+ dist n) aim)]
          [(cons 'd n) (values depth dist (+ aim n))]
          [(cons 'u n) (values depth dist (- aim n))])))))

(module* main #f
  (printf "Part 1: ~a\n" (part-1))
  (printf "Part 2: ~a\n" (part-2)))
