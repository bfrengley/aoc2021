#lang racket

(provide with-day zip stream-zip)

(define (with-day day fn)
  (let* ([filename (string-append "day" (number->string day) ".txt")]
         [filepath (build-path (current-directory) "input" filename)])
    (call-with-input-file filepath fn)))

(define (zip as bs)
  (for/list ([a as]
             [b bs])
    (list a b)))

(define (stream-zip as bs)
  (for/stream ([a as]
               [b bs])
    (list a b)))
