#lang racket

(provide with-day string->set)

(define (with-day day fn)
  (let* ([filename (string-append "day" (number->string day) ".txt")]
         [filepath (build-path (current-directory) "input" filename)])
    (call-with-input-file filepath fn)))

(define (string->set str)
  (for/set ([c (in-string str)]) c))
