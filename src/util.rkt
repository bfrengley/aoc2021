#lang racket

(provide with-day)

(define (with-day day fn)
  (let* ([filename (string-append "day" (number->string day) ".txt")]
         [filepath (build-path (current-directory) "input" filename)])
    (call-with-input-file filepath fn)))
