#lang racket

(require
  "util.rkt"
  threading
  (only-in srfi/1 zip))

(define (parse-rules in)
  (for/hash ([ln (in-lines in)])
    (match-let ([(list pair res) (string-split ln " -> ")])
      (values (cons (string-ref pair 0) (string-ref pair 1))
              (string-ref res 0)))))

(define (parse-template ln)
  (let ([chars (string->list ln)])
    (for/fold ([pairs (make-immutable-hash)])
              ([e1 chars]
               [e2 (drop chars 1)])
      (hash-update pairs (cons e1 e2) add1 0))))

(define (expand-template rules template n)
  (for/fold ([polymer template])
            ([_ (in-range 0 n)])
    (for/fold ([pairs (make-immutable-hash)])
              ([(pair n) polymer])
      (let ([insert (hash-ref rules pair)]
            [update (curry + n)])
        (~> pairs
            (hash-update (cons (car pair) insert) update 0)
            (hash-update (cons insert (cdr pair)) update 0))))))

(define (summarise polymer last)
  (let* ([counts (for/fold ([counts (hash last 1)]
                            #:result (hash-values counts))
                           ([(pair n) polymer])
                   (hash-update counts (car pair) (curry + n) 0))]
         [least (apply min counts)]
         [most  (apply max counts)])
    (most . - . least)))

(module+ main
  (with-day 14
    (λ (in)
      (let* ([template-ln (read-line in)]
             [last-elem   (string-ref template-ln (sub1 (string-length template-ln)))]
             [template    (parse-template template-ln)]
             [_           (read-line in)] ; skip blank line
             [rules       (parse-rules in)]
             [expanded    (expand-template rules template 10)]
             [part-1      (summarise expanded last-elem)]
             [expanded-2  (expand-template rules expanded 30)]
             [part-2      (summarise expanded-2 last-elem)])
        (printf "Part 1: ~a\n" part-1)
        (printf "Part 2: ~a\n" part-2)))))
