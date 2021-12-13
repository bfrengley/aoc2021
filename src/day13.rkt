#lang racket

(require "util.rkt" threading)

(struct pos (x y) #:transparent)

(define (parse-paper in)
  (for*/set ([(ln y) (in-indexed (in-lines in))]
             #:break (= 0 (string-length ln)))
    (match-let ([(list x y) (string-split ln ",")])
      (pos (string->number x) (string->number y)))))

(define (parse-fold ln)
  (match-let* ([fold (string-trim ln "fold along ")]
               [(list dim line) (string-split fold "=")])
    (cons (string->symbol dim) (string->number line))))

(define (fold paper dim line)
  (for/set ([pt paper])
    (match-let ([(pos x y) pt])
      (case dim
        [(x) (pos (if (> x line) ((* 2 line) . - . x) x) y)]
        [(y) (pos x (if (> y line) ((* 2 line) . - . y) y))]))))

(define/match (pos<? p1 p2)
  [((pos x1 y1) (pos x2 y2)) (or (< y1 y2) (and (= y1 y2) (< x1 x2)))])

(define (print-paper paper)
  (let* ([max-x (apply max (set-map paper pos-x))]
         [max-y (apply max (set-map paper pos-y))])
    (for ([y (in-inclusive-range 0 max-y)])
      (for ([x (in-inclusive-range 0 max-x)])
        (display (if (set-member? paper (pos x y))
                     "#"
                     ".")))
      (displayln ""))))

(module+ main
  (with-day 13
    (λ (in)
      (let* ([paper  (parse-paper in)]
             [fold-1 (parse-fold (read-line in))]
             [paper^ (fold paper (car fold-1) (cdr fold-1))]
             [part-1 (set-count paper^)]
             [final  (for/fold ([res paper^])
                               ([ln (in-lines in)]
                                #:break (= 0 (string-length ln)))
                       (match-let ([(cons dim line) (parse-fold ln)])
                         (fold res dim line)))])
        (printf "Part 1: ~a\n" part-1)
        (displayln "Part 2:")
        (print-paper final)))))
