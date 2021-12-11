#lang racket

(require "util.rkt" threading)

(struct pos (x y) #:transparent)
(struct octogrid (height width cells) #:transparent)

(define (parse-octogrid in)
  (let* ([lines (port->lines in)]
         [height (length lines)]
         [width (string-length (first lines))])
    (octogrid height width
              (for/vector #:length height ([ln lines])
                (for/vector #:length width ([o (in-string ln)])
                  ((char->integer o) . - . 48))))))

(define (neighbours og pt)
  (for*/list ([x-off (in-inclusive-range -1 1)]
              [y-off (in-inclusive-range -1 1)]
              #:when (not (and (= x-off 0) (= y-off 0)))
              [x^ (in-value (+ (pos-x pt) x-off))]
              [y^ (in-value (+ (pos-y pt) y-off))]
              #:when (and (x^ . >= . 0)
                          (x^ . < . (octogrid-width og))
                          (y^ . >= . 0)
                          (y^ . < . (octogrid-height og))))
    (pos x^ y^)))

(define (octogrid-update! og pt [update (curry + 1)])
  (match-let* ([(pos x y) pt]
               [row (~> og
                        octogrid-cells
                        (vector-ref y))]
               [val (vector-ref row x)]
               [val^ (update val)])

    (vector-set! row x val^)
    (= val^ 10)))

(define (octogrid-tick! og)
  (~>> (for*/list ([y (in-range 0 (octogrid-height og))]
                   [x (in-range 0 (octogrid-width og))]
                   [pt (in-value (pos x y))]
                   #:when (octogrid-update! og pt))
         pt)
       (octogrid-flash! og))

  (for*/sum ([(row y) (in-indexed (octogrid-cells og))]
             [(v x) (in-indexed row)]
             #:when (v . >= . 10))
    (octogrid-update! og (pos x y) (λ (_) 0))
    1))

(define (octogrid-flash! og pts)
  (match pts
    ['() (void)]
    [(list* pt pts^) (~>> pt
                          (neighbours og)
                          (filter (curry octogrid-update! og))
                          ((lambda (new-pts) (append new-pts pts^)))
                          (octogrid-flash! og))]))

(define (octogrid-size og)
  (* (octogrid-width og) (octogrid-height og)))

(module+ main
  (with-day 11
    (λ (in)
      (let* ([og (parse-octogrid in)]
             [octos (octogrid-size og)]
             [part-1 (for/sum ([_ (in-range 1 101)]) (octogrid-tick! og))]
             [part-2 (for/first ([i (in-naturals 101)]
                                 #:when (= octos (octogrid-tick! og)))
                       i)])
        (printf "Part 1: ~a\n" part-1)
        (printf "Part 2: ~a\n" part-2)))))
