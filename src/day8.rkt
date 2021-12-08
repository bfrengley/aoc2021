#lang racket

(require "util.rkt")

(define (find-digits line)
  (match-let ([(list signals outputs) (string-split line " | ")])
    (cons (string-split signals) (string-split outputs))))

(define (digit-is-1478 dig)
  (case (string-length dig)
    [(2 3 4 7) #t]
    [else #f]))

(define digit-segments
  (vector (set #\a #\b #\c     #\e #\f #\g) ; 0
          (set         #\c         #\f    ) ; 1
          (set #\a     #\c #\d #\e     #\g) ; 2
          (set #\a     #\c #\d     #\f #\g) ; 3
          (set     #\b #\c #\d     #\f    ) ; 4
          (set #\a #\b     #\d     #\f #\g) ; 5
          (set #\a #\b     #\d #\e #\f #\g) ; 6
          (set #\a     #\c         #\f    ) ; 7
          (set #\a #\b #\c #\d #\e #\f #\g) ; 8
          (set #\a #\b #\c #\d     #\f #\g) ; 9
          ))

(define (determine-signals signals)
  (let* ([signal-segs    (map string->set signals)]
         [sorted-signals (sort signal-segs < #:key set-count)]
         [sig-vec        (list->vector sorted-signals)]

         ; we know these ones already because they have a unique length
         [sig-1 (vector-ref sig-vec 0)]
         [sig-7 (vector-ref sig-vec 1)]
         [sig-4 (vector-ref sig-vec 2)]
         [sig-8 (vector-ref sig-vec 9)]

         ; we can immediately work out certain segments
         ; known: (cf)
         ; known: 1 4 7 8
         [seg-a (set-subtract sig-7 sig-1)]
         ; known: a (cf)
         ; known: 1 4 7 8
         [seg-bd (set-subtract sig-4 sig-1)]
         ; known: a (bd) (cf)
         ; known: 1 4 7 8
         [sig-5-i (for/first ([i '(3 4 5)]
                              #:when (subset? seg-bd (vector-ref sig-vec i)))
                    i)]
         [sig-5 (vector-ref sig-vec sig-5-i)]
         ; known: a (bd) (cf) (abdfg)
         ; known: 1 4 5 7 8
         [seg-f (set-intersect sig-5 sig-1)]
         [seg-c (set-subtract sig-1 seg-f)]
         [seg-g (set-subtract sig-5 (set-union seg-a seg-f seg-bd))]
         ; known: a c f g (bd)
         ; known: 1 4 5 7 8
         [seg-acfg (set-union seg-a seg-c seg-f seg-g)]
         [sig-3-i (for/first ([i '(3 4 5)]
                              #:when (subset? seg-acfg (vector-ref sig-vec i)))
                    i)]
         [sig-3 (vector-ref sig-vec sig-3-i)]
         [seg-d (set-subtract sig-3 seg-acfg)]
         [seg-b (set-subtract seg-bd seg-d)]
         ; known: a b c d f g
         ; known: 1 3 4 5 7 8
         [sig-2-i (for/first ([i '(3 4 5)]
                              #:when (not (set-member? (set sig-3-i sig-5-i) i)))
                    i)]
         [sig-2 (vector-ref sig-vec sig-2-i)]
         [seg-acdg (set-union seg-a seg-c seg-d seg-g)]
         [seg-e (set-subtract sig-2 seg-acdg)]
         ; known: a b c d e f g
         ; known 1 2 3 4 5 7 8
         [sig-6 (set-union seg-a seg-b seg-d seg-e seg-f seg-g)]
         [sig-9 (set-union seg-a seg-b seg-c seg-d seg-f seg-g)]
         [sig-0 (set-union seg-a seg-b seg-c seg-e seg-f seg-g)]
         ; known: everything!
         )
    (vector sig-0 sig-1 sig-2 sig-3 sig-4 sig-5 sig-6 sig-7 sig-8 sig-9)))

(define (decode signals outputs)
  (let ([nums (map (λ (out)
                     (for/first ([i (in-inclusive-range 0 9)]
                                 #:when (set=? out (vector-ref signals i)))
                       i))
                   outputs)])
    (for/sum ([(n i) (in-indexed nums)])
      (* n (expt 10 (3 . - . i))))))

(define (part-2 inputs)
  (for/sum ([input inputs])
    (let* ([signals (first input)]
           [outputs (map string->set (rest input))]
           [correct-signals (determine-signals signals)])
      (decode correct-signals outputs))))

(module+ main
  (with-day 8
    (λ (in)
      (let* ([inputs (map find-digits (port->lines in))]
             [part-1 (length (filter-map digit-is-1478 (flatten (map rest inputs))))]
             [part-2-res (part-2 inputs)])
        (printf "Part 1: ~a\n" part-1)
        (printf "Part 2: ~a\n" part-2-res)))))
