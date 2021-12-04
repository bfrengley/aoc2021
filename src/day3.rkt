#lang racket

(require "util.rkt")

(define bit0 (char->integer #\0))
(define bit1 (char->integer #\1))

(define (count-bits bitsets)
  (apply
   map
   (λ vs
     (for/sum ([b vs])
       (if (= b bit0) -1 1)))
   bitsets))

(define (bit-count->integer bit-count to-1?)
  (for/fold ([i 0])
            ([b bit-count])
    (bitwise-ior (arithmetic-shift i 1) (if (to-1? b) 1 0))))

(define (part-1)
  (with-day 3
    (λ (in)
      (let* ([bitsets    (map bytes->list (port->bytes-lines in))]
             [bit-counts (count-bits bitsets)]
             [gamma      (bit-count->integer bit-counts (λ (b) (> b 0)))]
             [epsilon    (bit-count->integer bit-counts (λ (b) (< b 0)))])
        (* gamma epsilon)))))

(define (find-bit-match bitsets to-1?)
  (define (loop bitsets n)
    (if (empty? (rest bitsets))
        ; return when we have a single match
        (first bitsets)
        ; otherwise filter and recur
        (let* ([bit-count  (for/sum ([bs bitsets]) (if (= bit0 (vector-ref bs n)) -1 1))]
               [common-bit (if (to-1? bit-count) bit1 bit0)]
               [matches    (filter (λ (bs) (= common-bit (vector-ref bs n))) bitsets)])
          (loop matches (+ n 1)))))
  (loop bitsets 0))

(define (bytes->integer bytes)
  (string->number (bytes->string/utf-8 (list->bytes (vector->list bytes))) 2))

(define (part-2)
  (with-day 3
    (λ (in)
      (let* ([bitsets    (map (compose1 list->vector bytes->list) (port->bytes-lines in))]
             [o2-match   (find-bit-match bitsets (λ (b) (>= b 0)))]
             [co2-match  (find-bit-match bitsets (λ (b) (<= b 0)))])
        (*
         (bytes->integer o2-match)
         (bytes->integer co2-match))))))

(module+ main
  (printf "Part 1: ~a\n" (part-1))
  (printf "Part 2: ~a\n" (part-2)))
