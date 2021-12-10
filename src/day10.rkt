#lang racket

(require "util.rkt")

(define (close open)
  (case open
    [(#\<) #\>]
    [(#\() #\)]
    [(#\[) #\]]
    [(#\{) #\}]
    [else #f]))

(define (syntax-error? text)
  (define (find-err tokens stack)
    (let ([eof? (null? tokens)]
          [eos? (null? stack)])
      (cond
        ; everything matched, no errors
        [(and eof? eos?) (values #f '())]
        ; out of input but unmatched opens, incomplete
        [eof? (values #f stack)]
        ; nothing to match, start a new open
        [eos? (find-err (rest tokens)
                        (list (close (first tokens))))]
        ; check if we have an opener or a closer
        [else (match-let* ([(list* tok tokens^) tokens]
                           [(list* expect stack^) stack]
                           [closer (close tok)])
                (cond
                  ; next token closes a pair, proceed
                  [(char=? tok expect) (find-err tokens^ stack^)]
                  ; next token opens a pair, push to stack and proceed
                  [closer (find-err tokens^ (cons closer stack))]
                  ; unexpected close that doesn't match the top of the stack that's an error
                  [else (values #t tok)]))])))

  (find-err (string->list text) '()))

(define (error-score err? chr)
  (if err? (case chr
             [(#\)) 3]
             [(#\]) 57]
             [(#\}) 1197]
             [(#\>) 25137])
      0))

(define (score err? toks)
  (define/match (score^ toks points)
    [('() _) points]
    [(_ _) (let ([points^ (+ (* points 5)
                             (case (first toks)
                               [(#\)) 1]
                               [(#\]) 2]
                               [(#\}) 3]
                               [(#\>) 4]))])
             (score^ (rest toks) points^))])

  (if (err? . or . (null? toks))
      #f
      (score^ toks 0)))

(module+ main
  (with-day 10
    (λ (in)
      (define input (port->lines in))

      (let ([part-1 (for/sum ([ln input])
                      (call-with-values
                       (λ () (syntax-error? ln))
                       error-score))])
        (printf "Part 1: ~a\n" part-1))

      (let* ([scores (filter-map (λ (ln) (call-with-values
                                          (λ () (syntax-error? ln))
                                          score))
                                 input)]
             [sorted-scores (sort scores <)]
             [mid-score (list-ref sorted-scores (arithmetic-shift (length sorted-scores) -1))])
        (printf "Part 2: ~a\n" mid-score)))))

