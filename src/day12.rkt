#lang racket

(require "util.rkt" threading)

(struct graph (maj min edges) #:transparent)

(define (graph-dests gr from)
  (hash-ref (graph-edges gr) from (set)))

(define (graph-major? gr node)
  (set-member? (graph-maj gr) node))

(define (is-major-node? node)
  (string=? node (string-upcase node)))

(define (parse-graph in)
  (for/fold ([maj (set)]
             [min (set)]
             [edges (make-immutable-hash)]
             #:result (graph maj min edges))
            ([ln (in-lines in)])
    (match-let ([(list from to) (string-split ln "-")])
      (let-values ([(maj^ min^) (partition is-major-node? (list from to))])
        (values (set-union maj (~>> maj^
                                    (map string->symbol)
                                    list->set))
                (set-union min (~>> min^
                                    (map string->symbol)
                                    list->set))
                (let* ([from (string->symbol from)]
                       [to   (string->symbol to)]
                       [has-terminal (or (symbol=? from 'start)
                                         (symbol=? to 'start)
                                         (symbol=? from 'end)
                                         (symbol=? to 'end))]
                       [is-reverse (or (symbol=? to 'start)
                                       (symbol=? from 'end))]
                       [from^  (if is-reverse to from)]
                       [to^    (if is-reverse from to)]
                       [edges^ (hash-update edges from^
                                            (λ~> (set-add to^))
                                            (set))])
                  ; only add the reverse edges for non-terminal nodes
                  (if has-terminal
                      edges^
                      (hash-update edges^ to^
                                   (λ~> (set-add from^))
                                   (set)))))))))

(define (find-paths caves allow-double?)
  (let search ([node 'start]
               [min-seen (set)]
               [doubled (not allow-double?)])
    (let* ([is-maj (graph-major? caves node)]
           [is-min (not is-maj)]
           [is-seen (and is-min (set-member? min-seen node))]
           [min-seen^ (if is-maj min-seen (set-add min-seen node))])
      (cond
        [(and is-seen doubled) '()]
        [(symbol=? node 'end) '((end))]
        [else (for*/list ([dest (graph-dests caves node)]
                          [path (search dest min-seen^ (or doubled is-seen))])
                (cons node path))]))))

(module+ main
  (with-day 12
    (λ (in)
      (let* ([caves (parse-graph in)]
             [part-1 (length (find-paths caves #f))]
             [part-2 (length (find-paths caves #t))])
        (printf "Part 1: ~a\n" part-1)
        (printf "Part 2: ~a\n" part-2)))))