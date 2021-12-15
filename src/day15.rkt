#lang racket

(require "util.rkt" threading data/heap)

(struct graph (w h costs) #:transparent)
(struct cand (est cost path) #:transparent)
(struct pos (x y) #:transparent)

(define (parse-graph in)
  (let ([costs (for/vector ([ln (in-lines in)])
                 (for/vector #:length (string-length ln)
                   ([c ln])
                   (- (char->integer c) 48)))])
    (graph (vector-length (vector-ref costs 0))
           (vector-length costs)
           costs)))

(define (expand-graph gr factor)
  (let* ([old-w (graph-w gr)]
         [old-h (graph-h gr)]
         [new-w (* factor old-w)]
         [new-h (* factor old-h)])
    (graph
     new-w
     new-h
     (for/vector #:length new-h ([y (in-range 0 new-h)])
       (let-values ([(y-inc base-y) (quotient/remainder y old-h)])
         (for/vector #:length new-w ([x (in-range 0 new-w)])
           (let-values ([(x-inc base-x) (quotient/remainder x old-w)])
             ; wtf
             (add1
              (modulo
               (+ y-inc
                  x-inc
                  (sub1 (graph-cost gr (pos base-x base-y))))
               9)))))))))

(define (neighbours gr pt)
  (for*/list ([x-off (in-inclusive-range -1 1)]
              [y-off (in-inclusive-range -1 1)]
              #:unless (= (abs x-off) (abs y-off))
              [x^ (in-value (+ (pos-x pt) x-off))]
              [y^ (in-value (+ (pos-y pt) y-off))]
              #:when (and (x^ . >= . 0)
                          (x^ . < . (graph-w gr))
                          (y^ . >= . 0)
                          (y^ . < . (graph-h gr))))
    (pos x^ y^)))

(define (graph-cost gr pt)
  (~> gr
      graph-costs
      (vector-ref (pos-y pt))
      (vector-ref (pos-x pt))))

(define (estimate-cost pt)
  (+ (pos-x pt) (pos-y pt)))

(define (pos=? p1 p2)
  (and (= (pos-x p1) (pos-x p2))
       (= (pos-y p1) (pos-y p2))))

(define (cand<=? c1 c2)
  (<= (cand-est c1) (cand-est c2)))

(define (cand-end c)
  (first (cand-path c)))

(define (heap-pop! hp)
  (let ([min (heap-min hp)])
    (heap-remove-min! hp)
    min))

; this is rather slow
(define (find-best-path gr)
  (define cands (make-heap cand<=?))

  ; we're actually going backwards - it doesn't really matter, it simplifies some
  ; maths, and it means we don't have to reverse the path at the end
  (define target (pos 0 0))

  ; our initial point
  (define start
    (let ([end (pos (- (graph-w gr) 1)
                    (- (graph-h gr) 1))])
      (cand
       (estimate-cost end)
       ; because we're going backwards, we're not counting the cost of the end node and
       ; we will end up counting the cost of the start
       ; we can deal with that now by setting the initial cost to `end_cost + (-start_cost)`
       (- (graph-cost gr end) (graph-cost gr target))
       (list end))))

  ; don't re-expand from already seen nodes - the first part we encounter to a node is
  ; the best part
  ; proof by induction is left as an exercise to the reader
  (define seen (mutable-set))

  (do ([cnd start (heap-pop! cands)])
    ; if we've reached the target, return the candidate
    ((pos=? (cand-end cnd) target) cnd)

    (match-let* ([(cand _ cost path) cnd]
                 [end (first path)])
      ; if it's in seen, we've already found the best path to this node and added its
      ; outgoing paths, so skip this one
      (unless (set-member? seen end)
        (set-add! seen end)
        (for ([n (neighbours gr end)])
          (let* ([n-cost (+ (graph-cost gr n) cost)]
                 [n-est  (+ n-cost (estimate-cost n))])
            (heap-add! cands (cand n-est
                                   n-cost
                                   (cons n path)))))))))

(module+ main
  (with-day 15
    (λ (in)
      (let* ([gr (parse-graph in)]
             [part-1 (cand-cost (find-best-path gr))]
             [gr-2 (expand-graph gr 5)]
             [part-2 (cand-cost (find-best-path gr-2))])
        (printf "Part 1: ~a\n" part-1)
        (printf "Part 2: ~a\n" part-2)))))
