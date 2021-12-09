#lang racket

(require "util.rkt")

(struct pos (x y) #:transparent)

(define (parse-heightmap in)
  (for*/hash ([(line y) (in-indexed (in-lines in))]
              [(h x) (in-indexed (in-string line))])
    (values (pos x y) (- (char->integer h) 48)))) ; '0' == 48

(define (heightmap-ref hm pt)
  (hash-ref hm pt 9))

(define (neighbours p)
  (match-let ([(pos x y) p])
    (list (pos (x . - . 1) y)
          (pos x (y . - . 1))
          (pos (x . + . 1) y)
          (pos x (y . + . 1)))))

(define (is-low-point? hm pt)
  (let ([h (heightmap-ref hm pt)])
    (andmap (λ (p)
              (h . < . (heightmap-ref hm p)))
            (neighbours pt))))

(define (find-low-points hm)
  (for/list ([(pt h) hm]
             #:when (is-low-point? hm pt))
    (cons pt h)))

(define (is-in-basin? hm pt)
  ((heightmap-ref hm pt) . < . 9))

(define (expand-basin hm start [basin (set)])
  (for/fold ([new-basin (set-add basin start)])
            ([p (neighbours start)]
             #:when (and (not (set-member? basin p))
                         (is-in-basin? hm p)))
    (expand-basin hm p new-basin)))

(define (find-basins hm low-points)
  (map (λ (p) (expand-basin hm (car p)))
       low-points))

(define (render-basin hm basin)
  (let ([locs (sort (hash-keys hm)
                    (λ (p1 p2) (or (< (pos-y p1) (pos-y p2))
                                   (and (= (pos-y p1) (pos-y p2))
                                        (< (pos-x p1) (pos-x p2))))))])
    (for ([p locs])
      (when (= 0 (pos-x p))
        (printf "\n"))
      (if (set-member? basin p)
          (display ".")
          (display (heightmap-ref hm p))))))

(module+ main
  (with-day 9
    (λ (in)
      (let* ([hm (parse-heightmap in)]
             [low-points (find-low-points hm)]
             [part-1 (for/sum ([pt low-points]) (+ (cdr pt) 1))]
             [basins (find-basins hm low-points)]
             [part-2 (apply * (take (sort (map set-count basins) >) 3))])

        ; (for ([b (sort basins > #:key set-count)])
        ;   (render-basin hm b)
        ;   (printf "\n"))

        (printf "Part 1: ~a\n" part-1)
        (printf "Part 2: ~a\n" part-2)))))
