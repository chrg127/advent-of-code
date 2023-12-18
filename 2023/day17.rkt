(require graph)

(define (to-num c) (string->number (string c)))
(define (grid-ref g p) (vector-ref (vector-ref g (cadr p)) (car p)))
(define vec+ (curry map +))
(define vec- (curry map -))
(define (in-bounds? g p)
  (andmap (lambda (x) (and (>= x 0) (< x (vector-length g)))) p))

(define (parse in)
  (list->vector (map (lambda (line) (list->vector (map to-num (string->list line))))
                     (file->lines in))))

(define (gen-neighbors g pos dir n)
  (filter (lambda (p) (and (in-bounds? g (car p))
                           (not (> (caddr p) 2))
                           (not (equal? dir (map - (cadr p))))))
          (map (lambda (d) (list (vec+ pos d) d (if (equal? d dir) (+ n 1) 0)))
               '((1 0) (-1 0) (0 1) (0 -1)))))

(define (gen-vertices grid)
  (append-map (lambda (p)
                (append-map (lambda (d)
                              (map (lambda(n) (list p d n)) (range 4)))
                            '((1 0) (-1 0) (0 1) (0 -1))))
              (cartesian-product (range (vector-length grid)) (range (vector-length grid)))))

(define (gen-graph grid)
  (weighted-graph/directed
         (append-map (lambda (p)
                       (map (lambda (n) (list (grid-ref grid (car p)) p n))
                            (apply (curry gen-neighbors grid) p)))
                     (gen-vertices grid))))

(define (min-heat-loss len dists [i (sub1 len)])
  (apply min (flatten
               (map (lambda (d)
                      (map (lambda (n) (hash-ref dists (list (list i i) d n)))
                           (range 4)))
                    '((1 0) (-1 0) (0 1) (0 -1))))))

(define (solve in [input (parse in)])
  (let ([g (gen-graph input)])
    (let-values ([( dists  parents) (dijkstra g '((0 0) (1 0) 0))]
                 [(dists2 parents2) (dijkstra g '((0 0) (0 1) 0))])
      (min (min-heat-loss (vector-length input) dists)
           (min-heat-loss (vector-length input) dists)))))

(println (solve "input17-1.txt"))
(println (solve "input17-2.txt"))
