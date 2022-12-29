(require graph)

(define (make-graph input)
  (unweighted-graph/directed
    (apply append (map (lambda (n d) (map (curry list n) (cddr d)))
                       (map car input) input))))

(define (make-rates input)
  (apply hash (flatten (map list (map car input) (map cadr input)))))

(define (vals s) (regexp-match* "[A-Z][A-Z]|[0-9]+" s))
(define (parse input)
  (let ([data (map (lambda (l) (list-update (vals l) 1 string->number)) input)])
    (values (make-graph data) (make-rates data))))

(define (non-zero-valves rates)
  (filter (lambda (n) (not (zero? (hash-ref rates n)))) (hash-keys rates)))

(define (solve graph rates [paths (floyd-warshall graph)])
  (define (loop from closed [mins 30] [pressure 0])
    (if (empty? closed)
      pressure
      (apply max
             (map (lambda (to)
                    (let ([m (- mins (+ 1 (hash-ref paths (list from to))))])
                      (if (< m 0)
                        pressure
                        (loop to (remove to closed) m
                              (+ pressure (* m (hash-ref rates to)))))))
                  closed))))
  (loop "AA" (non-zero-valves rates)))

(define-values (graph1 rates1) (parse (file->lines "input16-1.txt")))
(define-values (graph2 rates2) (parse (file->lines "input16-2.txt")))
(println (solve graph1 rates1))
(println (solve graph2 rates2))
