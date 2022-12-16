(define (zip . args) (apply map list args))
(define (make-indexes graph)
  (apply hash (flatten (zip (map car graph) (range (length graph))))))
(define (apply-indexes graph indexes)
  (map (lambda (n) (append (take n 2) (map (curry hash-ref indexes) (cddr n)))) graph))
(define (parse input)
  (let ([res (map (lambda (l) (list-update (regexp-match* "[A-Z][A-Z]|[0-9]+" l)
                                           1 string->number)) input)])
    (apply-indexes res (make-indexes res))))

(define (find-node g name)
  (index-of g (findf (compose (curry string=? name) car) g)))
(define (node-name graph i) (car (list-ref graph i)))
(define (node-rate graph i) (cadr (list-ref graph i)))

(define (node-list g v)
  (make-hash (map (lambda (i) (cons i v)) (range (length g)))))
(define (extract-min q d)
  (foldl (lambda (x y) (if (< (hash-ref d x) (hash-ref d y)) x y))
         (car q) (cdr q)))

(define (get-adj g u visited)
  (filter (lambda (x) (not (set-member? visited x))) (cddr (list-ref g u))))

(define (dijkstra graph start)
  (let* ([dist (node-list graph (expt 2 64))]
         [prev (node-list graph '())]
         [q (range (length graph))]
         [visited (mutable-set)])
    (hash-set! dist start 0)
    (do () ((empty? q))
      (let* ([u (extract-min q dist)])
        (set! q (remove u q))
        (set-add! visited u)
        (for-each (lambda (v)
                    (let ([tmp (+ 1 (hash-ref dist u))])
                      (when (< tmp (hash-ref dist v))
                        (begin (hash-set! dist v tmp)
                               (hash-set! prev v u)))))
                  (get-adj graph u visited))))
    (values dist prev)))

(define (ratio g ds n)
  (let ([d (hash-ref ds n)])
    (if (zero? d) -1 (/ (cadr (list-ref g n)) d))))

(define (sort-valves graph dists)
  (sort (range (length graph))
        (lambda (x y) (> (ratio graph dists x) (ratio graph dists y)))))

(define (dist-between graph x y)
  (let-values ([(dist prev) (dijkstra graph x)])
    (hash-ref dist y)))

(define (dist-loop g last cur valves [minutes 30] [pressure 0])
  (let* ([d (dist-between g last cur)]
         [new-min (- minutes (+ d 1))]
         [new-pressure (+ pressure (* (node-rate g cur) new-min))])
    (if (or (empty? valves) (<= new-min 0))
      pressure
      (begin
        (printf "between ~a, ~a: ~a; r = ~a, m = ~a, p = ~a\n"
                (node-name g last)
                (node-name g cur)
                d
                (node-rate g cur)
                new-min
                new-pressure)
        (dist-loop g cur (car valves) (cdr valves) new-min new-pressure)))))

(define (part1 graph)
  (let-values ([(dist prev) (dijkstra graph (find-node graph "AA"))])
    (let ([sorted (sort-valves graph dist)])
      (for-each (lambda (x) (printf "(~a ~a)\n" (node-name graph x) (hash-ref dist x))) sorted)
      (println (map (curry node-name graph) sorted))
      (println (dist-loop graph (find-node graph "AA") (car sorted) (cdr sorted))))))

(define (get-next-valve g cur opened)
  (let-values ([(dist prev) (dijkstra g cur)])
    (let* ([sorted (sort-valves g dist)]
           [possible (filter (lambda (x) (not (member x opened))) sorted)])
      (values (car possible) (hash-ref dist (car possible))))))

(define (dist-loop g last cur valves [minutes 30] [pressure 0])
  (let* ([d (dist-between g last cur)]
         [new-min (- minutes (+ d 1))]
         [new-pressure (+ pressure (* (node-rate g cur) new-min))])
    (if (or (empty? valves) (<= new-min 0))
      pressure
      (begin
        (printf "between ~a, ~a: ~a; r = ~a, m = ~a, p = ~a\n"
                (node-name g last) (node-name g cur) d (node-rate g cur) new-min new-pressure)
        (dist-loop g cur (car valves) (cdr valves) new-min new-pressure)))))

(define (dist-loop-2 g cur [opened '()] [minutes 30] [pressure 0])
  (let-values ([(next d) (get-next-valve g cur opened)])
    (let* ([new-min (- minutes (+ d 1))]
           [new-pressure (+ pressure (* (node-rate g next) new-min))])
      (if (<= new-min 0)
        pressure
        (begin
          (printf "between ~a, ~a: ~a; r = ~a, m = ~a, p = ~a\n"
                  (node-name g cur) (node-name g next) d (node-rate g next) new-min new-pressure)
          (dist-loop-2 g next (cons next opened) new-min new-pressure))))))

(define (part1-2 graph)
  (for-each println (sort graph (lambda (x y) (> (cadr x) (cadr y)))))
  (println (dist-loop-2 graph (find-node graph "AA"))))

(define input1 (parse (file->lines "input16-1.txt")))
(define input2 (parse (file->lines "input16-2.txt")))
(part1 input1)
(part1-2 input1)
(part1 input2)
(part1-2 input2)
