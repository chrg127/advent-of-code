(require graph)

(define (list-index f l [i 0])
  (cond ((empty? l) #f)
        ((f (car l)) i)
        (else (list-index f (cdr l) (+ 1 i)))))

(define (find-node g name) (list-index (compose (curry string=? name) car) g))
(define (node-name g i) (car (list-ref g i)))
(define (node-rate g i) (cadr (list-ref g i)))

(define (make-graph input)
  (define (make-edge-pairs node data)
    (map (compose (curry list node) (curry find-node input)) (cddr data)))
  (unweighted-graph/directed
    (apply append (map make-edge-pairs (range (length input)) input))))

(define (parse input)
  (let ([data (map (lambda (l) (list-update (regexp-match* "[A-Z][A-Z]|[0-9]+" l)
                                            1 string->number)) input)])
    (values (make-graph data) data)))

(define (ratio g ds n)
  (let ([d (hash-ref ds n)])
    (if (zero? d) -1 (/ (cadr (list-ref g n)) d))))

(define (sort-valves g ds)
  (sort (range (length g)) (lambda (x y) (> (ratio g ds x) (ratio g ds y)))))

; (define (dist-between graph x y)
;   (let-values ([(dist prev) (dijkstra graph x)])
;     (hash-ref dist y)))

; (define (dist-loop g last cur valves [minutes 30] [pressure 0])
;   (let* ([d (dist-between g last cur)]
;          [new-min (- minutes (+ d 1))]
;          [new-pressure (+ pressure (* (node-rate g cur) new-min))])
;     (if (or (empty? valves) (<= new-min 0))
;       pressure
;       (begin
;         (printf "between ~a, ~a: ~a; r = ~a, m = ~a, p = ~a\n"
;                 (node-name g last) (node-name g cur) d (node-rate g cur) new-min new-pressure)
;         (dist-loop g cur (car valves) (cdr valves) new-min new-pressure)))))

(define (memo-walk graph data dists)
  (define memo (make-hash))
  (define (loop cur opened minutes)
    (let ([memo-res (hash-ref memo (list cur opened minutes))])
      (if memo-res
        memo-res
        (let* ([possible (find-possible-next-nodes opened)]
               [the-result (apply max (map (lambda (n) (loop n opened minutes))))])
          (hash-set! memo (list cur opened minutes) the-result)
          the-result)))))

(define (part1 graph data)
  (let-values ([(dist prev) (dijkstra graph (find-node data "AA"))])
    (let ([sorted (sort-valves data dist)])
      (for-each (lambda (x) (printf "(~a ~a ~a)\n"
                                    (node-name data x)
                                    (hash-ref dist x)
                                    (exact->inexact (ratio data dist x)))) sorted))))
      ; (println (dist-loop graph (find-node graph "AA") (car sorted) (cdr sorted))))))

; (define (get-next-valve g cur opened)
;   (let-values ([(dist prev) (dijkstra g cur)])
;     (let* ([sorted (sort-valves g dist)]
;            [possible (filter (lambda (x) (not (member x opened))) sorted)])
;       (values (car possible) (hash-ref dist (car possible))))))

; (define (dist-loop-2 g cur [opened '()] [minutes 30] [pressure 0])
;   (let-values ([(next d) (get-next-valve g cur opened)])
;     (let* ([new-min (- minutes (+ d 1))]
;            [new-pressure (+ pressure (* (node-rate g next) new-min))])
;       (if (<= new-min 0)
;         pressure
;         (begin
;           (printf "between ~a, ~a: ~a; r = ~a, m = ~a, p = ~a\n"
;                   (node-name g cur) (node-name g next) d (node-rate g next) new-min new-pressure)
;           (dist-loop-2 g next (cons next opened) new-min new-pressure))))))

; (define (part1-2 graph)
;   (for-each println (sort graph (lambda (x y) (> (cadr x) (cadr y)))))
;   (println (dist-loop-2 graph (find-node graph "AA"))))

; (define input1 (parse (file->lines "input16-1.txt")))

(define-values (graph1 data1) (parse (file->lines "input16-1.txt")))
(part1 graph1 data1)
(println (floyd-warshall graph1))

; (define input2 (parse (file->lines "input16-2.txt")))
; (part1 input1)
; (part1 input2)
; (part1 input3)
; (part1-2 input2)
; (part1-2 input1)
; (part1-2 input3)
