(define (alphabet-index c)
  (cond ((char=? c #\S) (sub1 (alphabet-index #\a)))
        ((char=? c #\E) (add1 (alphabet-index #\z)))
        (else (- (char->integer c) 97))))

(define (parse input [rowlen (string-length (car input))])
  (let ([l (flatten (map string->list input))])
    (values (for/hash ([i (in-range (length l))])
                      (values (list (quotient i rowlen) (modulo i rowlen))
                              (alphabet-index (list-ref l i))))
            (length input) rowlen)))

(define (locate n tab)
  (findf (lambda (k) (= (hash-ref tab k) n)) (hash-keys tab)))
(define (array+ . args) (apply (curry map +) args))
(define (between a b x) (and (<= x b) (>= x a)))
(define (too-high? graph n x)
  (> (- (hash-ref graph x) (hash-ref graph n)) 1))

(define (make-adj graph h w node visited)
  (filter (lambda (x)
            (and (between 0 (- h 1) (car x))
                 (between 0 (- w 1) (cadr x))
                 (not (member visited x))
                 (not (too-high? graph node x))))
            (map (curry array+ node) '((0 1) (0 -1) (1 0) (-1 0)))))

(define (coord-list g v [coords (hash-keys g)] [len (hash-count g)])
  (make-hash (map (lambda (c) (cons c v)) coords)))

(define (extract-min q dist)
  (foldl (lambda (x y)
           (if (< (hash-ref dist x)
                  (hash-ref dist y (lambda () (expt 2 65)))) x y)) '() q))

(define (dijkstra graph h w start)
  (let* ([dist (coord-list graph (expt 2 64))]
         [q (hash-keys graph)]
         [prev (coord-list graph '())]
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
                  (make-adj graph h w u visited))))
    dist prev))

(define (parent-count node start parents [count 0])
  (let ([next-parent (hash-ref parents node)])
    (if (equal? next-parent '())
      (if (equal? node start) count -1)
      (parent-count next-parent start parents (+ 1 count)))))

(define (solve graph h w)
  (let* ([start (locate (sub1 (alphabet-index #\a)) graph)]
         [end (locate (add1 (alphabet-index #\z)) graph)]
         [part1 (parent-count end start (dijkstra graph h w start))];)
         [shortest
           (map (lambda (x)
                  (if (not (= (hash-ref graph x) 0))
                    -1
                    (parent-count end x (dijkstra graph h w x))))
                (hash-keys graph))]
         [part2 (take (sort (filter (lambda (x) (not (= x -1))) shortest) <) 1)])
    (println part1);))
    (println (car part2))))

(define-values (g1 h1 w1) (parse (file->lines "input12-1.txt")))
(define-values (g2 h2 w2) (parse (file->lines "input12-2.txt")))
(solve g1 h1 w1)
(solve g2 h2 w2)
