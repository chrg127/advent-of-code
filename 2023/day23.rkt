(require racket/hash)

(define (parse in)
  (list->vector (map (compose list->vector string->list) (file->lines in))))

(define (grid-ref g p)
  (let ([x (car p)] [y (cadr p)] [len (vector-length g)])
    (vector-ref (vector-ref g (modulo y len)) (modulo x len))))

(define (start-pos g y)
  (list (index-where (vector->list (vector-ref g y))
                     (lambda (x) (char=? x #\.))) y))

(define (get-neighbors g pos visited)
  (filter (lambda (p)
            (and (not (char=? (grid-ref g p) #\#))
                 (not (set-member? visited p))))
          (map (curry map + pos)
               (cond ([char=? (grid-ref g pos) #\>] (list '( 1  0)))
                     ([char=? (grid-ref g pos) #\<] (list '(-1  0)))
                     ([char=? (grid-ref g pos) #\v] (list '( 0  1)))
                     ([char=? (grid-ref g pos) #\^] (list '( 0 -1)))
                     (else '((1 0) (-1 0) (0 1) (0 -1)))))))

(define (path-count g start end visited i)
  (if (equal? start end)
    i
    (let ([ns (get-neighbors g start visited)])
      (if (empty? ns)
        -1
        (map (lambda (p) (path-count g p end (set-add visited p) (add1 i)))
             ns)))))

(define (solve1 in)
  (let* ([g (parse in)])
    (apply max (flatten (path-count g (start-pos g 0)
                                    (start-pos g (sub1 (vector-length g)))
                                    (set) 0)))))

(define (get-neighbors2 g pos from)
  (filter (lambda (p)
            (and (not (char=? (grid-ref g p) #\#)) (not (equal? p from))))
          (map (curry map + pos) '((1 0) (-1 0) (0 1) (0 -1)))))

(define (get-paths g start end)
  (define visited (mutable-set))
  (define (loop pos from start i)
    (let ([ns (get-neighbors2 g pos from)])
      (cond ([equal? pos end] (list (list start end i)))
            ([and (set-member? visited pos) (> (length ns) 1)]
             (list (list start pos i)))
            (else
              (set-add! visited pos)
              (if (= (length ns) 1)
                (loop (car ns) pos start (add1 i))
                (append* (list (list start pos i))
                         (map (lambda (p) (loop p pos pos 1)) ns)))))))
  (let ([next (map + start '(0 1))])
    (loop next start start 0)))

(define (make-path-graph paths)
  (foldl (lambda (p h)
           (hash-update h (car p)
                        (lambda (l) (remove-duplicates (cons (cadr p) l)))
                        (list (cadr p))))
         (hash) (append* (map (lambda (x)
                                (list (list (car x) (cdr x))
                                      (list (cadr x) (list (car x) (caddr x)))))
                              paths))))

(define (graph-neighbors g n visited)
  (filter (lambda (x) (not (set-member? visited (car x))))
          (hash-ref g n)))

(define (graph-path-count g pos end visited i)
  (if (equal? pos end)
    i
    (let ([ns (graph-neighbors g pos visited)])
      (if (empty? ns)
        -1
        (flatten (map (lambda (p)
                        (graph-path-count g (car p) end
                                          (set-add visited (car p))
                                          (+ (cadr p) i)))
                      ns))))))

(define (solve2 in)
  (let* ([grid (parse in)]
         [start (start-pos grid 0)]
         [end (start-pos grid (sub1 (vector-length grid)))]
         [graph (make-path-graph (get-paths grid start end))])
    (add1 (apply max (graph-path-count graph start end (set) 0)))))

(println (solve1 "input23-1.txt"))
(println (solve1 "input23-2.txt"))
(println (solve2 "input23-1.txt"))
(println (solve2 "input23-2.txt"))
