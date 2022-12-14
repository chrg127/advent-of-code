(define (ints s) (map string->number (regexp-match* "[0-9]+" s)))
(define (make-pairs l)
  (if (empty? l) '() (cons (list (car l) (cadr l)) (make-pairs (cddr l)))))
(define (array+ . args) (apply (curry map + ) args))

(define (gen-coords from to)
  (define (gen a b c d)
    (apply append
           (for/list ([i (in-range (min a c) (+ 1 (max a c)))])
             (for/list ([j (in-range (min b d) (+ 1 (max b d)))])
               (list i j)))))
  (apply gen (flatten (list from to))))

(define (make-paths l)
  (if (empty? (cdr l)) '()
    (append (gen-coords (car l) (cadr l)) (make-paths (cdr l)))))

(define (parse input)
  (define (matching n x) (if (= n (cadr x)) (car x) #f))
  (let* ([cs (apply append (map (compose make-paths make-pairs ints) input))]
         [max-y (apply max (map cadr cs))])
    (values (build-vector (+ 3 max-y)
                          (lambda (n) (list->set (filter-map (curry matching n)
                                                             cs))))
      max-y)))

(define (in-grid? grid p) (set-member? (vector-ref grid (cadr p)) (car p)))
(define (add-grid! grid pos)
  (vector-set! grid (cadr pos) (set-add
                                 (vector-ref grid (cadr pos)) (car pos))))

(define (next-pos p grid inf-cond)
  (findf (lambda (x) (and (not (in-grid? grid x)) (inf-cond x)))
         (map (curry array+ p) '((0 1) (-1 1) (1 1)))))

(define (sim-one pos grid can-finish? inf-cond)
  (let ([next (next-pos pos grid inf-cond)])
    (cond ((can-finish? pos next) 'finish)
          ((not next) pos)
          (else (sim-one next grid can-finish? inf-cond)))))

(define (sim grid can-finish? inf-cond [sand 0])
  (do ([finish #f]) (finish)
    (let ([res (sim-one '(500 0) grid can-finish? inf-cond)])
      (if (eq? res 'finish)
        (set! finish #t)
        (begin (add-grid! grid res)
               (set! sand (+ sand 1))))))
  sand)

(define (part1 grid max-y)
  (println (sim grid (lambda (p n) (and n (= (cadr n) (+ 1 max-y))))
                     (lambda (x) #t))))

(define (part2 grid max-y)
  (println (+ 1 (sim grid (lambda (p n) (and (not n) (equal? p '(500 0))))
                          (lambda (x) (not (= (cadr x) (+ max-y 2))))))))

(time (begin (define-values (paths1 max-y1) (parse (file->lines "input14-1.txt")))
             (define-values (paths2 max-y2) (parse (file->lines "input14-2.txt")))
             (part1 (vector-copy paths1) max-y1)
             (part2 (vector-copy paths1) max-y1)
             (part1 (vector-copy paths2) max-y2)
             (part2 (vector-copy paths2) max-y2)))
