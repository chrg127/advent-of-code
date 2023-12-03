(define (get-num line p) (string->number (substring line (car p) (cdr p))))
(define (grid-ref grid x y) (string-ref (list-ref grid y) x))
(define (is-symbol? c) (and (not (char=? c #\.)) (not (char-numeric? c))))
(define (is-gear? c) (char=? c #\*))

(define (get-coords p y)
  (append (list (list (- (car p) 1) y))
          (list (list (cdr p)       y))
          (map (lambda (x) (list x (sub1 y))) (range (sub1 (car p)) (+ (cdr p) 1)))
          (map (lambda (x) (list x (add1 y))) (range (sub1 (car p)) (+ (cdr p) 1)))))

; given a position on a line, find (start end]
(define (find-number line p [n (length line)])
  (let ([cond-fn (lambda (x) (or (< x 0) (>= x n)
                                 (not (char-numeric? (list-ref line x)))))])
    (cons (add1 (findf cond-fn (reverse (range -1 p))))
          (findf cond-fn (range p (add1 n))))))

; get numbers adjacent to a gear at x, y
(define (get-numbers grid x y)
  (map (lambda (m)
         (let ([line (list-ref grid (cadr m))])
           (get-num line (find-number (string->list line) (car m)))))
       (filter (lambda (c) (char-numeric? (grid-ref grid (car c) (cadr c))))
               (get-coords (cons x (+ x 1)) y))))

(define (solve is-symbol? post-process in)
  (apply + (flatten
  (map (lambda (line y)
         (map (lambda (x) (post-process (remove-duplicates (get-numbers in x y))))
              (filter (lambda (x) (is-symbol? (string-ref line x)))
                      (range (string-length line)))))
       in (range (length in))))))

(define solve1 (curry solve is-symbol? identity))
(define solve2 (curry solve is-gear? (lambda (ns) (if (= (length ns) 2) (apply * ns) 0))))

(println (solve1 (file->lines "input3-1.txt")))
(println (solve1 (file->lines "input3-2.txt")))
(println (solve2 (file->lines "input3-1.txt")))
(println (solve2 (file->lines "input3-2.txt")))
