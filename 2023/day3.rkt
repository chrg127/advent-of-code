(define (get-num line p) (string->number (substring line (car p) (cdr p))))
(define (out-of-bound? p n) (ormap (lambda (x) (or (< x 0) (>= x n))) p))
(define (is-symbol? c) (and (not (char=? c #\.)) (not (char-numeric? c))))
(define (grid-ref grid x y) (string-ref (list-ref grid y) x))

(define (get-coords p y)
  (append (list (list (- (car p) 1) y))
          (list (list (cdr p)       y))
          (map (lambda (x) (list x (sub1 y))) (range (sub1 (car p)) (+ (cdr p) 1)))
          (map (lambda (x) (list x (add1 y))) (range (sub1 (car p)) (+ (cdr p) 1)))))

(define (near-symbols? grid y pos)
  (ormap (lambda (c)
           (and (not (out-of-bound? c (length grid)))
                (is-symbol? (grid-ref grid (car c) (cadr c)))))
         (get-coords pos y)))

(define (solve in)
  (apply + (flatten
             (map (lambda (line y)
                    (map (curry get-num line)
                         (filter (curry near-symbols? in y)
                                 (regexp-match-positions* #rx"[0-9]+" line))))
                  in (range (length in))))))

(println (solve (file->lines "input3-1.txt")))
(println (solve (file->lines "input3-2.txt")))
