(define (manhattan a b)
  (apply + (map (lambda (x y) (abs (- x y))) a b)))

(define (increase positions dim x by)
  (map (lambda (p) (if (< (dim p) x) p (vec+ p by))) positions))

(define (expand positions dim n)
  (let ([scan (foldl (lambda (i h)
                       (if (not (empty? (filter (lambda (ps) (= (list-ref ps dim) i)) positions)))
                         h
                         (foldl (lambda (p h) (hash-update h p add1 0)) h
                                (filter (lambda (p) (> (list-ref p dim) i)) positions))))
                     (hash) (range (length positions)))])
    (map (lambda (p) (list-update p dim (lambda (x) (+ x (* (sub1 n) (hash-ref scan p 0))))))
         positions)))

(define (expand-2d positions n)
  (expand (expand positions 1 n) 0 n))

; car = y, cadr = x
(define (parse in)
  (let* ([lines (map string->list (file->lines in))]
         [positions (cartesian-product (range (length lines))
                                       (range (length (car lines))))])
    (map cadr (filter (lambda (x) (char=? (car x) #\#))
                      (map list (apply append lines) positions)))))

(define (solve in [n 2])
  (apply + (map (lambda (x) (apply manhattan x)) (combinations (expand-2d (parse in) n) 2))))

(println (solve "input11-1.txt"))
(println (solve "input11-2.txt"))
(println (solve "input11-1.txt" 10))
(println (solve "input11-1.txt" 100))
(println (solve "input11-2.txt" 1000000))
