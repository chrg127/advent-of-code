(require graph)

(define (get-coords value)
  (cond ([char=? value #\|] '(( 0 -1) ( 0  1)))
        ([char=? value #\-] '((-1  0) ( 1  0)))
        ([char=? value #\L] '(( 0 -1) ( 1  0)))
        ([char=? value #\J] '((-1  0) ( 0 -1)))
        ([char=? value #\7] '((-1  0) ( 0  1)))
        ([char=? value #\F] '(( 1  0) ( 0  1)))
        ([char=? value #\S] '(( 1  0) (-1  0) ( 0  1) ( 0 -1)))
        (else '())))

(define (make-coords tab pos)
  (let ([value (hash-ref tab pos #f)])
    (if value
      (map (curry map + pos) (get-coords value))
      '())))

(define (make-edges tab pos)
  (filter (lambda (c) (member pos (make-coords tab c))) (make-coords tab pos)))

(define (make-graph tab)
  (unweighted-graph/adj
    (map (lambda (k) (cons k (make-edges tab k))) (hash-keys tab))))

(define (parse in [lines (file->lines in)])
  (let* ([ps (map (lambda (line y)
                    (map (lambda (x) (list (list x y) (string-ref line x)))
                         (range (string-length line))))
                  lines (range (length lines)))]
         [tab (apply hash (apply append (apply append ps)))])
    (list tab (make-graph tab))))

(define (hash-find-value tab val)
  (findf (lambda (k) (equal? (hash-ref tab k) val)) (hash-keys tab)))

(define (solve tab graph)
  (let*-values ([(start-pos) (hash-find-value tab #\S)]
                [(paths parents) (dijkstra graph start-pos)])
    (apply max (filter (lambda (x) (not (= x +inf.0))) (hash-values paths)))))

(println (apply solve (parse "input10-1.txt")))
(println (apply solve (parse "input10-2.txt")))
(println (apply solve (parse "input10-3.txt")))
