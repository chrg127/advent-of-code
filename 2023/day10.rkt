(require graph)

(define vec+ (curry map +))

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
      (map (curry vec+ pos) (get-coords value))
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

(define (dfs-from g source)
  (define (loop node from i)
    (let ([next (car (filter (lambda (x) (not (equal? x from))) (get-neighbors g node)))])
      (if (equal? next source)
        (list node)
        (cons node (loop next node (+ i 1))))))
  (loop source '() 1))

(define (hash-find-value tab val)
  (findf (lambda (k) (equal? (hash-ref tab k) val)) (hash-keys tab)))

(define (solve tab graph [start-pos (hash-find-value tab #\S)])
  (quotient (length (dfs-from graph start-pos)) 2))

(define (get-wall value down)
  (cond ([or (not value) (not down)] #f)
        ([> (abs (- value down)) 1] #f)
        ([= (- value down) 1] 'up)
        ([= (- value down) -1] 'down)))

(define (scan-line pos width path tab [inside '()] [num-walls 0])
  (if (= (car pos) width)
    inside
    (let* ([is-inside (= (modulo num-walls 2) 1)]
           [value (hash-ref path pos #f)]
           [down (hash-ref path (vec+ pos '(0 1)) #f)]
           [wall (get-wall value down)])
      (scan-line (vec+ pos '(1 0)) width path tab
                 (if (or wall is-inside) (cons pos inside) inside)
                 (+ num-walls (if wall 1 0))))))

(define (solve2 tab graph [start-pos (hash-find-value tab #\S)])
  (let* ([path (dfs-from graph start-pos)]
         [path-tab (apply hash (apply append (map list path (range (length path)))))]
         [width  (apply max (map car (hash-keys tab)))]
         [height (apply max (map cadr (hash-keys tab)))])
    (length
      (filter (lambda (x) (and (not (hash-ref path-tab x #f))
                               (char=? (hash-ref tab x) #\.)))
              (apply append
                     (map (lambda (y) (scan-line (list 0 y) width path-tab tab))
                          (range height)))))))

(println (apply solve (parse "input10-1.txt")))
(println (apply solve (parse "input10-2.txt")))
(println (apply solve (parse "input10-3.txt")))

(println (apply solve2 (parse "input10-4.txt")))
(println (apply solve2 (parse "input10-5.txt")))
(println (apply solve2 (parse "input10-3.txt")))
