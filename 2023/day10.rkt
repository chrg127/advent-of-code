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
    (if value (map (curry vec+ pos) (get-coords value)) '())))

(define (make-tab lst) (apply hash (apply append lst)))

(define (make-edges tab pos)
  (filter (lambda (c) (member pos (make-coords tab c))) (make-coords tab pos)))

(define (parse in [lines (file->lines in)])
  (let* ([ps (map (lambda (line y)
                    (map (lambda (x) (list (list x y) (string-ref line x)))
                         (range (string-length line))))
                  lines (range (length lines)))]
         [tab (make-tab (apply append ps))])
    (list tab (make-tab (map (lambda (k) (list k (make-edges tab k)))
                             (hash-keys tab))))))

(define (dfs-from g source [node source] [from '()] [i 1])
  (let ([next (car (filter (lambda (x) (not (equal? x from))) (hash-ref g node)))])
    (if (equal? next source)
      (list node)
      (cons node (dfs-from g source next node (+ i 1))))))

(define (hash-find-value tab val)
  (findf (lambda (k) (equal? (hash-ref tab k) val)) (hash-keys tab)))

(define (solve tab graph [start-pos (hash-find-value tab #\S)])
  (quotient (length (dfs-from graph start-pos)) 2))

(define (get-wall value down last-wall)
  (if (or (not value) (not down) (> (abs (- value down)) 1))
    #f
    (let ([diff (- value down)])
      (if (not (= diff last-wall)) diff #f))))

(define (scan-line width path tab y)
  (caddr (foldl (lambda (pos r)
                  (let* ([inside? (car r)] [last-wall (cadr r)] [inside (caddr r)]
                         [value (hash-ref path pos #f)]
                         [down  (hash-ref path (vec+ pos '(0 1)) #f)]
                         [wall (get-wall value down last-wall)])
                    (list (xor inside? (if wall #t #f))
                          (if wall wall last-wall)
                          (if inside? (cons pos inside) inside))))
                '(#f 0 ()) (map list (range width) (make-list width y)))))

(define (solve2 tab graph [start-pos (hash-find-value tab #\S)])
  (let* ([path (dfs-from graph start-pos)]
         [path-tab (make-tab (map list path (range (length path))))]
         [w (apply max (map car  (hash-keys tab)))]
         [h (apply max (map cadr (hash-keys tab)))])
    (length
      (filter (lambda (x) (not (hash-ref path-tab x #f)))
              (apply append (map (curry scan-line w path-tab tab) (range h)))))))

(println (apply solve  (parse "input10-1.txt"))) ; simple square loop. result: 4
(println (apply solve  (parse "input10-2.txt"))) ; slightly from complex, from part 1. result: 8
(println (apply solve  (parse "input10-3.txt"))) ; my input
(println (apply solve2 (parse "input10-4.txt"))) ; squarish loop with with stuff squeezing between pipes. result: 4
(println (apply solve2 (parse "input10-5.txt"))) ; another example with random bits of ground. result: 8
(println (apply solve2 (parse "input10-6.txt"))) ; third example, with a lot of junk. result: 10
(println (apply solve2 (parse "input10-3.txt"))) ; my input again
