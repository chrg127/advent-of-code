(define (parse in)
  (list->vector (map (compose list->vector string->list) (file->lines in))))

(define (grid-ref g p) (vector-ref (vector-ref g (cadr p)) (car p)))
(define (swap-xy p) (list (cadr p) (car p)))
(define (in-bounds? g p)
  (andmap (lambda (x) (and (>= x 0) (< x (vector-length g)))) p))

(define (get-new-dir symb dir)
  (cond ([not symb] '())
        ([char=? symb #\/] (list (swap-xy (map - dir))))
        ([char=? symb #\\] (list (swap-xy dir)))
        ([and (char=? symb #\|) (= (cadr dir) 0)] '((0 -1) (0 1)))
        ([and (char=? symb #\-) (= (car  dir) 0)] '((-1 0) (1 0)))
        (else (list dir))))

(define (update-beam grid pos dir [symb (grid-ref grid pos)])
  (map (lambda (dir) (list (map + pos dir) dir)) (get-new-dir symb dir)))

(define (add-tile tiles pos dir)
  (hash-update tiles pos (lambda (t) (cons dir t)) (list dir)))

(define (in-tiles? tiles pos dir)
  (and (hash-has-key? tiles pos) (member dir (hash-ref tiles pos))))

(define (beams-step grid beams tiles)
  (if (empty? beams)
    (length (hash-keys tiles))
    (let* ([new-beams
             (filter (lambda (b)
                       (and (not (empty? b)) (in-bounds? grid (car b))
                            (not (apply (curry in-tiles? tiles) b))))
                     (foldl (lambda (b r)
                              (append (apply (curry update-beam grid) b) r))
                            '() beams))]
           [new-tiles (foldl (lambda (x r) (apply (curry add-tile r) x))
                             tiles new-beams)])
      (beams-step grid new-beams new-tiles))))

(define (solve1 in)
  (beams-step (parse in) '(((0 0) (1 0))) (hash '(0 0) (list '(1 0)))))

(define (beam-loop-all-rows in)
  (let* ([input (parse in)] [len (vector-length input)])
    (map (lambda (pos)
           (map (lambda (dir)
                  (beams-step input (list (list pos dir)) (hash pos (list dir))))
                '((1 0) (-1 0) (0 1) (0 -1))))
         (append (map (lambda (x) (list x         0)) (range len))
                 (map (lambda (x) (list x (- len 1))) (range len))
                 (map (lambda (y) (list         0 y)) (range len))
                 (map (lambda (y) (list (- len 1) y)) (range len))))))

(define (solve2 in) (apply max (flatten (beam-loop-all-rows in))))

(println (solve1 "input16-1.txt"))
(println (solve1 "input16-2.txt"))
(println (solve2 "input16-1.txt"))
(println (solve2 "input16-2.txt"))
