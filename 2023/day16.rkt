(define (parse in)
  (list->vector (map (compose list->vector string->list) (file->lines in))))

(define (in-bounds? g p)
  (andmap (lambda (x) (and (>= x 0) (< x (vector-length g)))) p))

(define (grid-ref g p) (vector-ref (vector-ref g (cadr p)) (car p)))

(define vec+ (curry map +))
(define (swap-xy p) (list (cadr p) (car p)))

(define (get-new-dir symb dir)
  (cond ([char=? symb #\/] (swap-xy (map - dir)))
        ([char=? symb #\\] (swap-xy dir))
        (else dir)))

(define (update-beam grid pos dir [symb (grid-ref grid pos)])
  (cond ([not symb] '())
        ([and (char=? symb #\|) (= (cadr dir) 0)]
         (list (list (vec+ pos '(0 -1)) '(0 -1))
               (list (vec+ pos '(0  1)) '(0  1))))
        ([and (char=? symb #\-) (= (car dir) 0)]
         (list (list (vec+ pos '(-1 0)) '(-1 0))
               (list (vec+ pos '( 1 0)) '( 1 0))))
        (else (let ([new-dir (get-new-dir symb dir)])
                (list (list (vec+ pos new-dir) new-dir))))))

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

(define (solve in)
  (beams-step (parse in) '(((0 0) (1 0))) (hash '(0 0) (list '(1 0)))))

(println (solve "input16-1.txt"))
(println (solve "input16-2.txt"))
