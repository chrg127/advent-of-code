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
  (let* ([g (parse in)]
         [start (start-pos g 0)]
         [end (start-pos g (sub1 (vector-length g)))])
    (printf "start = ~a, end = ~a\n" start end)
    (apply max (flatten (path-count g start end (set) 0)))))
