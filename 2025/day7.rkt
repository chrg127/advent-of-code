(define (grid-ref grid pos)
  (list-ref (list-ref grid (cadr pos)) (car pos)))

(define (vec+ . args) (apply map + args))

(define (advance beams grid)
  (let* ([num-splitted 0]
         [res (remove-duplicates
               (append* (map (lambda (beam)
                               (let ([new-beam (vec+ beam '(0 1))])
                                 (if (char=? (grid-ref grid new-beam) #\^)
                                     (begin
                                       (set! num-splitted (+ num-splitted 1))
                                       (list (vec+ new-beam '(-1 0))
                                             (vec+ new-beam '( 1 0))))
                                     (list new-beam))))
                             beams)))])
    (values res num-splitted)))

(define (part1 grid start end-y)
  (let loop ([beams (list start)]
             [splitted-count 0])
    (if (= (cadr (car beams)) (- end-y 1))
        splitted-count
        (let-values ([(new-beams num-splitted) (advance beams grid)])
          (loop new-beams (+ num-splitted splitted-count))))))

(define (part2 grid start end-y)
  (define memo (make-hash))
  (define (loop pos)
    (let ([beam (vec+ pos '(0 1))])
      (cond [(= (cadr beam) end-y) 1]
            [(hash-has-key? memo pos) (hash-ref memo pos)]
            [else (let ([res (if (char=? (grid-ref grid beam) #\^)
                                 (+ (loop (vec+ beam '(-1 0)))
                                    (loop (vec+ beam '( 1 0))))
                                 (loop beam))])
                    (begin
                      (hash-set! memo pos res)
                      res))])))
  (loop start))

(define (solve name)
  (let* ([grid (map string->list (file->lines name))]
         [end-y (length grid)]
         [coords (cartesian-product (range (length (car grid))) (range end-y))]
         [start (findf (lambda (p) (char=? (grid-ref grid p) #\S)) coords)])
    (list (part1 grid start end-y)
          (part2 grid start end-y))))
