(define adjacent-positions '((-1 -1) (0 -1) (1 -1) (-1  0) (1  0) (-1  1) (0  1) (1  1)))
(define (vec+ . args) (apply map + args))
(define (grid-ref grid pos) (list-ref (list-ref grid (cadr pos)) (car pos)))

(define (adjacents grid pos)
  (filter (lambda (p) (and (>= (car  p) 0) (< (car  p) (length grid))
                           (>= (cadr p) 0) (< (cadr p) (length grid))
                           (char=? (grid-ref grid p) #\@)))
          (map (lambda (p) (vec+ pos p)) adjacent-positions)))

(define (solve1 name)
  (let* ([grid (map string->list (file->lines name))]
         [coords (filter (lambda (p) (and (char=? (grid-ref grid p) #\@)
                                          (< (length (adjacents grid p)) 4)))
                         (cartesian-product (range (length grid)) (range (length grid))))])
    (length coords)))

(define (remove-paper grid coords)
  (map (lambda (l y)
         (map (lambda (i x) (if (or (char=? i #\.) (member (list x y) coords)) #\. #\@))
              l (range (length grid))))
       grid (range (length grid))))

(define (solve2 name)
  (let* ([in (map string->list (file->lines name))])
    (let loop ([grid in])
      (let* ([coords (filter (lambda (p) (and (char=? (grid-ref grid p) #\@)
                                          (< (length (adjacents grid p)) 4)))
                         (cartesian-product (range (length grid)) (range (length grid))))]
             [new-grid (remove-paper grid coords)])
        (if (equal? grid new-grid)
          (length coords)
          (+ (length coords) (loop new-grid)))))))
