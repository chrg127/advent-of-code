(define (make-cmd cmd n)
  (cond ([or (char=? cmd #\0) (char=? cmd #\R)] (list n 0))
        ([or (char=? cmd #\1) (char=? cmd #\D)] (list 0 n))
        ([or (char=? cmd #\2) (char=? cmd #\L)] (list (- n) 0))
        ([or (char=? cmd #\3) (char=? cmd #\U)] (list 0 (- n)))
        (else 'unreachable)))

(define (parse in)
  (map (lambda (line)
         (let ([split (string-split line " ")])
           (make-cmd (string-ref (car split) 0)
                     (string->number (cadr split)))))
       (file->lines in)))

(define (parse2 in)
  (map (lambda (line)
         (let* ([split (string-split line " ")]
                [color (third split)]
                [len (string-length color)])
           (make-cmd (string-ref color (- len 2))
                     (string->number (substring color 2 (- len 2)) 16))))
       (file->lines in)))

(define (manhattan a b)
  (apply + (map (lambda (x y) (abs (- x y))) a b)))

; get polygon corners and perimeter
(define (dig commands)
  (remove-duplicates
    (foldl (lambda (command r)
             (let* ([pos (car (car r))]
                    [new-pos (map + pos command)])
               (list (append (list new-pos) (car r))
                     (+ (cadr r) (manhattan new-pos pos)))))
           '(((0 0)) 0) commands)))

; calculate length of polygon
(define (shoelace ps)
  (let ([xs (map car ps)] [ys (map cadr ps)])
    (quotient
      (apply + (map (lambda (xi yi xj yj) (- (* xi yj) (* xj yi)))
                    xs ys
                    (append (drop xs 1) (list (car xs)))
                    (append (drop ys 1) (list (car ys))))) 2)))

(define (solve in [part2? #f])
  (let* ([res (dig ((if part2? parse2 parse) in))]
         [dist (cadr res)]
         [area (abs (shoelace (car res)))])
    (+ area (quotient dist 2) 1)))

(println (solve "input18-1.txt"))
(println (solve "input18-2.txt"))
(println (solve "input18-1.txt" #t))
(println (solve "input18-2.txt" #t))
