(define (rotate-left rocks)
  (for/list ([y (in-range (sub1 (length rocks)) -1 -1)])
    (for/list ([x (in-range (length rocks))])
      (list-ref (list-ref rocks x) y))))

(define (rotate-right rocks)
  (for/list ([y (in-range (length rocks))])
    (for/list ([x (in-range (sub1 (length rocks)) -1 -1)])
      (list-ref (list-ref rocks x) y))))

(define (parse in) (map string->list (file->lines in)))

(define (move-rocks line)
  (let ([next (index-where line (lambda (x) (or (char=? x #\#) (char=? x #\O))))])
    (cond ([not next] line)
          ([= next 0] (cons (car line) (move-rocks (cdr line))))
          ([char=? (list-ref line next) #\#] (append (take line next) (move-rocks (list-tail line next))))
          (else
            (let* ([new-line (list-set line next #\.)])
              (cons #\O (move-rocks (cdr new-line))))))))

(define (total-load line)
  (map (lambda (x i) (if (char=? x #\O) (- (length line) i) 0))
       line (range (length line))))

(define (solve in)
  (apply + (flatten (map (compose total-load move-rocks) (rotate-left (parse in))))))

(define (cycle-step rocks)
  (let* ([west      (rotate-right (map move-rocks rocks))]
         [south     (rotate-right (map move-rocks west))]
         [rot-east  (rotate-right (map move-rocks south))]
         [rot-north (rotate-right (map move-rocks rot-east))])
    rot-north))

(define (run-for rocks n)
  (foldl (lambda (x r) (cycle-step r)) (rotate-left rocks) (range n)))

(define (solve2 in)
  (apply + (flatten (map total-load (run-for (parse in) 1000)))))

(println (solve "input14-1.txt"))
(println (solve "input14-2.txt"))
(println (solve2 "input14-1.txt"))
(println (solve2 "input14-2.txt"))
