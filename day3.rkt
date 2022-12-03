(define (priority-of item)
  (let ((n (char->integer item)))
    (- n (if (> n 97) 96 38))))

(define (split-by lst n)
  (if (not (empty? lst))
    (cons (take lst n) (split-by (drop lst n) n))
    '()))

(define (part1 input)
  (println (apply + (map
                      (lambda (r) (priority-of (car (apply set-intersect (split-by (string->list r)
                                                                                   (/ (string-length r) 2))))))
                      input))))

(define (part2 input)
  (println (apply + (map
                      (lambda (gs) (priority-of (car (apply set-intersect (map string->list gs)))))
                      (split-by input 3)))))

(time (begin
        (define input1 (file->lines "input3-1.txt"))
        (define input2 (file->lines "input3-2.txt"))
        (part1 input1)
        (part1 input2)
        (part2 input1)
        (part2 input2)))
