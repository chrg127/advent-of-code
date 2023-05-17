(define (priority item)
  (let ((n (char->integer item)))
    (- n (if (> n 97) 96 38))))

(define (split-by lst n)
  (if (not (empty? lst))
    (cons (take lst n) (split-by (drop lst n) n))
    '()))

(define ((process make-sets adjust-input) input)
  (println (apply + (map (lambda (r)
                           (priority (car (apply set-intersect (make-sets r)))))
                         (adjust-input input)))))

(define part1
  (process (lambda (r) (split-by (string->list r) (/ (string-length r) 2)))
           identity))
(define part2
  (process (lambda (r) (map string->list r)) (lambda (i) (split-by i 3))))

(time (begin
        (define input1 (file->lines "input3-1.txt"))
        (define input2 (file->lines "input3-2.txt"))
        (part1 input1)
        (part1 input2)
        (part2 input1)
        (part2 input2)))
