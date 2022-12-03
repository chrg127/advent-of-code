(define (priority-of item)
  (let ((n (char->integer item)))
    (- n (if (> n 97) 96 38))))

(define (find-item rucksack)
  (let ((half (/ (string-length rucksack) 2)))
    (car (set-intersect (string->list (substring rucksack 0 half))
                        (string->list (substring rucksack half))))))

(define (part1 input)
  (println (apply + (map (lambda (r) (priority-of (find-item r))) input))))

(define (split-by lst n)
  (if (not (empty? lst))
    (cons (take lst n) (split-by (drop lst n) n))
    '()))

(define (find-badge groups)
  (car (apply set-intersect (map string->list groups))))

(define (part2 input)
  (println (apply + (map (lambda (gs) (priority-of (find-badge gs))) (split-by input 3)))))

(time (begin
        (define input1 (file->lines "input3-1.txt"))
        (define input2 (file->lines "input3-2.txt"))
        (part1 input1)
        (part1 input2)
        (part2 input1)
        (part2 input2)))
