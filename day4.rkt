(define (parse input)
  (map (lambda (l)
         (map (lambda (p) (map string->number (string-split p "-")))
                       (string-split l ","))) input))

(define (covers? a b c d) (or (and (>= c a) (<= d b)) (and (>= a c) (<= b d))))
(define (overlaps? a b c d) (not (or (< b c) (< d a))))

(define ((find-assignments pred) input)
  (println (length (filter (lambda (p) (apply pred (flatten p))) input))))

(define part1 (find-assignments covers?))
(define part2 (find-assignments overlaps?))

(time (begin
        (define input1 (parse (file->lines "input4-1.txt")))
        (define input2 (parse (file->lines "input4-2.txt")))
        (part1 input1)
        (part1 input2)
        (part2 input1)
        (part2 input2)))
