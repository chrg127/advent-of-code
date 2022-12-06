(define ((slide n) s)
  (for/first ([i (in-range (- (string-length s) n))]
              #:when (not (check-duplicates
                            (string->list (substring s i (+ i n))))))
      (+ n i)))

(define (part1 input) (println (map (slide 4) input)))
(define (part2 input) (println (map (slide 14) input)))

(time (begin (define input1 (file->lines "input6-1.txt"))
             (define input2 (file->lines "input6-2.txt"))
             (part1 input1)
             (part1 input2)
             (part2 input1)
             (part2 input2)))
