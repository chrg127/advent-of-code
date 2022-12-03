(define (part1 input)
  (println (apply max (map (lambda (x) (apply + x))
                           (map (lambda (x) (map string->number (string-split x "\n")))
                                (string-split input "\n\n"))))))

(define (part2 input)
  (println (apply + (take (sort
      (map (lambda (x) (apply + x))
                    (map (lambda (x) (map string->number (string-split x "\n")))
                         (string-split input "\n\n"))) >) 3))))

(time (begin
        (define input1 (file->string "input1-1.txt"))
        (define input2 (file->string "input1-2.txt"))
        (part1 input1)
        (part1 input2)
        (part2 input1)
        (part2 input2)))
