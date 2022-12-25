(define (snafu->digit c) (hash-ref (hash #\= -2 #\- -1 #\0 0 #\1 1 #\2 2) c))
(define (digit->snafu c) (hash-ref (hash -2 #\= -1 #\- 0 #\0 1 #\1 2 #\2) c))

(define (snafu->string n) (list->string (map digit->snafu n)))

(define (snafu->number s)
  (define (loop cs [i 0])
    (if (empty? cs) 0 (+ (* (car cs) (expt 5 i)) (loop (cdr cs) (+ i 1)))))
  (loop (reverse (map snafu->digit (string->list s)))))

(define (number->snafu n)
  (define (loop n)
    (if (= n 0)
      '()
      (let-values ([(div mod) (quotient/remainder n 5)])
        (if (> mod 2)
          (cons (- mod 5) (loop (+ 1 div)))
          (cons mod (loop div))))))
  (reverse (loop n)))

(define (part1 input)
  (println (snafu->string (number->snafu (apply + (map snafu->number input))))))

(define input1 (file->lines "input25-1.txt"))
(define input2 (file->lines "input25-2.txt"))
(part1 input1)
(part1 input2)
