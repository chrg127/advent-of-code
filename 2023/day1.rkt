(define (convert x)
  (if (char-numeric? (string-ref x 0))
    (string->number (string (string-ref x 0)))
    (hash-ref (hash "one" 1 "two" 2 "three" 3 "four" 4 "five" 5
                    "six" 6 "seven" 7 "eight" 8 "nine" 9) x)))

(define (parse-line line regexes)
  (map (lambda (pos) (convert (substring line (car pos) (cdr pos))))
       (sort
         (apply append
              (map (lambda (regex) (regexp-match-positions* regex line))
                   regexes))
         (lambda (x y) (< (car x) (car y))))))

(define (solve regexes in)
  (apply + (map (lambda (line)
                  (let ([ns (parse-line line regexes)])
                    (+ (* 10 (first ns)) (last ns))))
                (file->lines in))))

(define regexes1 '(#rx"[1-9]"))
(define regexes2 '(#rx"one" #rx"two" #rx"three" #rx"four" #rx"five"
                   #rx"six" #rx"seven" #rx"eight" #rx"nine" #rx"[1-9]"))

(println (solve regexes1 "input1-1.txt"))
(println (solve regexes1 "input1-3.txt"))
(println (solve regexes2 "input1-2.txt"))
(println (solve regexes2 "input1-3.txt"))
