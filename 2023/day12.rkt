(define (parse in)
  (map (lambda (line)
         (let* ([split (string-split line " ")]
                [nums (map string->number (string-split (cadr split) ","))])
           (list (string->list (car split)) nums)))
       (file->lines in)))

(define (decrease nums)
  (cond ([empty? nums] '(() #f #f))
        ([= (car nums) 1] (list (cdr nums) #t #f))
        (else (list (list-update nums 0 sub1) #f #t))))

(define (arrangements line nums)
  (define h (make-hash))
  (define (loop line nums group-end? inside?)
    (let ([cached (hash-ref h (list line nums group-end? inside?) #f)])
      (if cached
        cached
        (let ([res (cond ([empty? line] (if (empty? nums) 1 0))
                         ([and group-end?    (char=? (car line) #\#)] 0)
                         ([and inside?       (char=? (car line) #\.)]    0)
                         ([and (empty? nums) (char=? (car line) #\#)] 0)
                         ([char=? (car line) #\.] (loop (cdr line) nums #f #f))
                         ([empty? nums]           (loop (cdr line) nums #f #f))
                         (group-end?              (loop (cdr line) nums #f #f))
                         ([char=? (car line) #\#] (apply (curry loop (cdr line)) (decrease nums)))
                         (inside?                 (apply (curry loop (cdr line)) (decrease nums)))
                         (else (+ (apply (curry loop (cdr line)) (decrease nums))
                                  (loop (cdr line) nums #f #f))))])
          (hash-set! h (list line nums group-end? inside?) res)
          res))))
  (loop line nums #f #f))

(define (solve1 in)
  (apply + (map (lambda (x) (apply arrangements x)) (parse in))))

(define (expand line nums)
  (list (append line '(#\?) line '(#\?) line '(#\?) line '(#\?) line)
        (append nums nums nums nums nums)))

(define (solve2 in)
  (apply + (map (lambda (x) (apply arrangements (apply expand x))) (parse in))))

(println (solve1 "input12-1.txt"))
(println (solve1 "input12-2.txt"))
(println (solve2 "input12-1.txt"))
(println (solve2 "input12-2.txt"))
