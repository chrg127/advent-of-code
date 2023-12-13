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

;; behold, the definition of insanity...
(define (arrangements line nums [group-end? #f] [inside? #f])
  (cond ([empty? line] (if (empty? nums) 1 0))
        ([and group-end? (char=? (car line) #\#)] 0)
        ([and inside? (char=? (car line) #\.)]    0)
        ([and (empty? nums) (char=? (car line) #\#)] 0)
        ([char=? (car line) #\.] (arrangements (cdr line) nums #f #f))
        ([empty? nums]           (arrangements (cdr line) nums #f #f))
        (group-end?              (arrangements (cdr line) nums #f #f))
        ([char=? (car line) #\#] (apply (curry arrangements (cdr line)) (decrease nums)))
        (inside?                 (apply (curry arrangements (cdr line)) (decrease nums)))
        (else (+ (apply (curry arrangements (cdr line)) (decrease nums))
                 (arrangements (cdr line) nums #f #f)))))

(define (solve1 in)
  (apply + (map (lambda (x) (apply arrangements x)) (parse in))))

(println (solve1 "input12-1.txt"))
(println (solve1 "input12-2.txt"))
