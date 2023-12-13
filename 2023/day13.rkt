(define (parse in)
  (map (lambda (pattern)
         (map (lambda (line) (string->list line)) (string-split pattern "\n")))
       (string-split (file->string in) "\n\n")))

(define (transpose pattern)
  (map (lambda (i) (map (lambda (line) (list-ref line i)) pattern))
       (range (length (car pattern)))))

(define (find-one-line line)
  (map (lambda (i)
         (let* ([m (min i (- (length line) i))])
           (andmap equal? (take-right (take line i) m)        ;; left part
                          (reverse (take (drop line i) m))))) ;; right part
       (range 1 (length line))))

(define (find-reflection pattern valid?)
  (index-where (map valid? (transpose (map find-one-line pattern))) identity))

(define (find-reflect-hor-ver valid? pattern)
  (let ([res (find-reflection pattern valid?)])
    (if res
      (add1 res)
      (* 100 (add1 (find-reflection (transpose pattern) valid?))))))

(define (solve in valid?)
  (apply + (map (curry find-reflect-hor-ver valid?) (parse in))))

(define valid1 (curry andmap identity))
(define (valid2 line)
  (= (sub1 (length line)) (apply + (map (lambda (x) (if x 1 0)) line))))

(println (solve "input13-1.txt" valid1))
(println (solve "input13-2.txt" valid1))
(println (solve "input13-1.txt" valid2))
(println (solve "input13-2.txt" valid2))
