(define (idiv a b) (floor (/ a b)))

(define (has-pattern-1 id)
  (let* ([str (number->string id)] [part-len (idiv (string-length str) 2)])
    (string=? (substring str 0 part-len)
              (substring str part-len))))

(define (all-equal? l)
  (if (empty? l)
    #t
    (andmap equal? (take l (- (length l) 1)) (drop l 1))))

(define (generate-parts str num-parts)
  (let* ([len (string-length str)]
         [part-len (idiv len num-parts)])
      (map (lambda (i) (substring str (* i part-len) (* (+ i 1) part-len)))
           (range num-parts))))

(define (has-pattern-2 id)
  (let* ([str (number->string id)] [len (string-length str)])
    (call/cc (lambda (return)
      (for-each (lambda (num-parts)
                  (when (and (= (modulo len num-parts) 0)
                             (all-equal? (generate-parts str num-parts)))
                    (return #t)))
                (inclusive-range 2 len))
      #f))))

(define (solve name)
  (let* ([in (map (lambda (x) (map string->number (string-split x "-")))
                  (string-split (string-replace (file->string name) "\n" "") ","))])
    (map (lambda (ids) (apply + (flatten ids)))
         (list (map (lambda (x) (filter has-pattern-1 (apply inclusive-range x))) in)
               (map (lambda (x) (filter has-pattern-2 (apply inclusive-range x))) in)))))

(println (solve "input2-1.txt"))
(println (solve "input2-2.txt"))
