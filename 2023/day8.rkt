(require srfi/1)

(define (end=? c s) (char=? (string-ref s (- (string-length s) 1)) c))
(define pred1 (lambda (x) (string=? x "ZZZ")))
(define pred2 (lambda (x) (end=? #\Z x)))

(define (path-loop pred instrs paths [pos "AAA"] [i 0])
  (if (pred pos)
    i
    (let* ([next ((car instrs) (hash-ref paths pos))])
      (path-loop pred (cdr instrs) paths next (+ i 1)))))

(define (parse in)
  (let* ([split (string-split (file->string in) "\n\n")]
         [instrs (map (lambda (c) (if (char=? c #\L) car cadr))
                      (string->list (car split)))]
         [paths (map (lambda (line)
                       `(,(substring line 0 3)
                          (,(substring line 7 10) ,(substring line 12 15))))
                     (string-split (cadr split) "\n"))])
    (list (apply circular-list instrs) (apply hash (apply append paths)))))

(define (solve1 in) (apply (curry path-loop pred1) (parse in)))

(define (solve2 in)
  (let ([ins (parse in)])
    (apply lcm (map (lambda (x) (path-loop pred2 (car ins) (cadr ins) x))
                    (filter (curry end=? #\A) (hash-keys (cadr ins)))))))

(println (solve1 "input8-1.txt"))
(println (solve1 "input8-3.txt"))
(println (solve1 "input8-2.txt"))
(println (solve2 "input8-2.txt"))
