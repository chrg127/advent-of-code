(define (end=? c s) (char=? (string-ref s (- (string-length s) 1)) c))

(define (build-node-path instrs paths [pos "AAA"] [i 0])
  (let* ([n (modulo i (length instrs))]
         [next ((list-ref instrs n) (hash-ref paths pos))])
    (stream-cons (list pos n) (build-node-path instrs paths next (+ i 1)))))

(define (find-zzz pred instrs paths [pos "AAA"])
  (for/or ([pos (build-node-path instrs paths pos)] [i (in-naturals)])
    (if (pred (car pos)) i #f)))

(define pred1 (lambda (x) (string=? x "ZZZ")))
(define pred2 (lambda (x) (end=? #\Z x)))

(define (parse in)
  (let* ([split (string-split (file->string in) "\n\n")]
         [instrs (map (lambda (c) (if (char=? c #\L) car cadr))
                      (string->list (car split)))]
         [paths (map (lambda (line)
                       `(,(substring line 0 3)
                          (,(substring line 7 10) ,(substring line 12 15))))
                     (string-split (cadr split) "\n"))])
    (list instrs (apply hash (apply append paths)))))

(define (solve1 in) (apply (curry find-zzz pred1) (parse in)))

(define (solve2 in)
  (let ([ins (parse in)])
    (apply lcm (map (lambda (x) (find-zzz pred2 (car ins) (cadr ins) x))
                    (filter (curry end=? #\A) (hash-keys (cadr ins)))))))

(println (solve1 "input8-1.txt"))
(println (solve1 "input8-3.txt"))
(println (solve1 "input8-2.txt"))
(println (solve2 "input8-2.txt"))
