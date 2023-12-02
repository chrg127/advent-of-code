(require srfi/1)

(define (possible? game)
  (andmap (lambda (cubes) (and (<= (first cubes) 12)
                               (<= (second cubes) 13)
                               (<= (third cubes) 14)))
          (cdr game)))

(define (get-pos x) (hash-ref (hash "red" 0 "green" 1 "blue" 2) x))
(define (find-char str c) (list-index (curry char=? #\:) (string->list str)))

(define (parse in)
  (map (lambda (line id)
         (append (list id)
                 (map (lambda (sub)
                        (foldl (lambda (x r)
                                 (let ([tmp (string-split x " ")])
                                   (list-set r (get-pos (second tmp)) (string->number (first tmp)))))
                               '(0 0 0) (string-split sub ", ")))
                      (string-split (substring line (+ 2 (find-char line #\:))) "; "))))
       in (range 1 (add1 (length in)))))

(define (solve in)
  (apply + (map car (filter possible? (parse (file->lines in))))))

(println (solve "input2-1.txt"))
(println (solve "input2-2.txt"))
