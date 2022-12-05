(define (string-ref-def s p fn)
  (if (< p (string-length s)) (string-ref s p) (fn)))

(define (make-stacks strings)
  (map (lambda (p)
         (filter char-upper-case?
                 (map (lambda (s) (string-ref-def s (add1 p) (lambda () #\a)))
                      strings)))
       (range 0 (string-length (last strings)) 4)))

(define (parse input)
  (let ([splitted (string-split input "\n\n")])
    (list (make-stacks (drop-right (string-split (car splitted) "\n") 1))
          (map (lambda (l) (map string->number (regexp-match* "[0-9]+" l)))
               (string-split (cadr splitted) "\n")))))

(define ((move-crates n from to) stacks fn)
  (let-values ([(taken new) (split-at (list-ref stacks (sub1 from)) n)])
    (list-update (list-update stacks (sub1 from) (lambda (x) new))
                 (sub1 to) (lambda (x) (append (fn taken) x)))))

(define ((solve fn) input)
  (println (list->string (map car (foldl (lambda (x r)
                                           ((apply move-crates x) r fn))
                                         (car input) (cadr input))))))

(define part1 (solve reverse))
(define part2 (solve identity))

(time (begin (define input1 (parse (file->string "input5-1.txt")))
             (define input2 (parse (file->string "input5-2.txt")))
             (part1 input1)
             (part1 input2)
             (part2 input1)
             (part2 input2)))
