(define (string-ref-def s p fn)
  (if (< p (string-length s)) (string-ref s p) (fn)))

(define (make-stack strings pos)
  (filter char-upper-case?
          (map (lambda (s) (string-ref-def s (add1 pos) (lambda () #\a)))
               strings)))

(define (make-stacks strings)
  (map (lambda (pos) (make-stack strings pos))
       (range 0 (string-length (last strings)) 4)))

(define (parse input)
  (let* ([splitted (string-split input "\n\n")])
    (list (make-stacks (drop-right (string-split (car splitted) "\n") 1))
          (map (lambda (l) (map string->number (regexp-match* "[0-9]+" l)))
               (string-split (cadr splitted) "\n")))))

(define (move-crates stacks n from to)
  (list-update
    (list-update stacks (sub1 from) (lambda (x) (drop x n)))
    (sub1 to) (lambda (x) (append (take (list-ref stacks (sub1 from)) n) x))))


(define (part1 input)
  (println (list->string
             (map car
                  (foldl (lambda (x r) (move-crates r (first x) (second x) (third x)))
                         (car input) (cadr input))))))

(define input1 (parse (file->string "input5-1.txt")))
(define input2 (parse (file->string "input5-2.txt")))
(part1 input1)
(part1 input2)
