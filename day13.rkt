(require srfi/1)

(define (parse input)
  (map (lambda (p)
         (map (lambda (s) (read (open-input-string (string-replace s "," " "))))
              (string-split p "\n"))) (string-split input "\n\n")))

(define (convert x) (if (number? x) (list x) x))
(define (compare-numbers a b)
  (cond ((< a b) 'lower) ((> a b) 'higher) ((= a b) 'idk)))

(define (compare-values a b)
  (cond ((and (number? a) (number? b)) (compare-numbers a b))
        ((and (list? a) (list? b)) (compare-lists a b))
        (else (compare-lists (convert a) (convert b)))))

(define (compare-lists a b)
  (cond ((and (empty? a) (not (empty? b))) 'lower)
        ((and (not (empty? a)) (empty? b)) 'higher)
        ((and (empty? a) (empty? b)) 'idk)
        (else (let ([r (compare-values (car a) (car b))])
                (if (not (eq? r 'idk)) r (compare-lists (cdr a) (cdr b)))))))

(define (imap proc lst [i 0])
  (if (equal? lst '()) '()
    (cons (proc (car lst) i) (imap proc (cdr lst) (+ i 1)))))

(define (part1 input)
  (println (apply + (imap (lambda (x i) (if (eq? x 'lower) i 0))
                          (map (lambda (p) (compare-values (car p) (cadr p)))
                               input) 1))))

(define (part2 input)
  (let* ([sorted (sort (append (apply append input) (list '((2)) '((6))))
                       (lambda (x y) (eq? (compare-values x y) 'lower)))])
    (println (apply * (map (lambda (x) (+ 1 (list-index (curry equal? x) sorted)))
                           (list '((2)) '((6))))))))

(define input1 (parse (file->string "input13-1.txt")))
(define input2 (parse (file->string "input13-2.txt")))
(part1 input1)
(part1 input2)
(part2 input1)
(part2 input2)
