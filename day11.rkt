(define (ints s) (map string->number (regexp-match* "[0-9]+" s)))

(define (parse-op s)
  (define ((make-op l op r) old)
    ((if (string=? op "+") + *)
     old (if (string=? r "old") old (string->number r))))
  (apply make-op (string-split (substring s 19) " ")))

(define (parse input)
  (let* ([res (map (lambda (x)
                     (let* ([m (string-split x "\n")])
                       (list (ints (second m))
                             (parse-op (third m))
                             (car (ints (fourth m)))
                             (car (ints (fifth m)))
                             (car (ints (sixth m))))))
                  (string-split input "\n\n"))]
         [magic (apply * (map third res))])
    (values (map car res) (map cdr res) magic)))

(define (select-monkey items op test fn)
  (let ([new-items (map (lambda (i) (fn (op i))) items)])
    (partition (lambda (i) (zero? (modulo i test))) new-items)))

(define (item-loop index items f fn op test t-val f-val)
  (f index (length (list-ref items index)))
  (let-values ([(tis fis) (select-monkey (list-ref items index) op test fn)])
    (let* ([i1 (list-set items index '())]
           [i2 (list-update i1 t-val (lambda (x) (append x tis)))])
      (list-update i2 f-val (lambda (x) (append x fis))))))

(define (round-loop items monkeys rounds fn f [len (length monkeys)])
  (foldl (lambda (x is)
           (foldl (lambda (index is2)
                    (apply item-loop (append (list index is2 f fn)
                                             (list-ref monkeys index))))
                  is (range len)))
         items (range rounds)))

(define ((solve num-rounds fn) items monkeys)
  (let ([inspections (build-list (length items) (lambda (x) 0))])
    (round-loop items monkeys num-rounds fn
                (lambda (m n) (set! inspections
                                (list-update inspections m (curry + n)))))
    (println (apply * (take (sort inspections >) 2)))))

(define ((adjust magic) x)
  (if (> x magic) (modulo x magic) x))

(define part1 (solve 20 (lambda (x) (quotient x 3))))
(define (part2 magic) (solve 10000 (lambda (x)
                                     (if (> x magic) (modulo x magic) x))))

(time (begin (define-values (items1 monkeys1 magic1) (parse (file->string "input11-1.txt")))
             (define-values (items2 monkeys2 magic2) (parse (file->string "input11-2.txt")))
             (part1 items1 monkeys1)
             (part1 items2 monkeys2)
             ((part2 magic1) items1 monkeys1)
             ((part2 magic2) items2 monkeys2)))
