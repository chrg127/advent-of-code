(define (cat-to-num c) (hash-ref (hash #\x 0 #\m 1 #\a 2 #\s 3) c))

(define (parse-cond s)
  (let* ([split (string-split s ":")])
    (if (= (length split) 1)
      (list 0 > 0 (car split))
      (list (cat-to-num (string-ref s 0))
            (if (char=? (string-ref s 1) #\<) < >)
            (string->number (substring (car split) 2))
            (cadr split)))))

(define (parse-workflows line)
  (let* ([brace-start (index-where (string->list line) (curry char=? #\{))])
    (list (substring line 0 brace-start)
          (map parse-cond
               (string-split (substring line (add1 brace-start)
                                        (sub1 (string-length line))) ",")))))

(define (parse-parts line)
  (map (lambda (f) (string->number (substring f 2)))
       (string-split (substring line 1 (sub1 (string-length line))) ",")))

(define (parse in)
  (let ([split (string-split (file->string in) "\n\n")])
    (values (apply hash (append* (map parse-workflows
                                      (string-split (car split) "\n"))))
            (map parse-parts (string-split (cadr split) "\n")))))

(define (apply-workflow part wf)
  (fourth
    (findf (lambda (rule)
             (cond ([not (list? rule)] rule)
                   ([(second rule) (list-ref part (first rule)) (third rule)]
                    (fourth rule))
                   (else #f)))
           wf)))

(define (accepted? workflows name part)
  (let ([res (apply-workflow part (hash-ref workflows name))])
    (cond ([string=? res "A"] #t)
          ([string=? res "R"] #f)
          (else (accepted? workflows res part)))))

(define (solve1 in)
  (let-values ([(ws ps) (parse in)])
    (apply + (flatten (filter (curry accepted? ws "in") ps)))))

(define (update-bound bounds which cat value)
  (list-update bounds which (lambda (b) (list-set b cat value))))

(define (apply-rule rule bounds)
  (let ([inc (if (equal? (second rule) >) 1 -1)] [val (third rule)])
    (if (= val 0) ; special case
      (values bounds bounds)
      (values (update-bound bounds (if (= inc 1) 0 1) (first rule) (+ inc val))
              (update-bound bounds (if (= inc 1) 1 0) (first rule) val)))))

(define (walk-rules workflows name bounds)
  (foldl (lambda (rule r)
           (let ([cur-bounds (car r)] [collected (cadr r)])
             (let-values ([(if-true if-false) (apply-rule rule cur-bounds)])
               (list if-false (cons (walk-tree workflows (fourth rule) if-true)
                                    collected)))))
         (list bounds '()) (hash-ref workflows name)))

(define (walk-tree workflows name bounds)
  (cond ([equal? name "R"] '())
        ([equal? name "A"] (list bounds))
        (else (append*
                (filter (lambda (x) (not (empty? x)))
                        (cadr (walk-rules workflows name bounds)))))))

(define (mul-bounds bs) (apply * (map (compose add1 -) (cadr bs) (car bs))))

(define (solve2 in)
  (define start-bounds '((1 1 1 1) (4000 4000 4000 4000)))
  (let-values ([(workflows parts) (parse in)])
    (apply + (map mul-bounds (walk-tree workflows "in" start-bounds)))))

(println (solve1 "input19-1.txt"))
(println (solve1 "input19-2.txt"))
(println (solve2 "input19-1.txt"))
(println (solve2 "input19-2.txt"))
