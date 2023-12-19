(define (list-index pred lst [i 0])
  (cond ([empty? lst] #f)
        ([pred (car lst)] i)
        (else (list-index pred (cdr lst) (add1 i)))))

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
  (let* ([brace-start (list-index (lambda (c) (char=? c #\{)) (string->list line))])
    (list (substring line 0 brace-start)
          (map parse-cond (string-split (substring line (add1 brace-start) (sub1 (string-length line))) ",")))))

(define (parse-parts line)
  (map (lambda (f) (string->number (substring f 2)))
       (string-split (substring line 1 (sub1 (string-length line))) ",")))

(define (parse in)
  (let ([split (string-split (file->string in) "\n\n")])
    (values (apply hash (append* (map parse-workflows (string-split (car split) "\n"))))
            (map parse-parts (string-split (cadr split) "\n")))))

(define (apply-workflow part wf)
  (fourth
    (findf (lambda (rule)
             (cond ([not (list? rule)] rule)
                   ([(second rule) (list-ref part (first rule)) (third rule)] (fourth rule))
                   (else #f)))
           wf)))

(define (accepted? workflows name part)
  (let ([res (apply-workflow part (hash-ref workflows name))])
    (cond ([string=? res "A"] #t)
          ([string=? res "R"] #f)
          (else (accepted? workflows res part)))))

(define (solve in)
  (let-values ([(ws ps) (parse in)])
    (apply + (flatten (filter (curry accepted? ws "in") ps)))))

(println (solve "input19-1.txt"))
(println (solve "input19-2.txt"))
