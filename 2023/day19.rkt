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

(define (make-workflow-tree workflows name)
  (map (lambda (rule)
         (cond ([string=? (fourth rule) "R"] (list rule "R"))
               ([string=? (fourth rule) "A"] (list rule "A"))
               (else (cons rule (make-workflow-tree workflows (fourth rule))))))
       (hash-ref workflows name)))

(define (apply-rule rule bounds)
  (let ([inc (if (equal? (second rule) >) 1 -1)])
    (values (list-update bounds (if (equal? (second rule) <) 1 0)
                         (lambda (b) (list-update b (first rule)
                                                  (lambda (value) (+ inc (third rule))))))
            (list-update bounds (if (equal? (second rule) <) 0 1)
                         (lambda (b) (list-update b (first rule)
                                                  (lambda (value) (third rule))))))))

(define (walk-tree t bounds [i 0])
  ; (printf "~abounds = ~a\n" (make-string (* i 4) #\ ) bounds)
  (if (string? (car t))
    (begin ;(printf "~afinal bounds = ~a\n" (make-string (* i 4) #\ ) bounds)
           (if (string=? (car t) "R") '() (list bounds)))
    (append*
      (filter (lambda (x) (not (empty? x)))
              (cadr
                (foldl (lambda (c r)
                         (let ([cur-bounds (car r)]
                               [bounds-list (cadr r)])
                           (let-values ([(bound-if-true bound-if-false)
                                         (apply-rule (car c) cur-bounds)])
                             ; (printf "~achecking ~a\n" (make-string (* i 4) #\ ) (fourth (car c)))
                             (list bound-if-false
                                   (cons (walk-tree (cdr c) bound-if-true (+ i 1))
                                         bounds-list)))))
                       (list bounds '()) t))))))

(define (mul-bounds bs)
  (apply * (map (lambda (x y) (add1 (- x y)))
                (cadr bs) (car bs))))

(define (solve2 in)
  (let*-values ([(workflows parts) (parse in)]
                [(tree) (make-workflow-tree workflows "in")]
                [(bounds) (walk-tree tree '((1 1 1 1) (4000 4000 4000 4000)))]
                [(result) (apply + (remove-duplicates (map mul-bounds bounds)))])
    result))

(println (solve2 "input19-1.txt"))
(println (solve2 "input19-2.txt"))
