(require srfi/1) ; list-index

(define (update-dir cwd i new-dir)
  (list-update cwd 2 (lambda (c) (list-set c i new-dir))))

(define (chdir dir lines cwd)
  (if (string=? dir "..")
    (list 'early-break cwd)
    (let* ([i (list-index (lambda (c) (and (eq? (car c) 'dir)
                                          (string=? (cadr c) dir)))
                         (third cwd))]
           [r (step lines (list-ref (third cwd) i))])
      (list (car r) (update-dir cwd i (cadr r))))))

(define (new-file line)
  (let ([splitted (string-split line " ")])
    (if (string=? (car splitted) "dir")
      (list 'dir (cadr splitted) '())
      (list 'file (cadr splitted) (string->number (car splitted))))))

(define (dup-file? files file)
  (memf (lambda (f) (and (eq? (car f) (car file))
                         (string=? (cadr f) (cadr file)))) files))

(define (add-file dir file)
  (list-update dir 2 (lambda (c)
                       (if (dup-file? c file) c (cons file c)))))

(define (parse-line line lines cwd)
  (cond ((not (char=? (string-ref line 0) #\$))
         (list lines (add-file cwd (new-file line))))
        ((string=? line "$ ls")
         (list lines cwd)) ; ls commands are ignored
        (else (chdir (substring line 5) lines cwd))))

(define (step lines [cwd (list 'dir "/" '())])
  (if (empty? lines)
    (list '() cwd)
    (let ([r (parse-line (car lines) (cdr lines) cwd)])
      (if (eq? (car r) 'early-break)
        (list (cdr lines) (cadr r))
        (apply step r)))))

(define (make-tree input) (cadr (step (cdr input))))

(define (total-size dir)
  (if (eq? (car dir) 'file)
    (third dir)
    (apply + (map total-size (third dir)))))

(define (filter-sizes fn root)
  (define dir-sizes '())
  (define (loop dir)
    (if (eq? (car dir) 'file)
      (third dir)
      (let ([size (apply + (map loop (third dir)))])
        (set! dir-sizes (if (fn size) (cons size dir-sizes) dir-sizes))
        size)))
  (loop root)
  dir-sizes)

(define (part1 input)
  (println (apply + (filter-sizes (lambda (s) (< s 100000))
                                  (make-tree input)))))

(define (part2 input)
  (let* ([tree (make-tree input)]
         [unused (- 70000000 (total-size tree))])
    (println
      (apply min (filter-sizes (lambda (s) (> (+ unused s) 30000000))
                               (make-tree input))))))

; not actually part of the solution, but it's neat
(define (pretty-printer tree [level 0])
  (printf "~a- ~a ~a\n"
          (make-string (* level 2) #\ )
          (cadr tree)
          (if (eq? (car tree) 'dir)
            "(dir)"
            (format "(file, size=~a)" (third tree))))
  (if (eq? (car tree) 'dir)
    (for-each (lambda (c) (pretty-printer c (+ level 1))) (third tree))
    (void)))

(time (begin (define input1 (file->lines "input7-1.txt"))
             (define input2 (file->lines "input7-2.txt"))
             (part1 input1)
             (part1 input2)
             (part2 input1)
             (part2 input2)))
