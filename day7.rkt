(require racket/trace)

(define (change-dir dir lines cwd)
  (if (string=? dir "..")
    (list 'early-break cwd)
    (let ([new-cwd (car (memf (lambda (c) (and (eq? (car c) 'dir)
                                               (string=? (cadr c) dir)))
                         (third cwd)))])
      (step lines new-cwd))))

(define (parse-cmd line lines cwd)
  (let ([splitted (drop (string-split line " ") 1)])
    (if (not (string=? (car splitted) "cd"))
      (list lines cwd) ; ls commands are ignored
      (change-dir (cadr splitted) lines cwd))))

(define (new-file line)
  (let ([splitted (string-split line " ")])
    (if (string=? (car splitted) "dir")
      (list 'dir (cadr splitted) '())
      (list 'file (cadr splitted) (string->number (car splitted))))))

(define (add-child dir child)
  (printf "add-child ~a ~a\n" dir child)
  (list-update dir 2 (lambda (x) (cons child x))))

(define (command? line)
  (char=? (string-ref line 0) #\$))

(define (parse-line line lines cwd)
  (println line)
  (if (not (command? line))
    (list lines (add-child cwd (new-file line)))
    (parse-cmd line lines cwd)))

(define (step lines cwd)
  (if (empty? lines)
    cwd
    (let ([r (parse-line (car lines) (cdr lines) cwd)])
      (println "checking early break")
      (if (eq? (car r) 'early-break)
        (begin (println "early break") (cadr r))
        (begin (println "apply") (apply step r))))))

(define (part1 input)
  (println (step (cdr input) (list 'dir "/" '()))))

(define input1 (file->lines "input7-1.txt"))
(part1 input1)
