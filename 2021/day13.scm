(define (parse name)
  (let* ([splitted (string-split (file->string name) "\n\n")]
         [folds (map (lambda (l) (list (string-ref l 11)
                                       (string->number (substring l 13 (string-length l)))))
                     (string-split (cadr splitted) "\n"))]
         [points (map (lambda (p) (map string->number (string-split p ",")))
                 (string-split (car splitted) "\n"))])
    (values points folds)))

(define (mat-print mat)
  (let* ([w (apply max (map car mat))] [h (apply max (map cadr mat))])
    (for ([i (in-range (add1 h))])
         (for ([j (in-range (add1 w))])
              (display (if (member (list j i) mat) #\# #\.)))
         (newline))))

(define (fold points axis line)
  (define index (if (char=? axis #\x) 0 1))
  (define-values (a b) (partition (lambda (x) (> (list-ref x index) line)) points))
  (remove-duplicates (append b (map (lambda (x) (list-update x index (lambda (x) (- (* 2 line) x)))) a))))

(define (sol1 name)
  (define-values (points folds) (parse name))
  (displayln (length (fold points (caar folds) (cadr (car folds))))))

(define (sol2 name)
  (define-values (points folds) (parse name))
  (mat-print (foldl (lambda (x r) (fold r (car x) (cadr x))) points folds)))

(sol1 "input13-1.txt")
(sol1 "input13-2.txt")
(sol2 "input13-1.txt")
(sol2 "input13-2.txt")
