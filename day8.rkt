(require srfi/1) ; for zip

(define (find-visibles lst [max-tree -1] [visibles '()])
  (if (empty? lst)
    visibles
    (let* ([tree-value (caar lst)])
      (if (> tree-value max-tree)
        (find-visibles (cdr lst) tree-value (cons (cadr (car lst)) visibles))
        (find-visibles (cdr lst) max-tree visibles)))))

(define (make-grid input)
  (for/list ([i (in-range (length input))])
    (let ([l (string->list (list-ref input i))])
      (zip (map (lambda (c) (- (char->integer c) 48)) l)
           (map (lambda (x) (list x i)) (range (length l)))))))

(define (traspose grid)
  (for/list ([col (in-range (length grid))])
    (map (lambda (v) (list-ref v col)) grid)))

(define (find-all-visibles grid)
  (let ([t (traspose grid)])
    (append (apply append (map find-visibles grid))
            (apply append (map (compose find-visibles reverse) grid))
            (apply append (map find-visibles t))
            (apply append (map (compose find-visibles reverse) t)))))

(define (part1 grid)
  (println (length (remove-duplicates (find-all-visibles grid)))))

(define input1 (make-grid (file->lines "input8-1.txt")))
(define input2 (make-grid (file->lines "input8-2.txt")))
(part1 input1)
(part1 input2)
