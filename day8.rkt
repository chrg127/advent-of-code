(require srfi/1) ; for zip

(define ((loop-row init collect) row [len (length row)] [pos 0])
  (if (empty? (cdr row))
    (list (init row pos))
    (let* ([res ((loop-row init collect) (cdr row) len (add1 pos))])
      (collect row len pos res))))

(define score-row
  (loop-row (lambda (row pos) (list (caar row) pos 0))
            (lambda (row len pos res)
              (let* ([higher (filter (lambda (x) (>= (car x) (caar row))) res)]
                     [min-pos (if (empty? higher)
                                (sub1 len)
                                (cadr (argmin cadr higher)))])
                (cons (list (caar row) pos (- min-pos pos)) res)))))

(define (find-visibles row)
  (define the-loop
    (loop-row (lambda (row pos) (car row))
              (lambda (row len pos res)
                (if (> (caar row) (apply max (map car res)))
                  (cons (car row) res)
                  res))))
  (map cadr (the-loop row)))

(define (make-grid input)
  (for/list ([i (in-range (length input))])
    (let ([l (string->list (list-ref input i))])
      (zip (map (lambda (c) (- (char->integer c) 48)) l)
           (map (lambda (x) (list x i)) (range (length l)))))))

(define (traspose grid)
  (for/list ([col (in-range (length grid))])
    (map (lambda (v) (list-ref v col)) grid)))

(define (find-all-visibles grid tr)
  (append (apply append (map find-visibles grid))
          (apply append (map (compose find-visibles reverse) grid))
          (apply append (map find-visibles tr))
          (apply append (map (compose find-visibles reverse) tr))))

(define (scenic-score-all grid tr)
  (map *
       (flatten           (map (lambda (r) (map caddr          (score-row          r)))   grid))
       (flatten           (map (lambda (r) (map caddr (reverse (score-row (reverse r))))) grid))
       (flatten (traspose (map (lambda (r) (map caddr          (score-row          r)))   tr)))
       (flatten (traspose (map (lambda (r) (map caddr (reverse (score-row (reverse r))))) tr)))))

(define (part1 grid)
  (println (length (remove-duplicates (find-all-visibles grid (traspose grid))))))

(define (part2 grid)
  (println (apply max (scenic-score-all grid (traspose grid)))))

(time (begin (define input1 (make-grid (file->lines "input8-1.txt")))
             (define input2 (make-grid (file->lines "input8-2.txt")))
             (part1 input1)
             (part1 input2)
             (part2 input1)
             (part2 input2)))
