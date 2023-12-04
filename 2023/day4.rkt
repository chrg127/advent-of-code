(define (score matches) (floor (expt 2 (sub1 (length matches)))))
(define (find-matches ws nums) (filter (lambda (x) (member x ws)) nums))
(define (vec+ . vs) (apply (curry map +) vs))
(define (vec* n v) (map (curry * n) v))

(define (parse in num-winnings)
  (map (lambda (line)
         (let* ([nums (map string->number (regexp-match* #rx"[0-9]+" line))]
                [winnings (take (cdr nums) num-winnings)]
                [numbers (drop (cdr nums) num-winnings)])
           (list winnings numbers)))
       in))

(define (make-result-vec i len n)
  (foldl (lambda (x r) (list-set r x 1)) (make-list n 0) (range i (+ i len))))

(define (compute-copies in matches [n (length in)])
  (foldl (lambda (x i r)
           (vec+ r (vec* (list-ref r i) (make-result-vec (add1 i) x n))))
         (make-list n 1) matches (range (length matches))))

(define (solve process post-process _in num-winnings [in (file->lines _in)])
  (apply + (post-process in (map (lambda (x) (process (apply find-matches x)))
                                 (parse in num-winnings)))))

(define solve1 (curry solve score (lambda (a b) b)))
(define solve2 (curry solve length compute-copies))

(println (solve1 "input4-1.txt" 5))
(println (solve1 "input4-2.txt" 10))
(println (solve2 "input4-1.txt" 5))
(println (solve2 "input4-2.txt" 10))
