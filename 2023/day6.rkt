(define (solve-2order-eq a b c)
  (define (solve f) (/ (f (- b) (sqrt (- (expt b 2) (* 4 a c)))) (* 2 a)))
  (list (solve +) (solve -)))

(define (find-options time best-dist)
  (let ([sols (solve-2order-eq -1 time (- best-dist))])
    (sub1 (ceiling (- (ceiling (cadr sols)) (car sols))))))

;; older find-options, kept here for history
(define (old-find-options time dist)
  (length (filter (lambda (x) (> (* x (- time x)) dist)) (range (+ time 1)))))

(define (parse process in)
  (map (lambda (x) (map string->number (process (regexp-match* #rx"[0-9]+" x))))
       (file->lines in)))

(define (join-nums nums) (list (apply string-append nums)))

(define (solve process in)
  (apply * (apply (curry map find-options) (parse process in))))

(println (solve identity "input6-1.txt"))
(println (solve identity "input6-2.txt"))
(println (solve join-nums "input6-1.txt"))
(println (solve join-nums "input6-2.txt"))
