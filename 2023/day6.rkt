(define (solve-2order-eq a b c)
  (define (solve f) (/ (f (- b) (sqrt (- (expt b 2) (* 4 a c)))) (* 2 a)))
  (list (solve +) (solve -)))

(define (find-options time best-dist)
  (let ([sols (solve-2order-eq -1 time (- best-dist))])
    (sub1 (ceiling (- (ceiling (cadr sols)) (car sols))))))

(define (parse process in)
  (map (lambda (x) (map string->number (process (regexp-match* #rx"[0-9]+" x))))
       (file->lines in)))

(define (join-nums nums) (list (apply string-append nums)))

(define (solve process in)
  (let ([parsed (parse process in)])
    (apply * (map (lambda (time dist) (find-options time dist))
                  (car parsed) (cadr parsed)))))

(println (solve identity "input6-1.txt"))
(println (solve identity "input6-2.txt"))
(println (solve join-nums "input6-1.txt"))
(println (solve join-nums "input6-2.txt"))

;; older find-options, kept here for history
(define (old-find-options time best-dist)
  (length (filter (lambda (x) (> x best-dist))
                  (map (lambda (h) (* h (- time h)))
                       (range (+ time 1))))))
