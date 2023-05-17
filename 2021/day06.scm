(define (parse name)
  (map string->number (string-split (read-line (open-input-file name)) ",")))

(define (lanturn-yield-days first-day yield)
  (map (lambda (x) (+ (* x 7) (modulo (sub1 first-day) 7))) (range yield)))

(define (ceil-div x y) (quotient (sub1 (+ x y)) y))

(define *cache* (make-hash))

(define (lanturn-yield num days)
  (define (lanturn-yield-cache num days)
    (if (< days num)
      0
      (hash-ref *cache* (list num days)
                (lambda ()
                  (let ([res (lanturn-yield-no-check num days)])
                    (hash-set! *cache* (list num days) res)
                    res)))))

  (define (lanturn-yield-no-check num days)
    (let* ([first-yield-day (- days num)]
           [yield (ceil-div first-yield-day 7)]
           [yield-days (lanturn-yield-days first-yield-day yield)]
           [yield-children (map (lambda (x) (lanturn-yield-cache 8 x)) yield-days)])
      (+ yield (apply + yield-children))))

  (add1 (lanturn-yield-cache num days)))

(define (sol name days)
  (displayln (apply + (map (lambda (x) (lanturn-yield x days)) (parse name)))))

(sol "input6-1.txt" 80)
(sol "input6-2.txt" 80)
(sol "input6-1.txt" 256)
(sol "input6-2.txt" 256)
