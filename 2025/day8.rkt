(define (vec+ . args) (apply map + args))
(define (vec- . args) (apply map - args))

(define (dist p q)
  (apply + (map (lambda (x) (* x x)) (vec- p q))))

(define (add-circuits circuits p q)
  (let-values ([(change keep)
                (partition (lambda (s)
                             (or (set-member? s p)
                                 (set-member? s q)))
                           circuits)])
    (cons (apply set-union (set p q) change) keep)))

(define (part1 pairs n)
  (let ([circuits (foldl (lambda (pair circuits)
                            (apply add-circuits circuits pair))
                         '() (take pairs n))])
    (apply * (take (sort (map set-count circuits) >) 3))))

(define (part2 pairs n)
  (let loop ([circuits '()] [pairs pairs])
    (let ([new-circuits (apply add-circuits circuits (car pairs))])
      (if (and (= (length new-circuits) 1)
               (= (set-count (car new-circuits)) n))
          (car pairs)
          (loop new-circuits (cdr pairs))))))

(define (solve name)
  (let* ([in (map (lambda (l) (map string->number (string-split l ",")))
                  (file->lines name))]
         ;; first get distances for each pair, then sort them
         ;; (it's faster than getting distances on demand)
         [pairs (sort (map (lambda (x) (list (car x) (cadr x) (apply dist x)))
                           (combinations in 2))
                      (lambda (x y) (< (caddr x) (caddr y))))]
         ;; get rid of the distances
         [pairs (map (lambda (x) (take x 2)) pairs)])
    (list (part1 pairs (if (= (length in) 20) 10 1000))
          (apply * (map car (part2 pairs (length in)))))))
