(define (ints s) (map string->number (regexp-match* "[-?0-9]+" s)))
(define (parse input) (map ints input))
(define (array- . args) (apply (curry map -) args))
(define (array-abs . args) (apply (curry map abs) args))
(define (count-n v n) (length (filter (curry = n) v)))
(define (bool->num b) (if b 1 0))

(define (touching? a b)
  (let ([d (array-abs (array- a b))])
    (and (= (count-n d 1) 1) (= (count-n d 0) 2))))

(define (count-sides cubes)
  (* 2 (apply + (map (lambda (x) (bool->num (apply touching? x)))
                     (combinations cubes 2)))))

(define (count-exterior cubes) (- (* (length cubes) 6) (count-sides cubes)))
(define (part1 input) (println (count-exterior input)))

(define x-dim car)
(define y-dim cadr)
(define z-dim caddr)
(define (dim-filter cubes dim n) (filter (lambda (c) (= (dim c) n)) cubes))
(define (get-dim dim cubes) (remove-duplicates (map dim cubes)))
(define (mat-rot m) (list (map car m) (map cadr m) (map caddr m)))
(define (min-max l) (list (apply min l) (apply max l)))
(define (inside? x a b) (and (< x b) (> x a)))

(define (find-gaps nums)
  (define (loop cur nums)
    (cond ((empty? nums) '())
          ((>= (- (car nums) cur) 2)
           (append (range (+ 1 cur) (car nums))
                    (loop (car nums) (cdr nums))))
          (else (loop (car nums) (cdr nums)))))
  (if (empty? nums)
    '()
    (let ([s (sort nums <)])
      (loop (car s) (cdr s)))))

(define ((inside-3d? extremes) c)
  (and (inside? (x-dim c) (car (x-dim extremes)) (cadr (x-dim extremes)))
       (inside? (y-dim c) (car (y-dim extremes)) (cadr (y-dim extremes)))
       (inside? (z-dim c) (car (z-dim extremes)) (cadr (z-dim extremes)))))

(define (scan-dim cubes [cubes-t (mat-rot cubes)] [extremes (map min-max cubes-t)])
  (let ([xs (get-dim x-dim cubes)])
    (apply append (map (lambda (x)
           (let* ([cubes-y (dim-filter cubes x-dim x)]
                  [ys (get-dim y-dim cubes-y)])
             (printf "x = ~a\n" x)
             (apply append (map (lambda (y)
                    (let* ([zs (get-dim z-dim (dim-filter cubes-y y-dim y))]
                           [gaps (find-gaps zs)])
                      (printf "  y = ~a, zs = ~a, gaps = ~a\n" y (sort zs <) gaps)
                      (filter (inside-3d? extremes)
                              (map (curry list x y) gaps)))) ys))))
         xs))))

(define (part2 cubes)
  (let ([air (scan-dim cubes)])
    (println air)
    (println (- (count-exterior cubes) (count-exterior air)))))

(define input1 (parse (file->lines "input18-1.txt")))
(define input2 (parse (file->lines "input18-2.txt")))
(define input3 (parse (file->lines "input18-3.txt")))
;(part1 '((1 1 1) (2 1 1)))
;(part1 input1)
;(part2 input1)
; (part1 input2)
;(part1 input3)
;(part2 input3)
(part2 input2)