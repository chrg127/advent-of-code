(define (ints s) (map string->number (regexp-match* "[-?0-9]+" s)))
(define (parse input) (map ints input))
(define (array- . args) (apply (curry map -) args))
(define (array-abs . args) (apply (curry map abs) args))
(define (count-n n v) (length (filter (curry = n) v)))
(define (bool->num b) (if b 1 0))

(define (touching? a b)
  (let ([d (array-abs (array- a b))])
    (and (= (count-n 1 d) 1) (= (count-n 0 d) 2))))

(define (count-sides cubes)
  (* 2 (apply + (map (lambda (x) (bool->num (apply touching? x)))
                     (combinations cubes 2)))))

(define (part1 cubes) (println (- (* (length cubes) 6) (count-sides cubes))))

(define input1 (parse (file->lines "input18-1.txt")))
(define input2 (parse (file->lines "input18-2.txt")))
(part1 '((1 1 1) (2 1 1)))
(part1 input1)
(part1 input2)
