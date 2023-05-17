(define (modulo-1 i m n) (add1 (modulo (sub1 (+ i m)) n)))
(define (roll-3 s) (apply + (map (lambda (x) (modulo-1 s x 100)) (range 1 4))))
(define (roll->num-unis roll) (hash-ref (hash 3 1 4 3 5 6 6 7 7 6 8 3 9 1) roll))
(define (vec+ l m) (list (+ (car l) (car m)) (+ (cadr l) (cadr m))))
(define (vec* l s) (list (* s (car l)) (* s (cadr l))))

(define (deterministic-game pos scores p roll die steps)
  (let* ([new-pos (list-update pos p (lambda (x) (modulo-1 x roll 10)))]
         [new-scores (list-update scores p (lambda (x) (+ x (list-ref new-pos p))))]
         [next-p (if (= p 0) 1 0)])
    (if (>= (list-ref new-scores p) 1000)
      (values new-scores steps)
      (deterministic-game new-pos new-scores next-p (roll-3 die) (modulo-1 die 3 100) (+ 3 steps)))))

(define (dirac-game init-pos)
  (define memo (make-hash))
  (define (loop pos scores p roll)
    (let ([memo-res (hash-ref memo (list pos scores p roll) (lambda () #f))])
      (if memo-res
        memo-res
        (let* ([new-pos (list-update pos p (lambda (x) (modulo-1 x roll 10)))]
               [new-scores (list-update scores p (lambda (x) (+ x (list-ref new-pos p))))]
               [next-p (if (= p 0) 1 0)])
          (cond ((>= (car  new-scores) 21) (list (roll->num-unis roll) 0))
                ((>= (cadr new-scores) 21) (list 0 (roll->num-unis roll)))
                (else
                  (let ([res1 (vec* (foldl (lambda (x r) (vec+ r (loop new-pos new-scores next-p x))) '(0 0) '(3 4 5 6 7 8 9))
                                    (roll->num-unis roll))])
                    (hash-set! memo (list pos scores p roll) res1)
                    res1)))))))
  (foldl (lambda (x r) (vec+ r (loop init-pos '(0 0) 0 x))) '(0 0) '(3 4 5 6 7 8 9)))

(define (sol1 init-pos)
  (define-values (scores steps) (deterministic-game init-pos '(0 0) 0 (roll-3 0) (modulo-1 0 3 100) 3))
  (displayln (* steps (if (>= (car scores) 1000) (cadr scores) (car scores)))))

(define (sol2 positions) (displayln (apply max (dirac-game positions))))

(sol1 '(4 8))
(sol1 '(9 10))
(sol2 '(4 8))
(sol2 '(9 10))
