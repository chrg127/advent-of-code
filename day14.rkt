(define (ints s) (map string->number (regexp-match* "[0-9]+" s)))
(define (make-pairs l)
  (if (empty? l) '() (cons (list (car l) (cadr l)) (make-pairs (cddr l)))))
(define (make-paths l)
  (if (empty? (cdr l)) '() (cons (list (car l) (cadr l)) (make-paths (cdr l)))))

(define (gen-coords l)
  (define (gen a b c d)
    (apply append
           (for/list ([i (in-inclusive-range (min a c) (max a c))])
             (for/list ([j (in-inclusive-range (min b d) (max b d))])
               (list i j)))))
  (apply gen (flatten l)))

(define (parse input)
  (apply append (map (compose make-paths make-pairs ints) input)))
    ;(make-grid (apply append (map (lambda (p) (apply gen-coords (flatten p))) paths1)

(define (between? x a b) (and (<= x (max a b)) (>= x (min a b))))
(define (in-path? p road)
  (and (between? (car p) (caar road) (caadr road))
       (between? (cadr p) (cadar road) (cadadr road))))
(define (in-paths? p l) (ormap (curry in-path? p) l))
(define in-sand? member)

(define (array+ . args) (apply (curry map + ) args))
(define (next-pos p paths sand third-cond)
  (findf (lambda (x) (and (not (in-paths? x paths))
                          (not (set-member? sand x))
                          (third-cond x)))
         (map (curry array+ p) '((0 1) (-1 1) (1 1)))))

(define (sim-one pos paths sand can-finish? third-cond)
  (let ([next (next-pos pos paths sand third-cond)])
    (cond ((can-finish? pos next) 'finish)
          ((not next) (set-add sand pos))
          (else (sim-one next paths sand can-finish? third-cond)))))

(define (sim paths can-finish? third-cond [sand (set)])
  (let ([res (sim-one '(500 0) paths sand can-finish? third-cond)])
    (if (eq? res 'finish)
      sand
      (sim paths can-finish? third-cond res))))

(define (part1 paths max-y)
  (println (set-count (sim paths
                        (lambda (p n) (and n (= (cadr n) max-y)))
                        (lambda (x) #t)))))

(define (part2 paths max-y)
  (println (+ 1 (set-count (sim paths
                             (lambda (p n) (and (not n) (equal? p '(500 0))))
                             (lambda (x) (not (= (cadr x) (+ max-y 2)))))))))


(define (make-grid coords)
  (map list->set (group-by cadr coords)))

(define paths1 (parse (file->lines "input14-1.txt")))
;(define-values (paths2 max-y2) (parse (file->lines "input14-2.txt")))
(println paths1)
;(part1 paths1 max-y1)
;(time (part1 paths2 max-y2))
;(part2 paths1 max-y1)
;(time (part2 paths2 max-y2))
