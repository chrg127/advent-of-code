(define (match-nums l) (map string->number (regexp-match* #rx"[0-9]+" l)))

(define (next seed-range line)
  (map (curry + (- (first line) (second line))) seed-range))

(define (x-to-y seed seed-map)
  (let ([the-line (findf (lambda (line)
                           (and (>= seed (cadr line)) (<= seed (caddr line))))
                         seed-map)])
    (if the-line
      (car (next (list seed) the-line))
      seed)))

;; generate new ranges based on seed range and line
;; assume that the pairs are structured as (start-number end-number)
;; and not (start-number length)
;; the range inside the line is put at the start of the list.
(define (generate l s)
  (let ([a (car s)] [b (cadr s)] [x (cadr l)] [y (caddr l)])
    (cond ((and (< b x))         (list '() (list a b)))
          ((and (> a y))         (list '() (list a b)))
          ((and (> a x) (<= b y)) (list (next (list a b) l)))
          ((and (> a x) (>  b y)) (list (next (list a y) l) (list y b)))
          ((and (= a x) (<= b y)) (list (next (list a b) l)))
          ((and (= a x) (=  b y)) (list (next (list a b) l)))
          ((and (= a x) (>  b y)) (list (next (list a y) l) (list y b)))
          ((and (< a x) (<= b y)) (list (next (list x b) l) (list a x)))
          ((and (< a x) (>  b y)) (list (next (list x y) l) (list a x) (list y b)))
          (else 'unreachable))))

(define (x-to-y2 seed-range seed-map)
  (filter (lambda (x) (not (null? x)))
          (apply append
                 (foldl (lambda (line r)
                          (let* ([gen (map (curry generate line) (car r))])
                            (list (apply append (map cdr gen))
                                  (append (map car gen) (cadr r)))))
                        (list (list seed-range) '()) seed-map))))

(define (make-pairs nums)
  (if (null? nums) '()
    (cons (list (car nums) (+ (car nums) (cadr nums)))
          (make-pairs (cddr nums)))))

(define (make-map nums) (list-set nums 2 (+ (cadr nums) (caddr nums))))

(define (parse process in)
  (let ([split (string-split in "\n\n")])
    (list (process (match-nums (car split)))
          (map (lambda (sect) (map (compose make-map match-nums)
                                   (cdr (string-split sect "\n"))))
               (cdr split)))))

(define (solve1 in)
  (let ([parsed (parse identity (file->string in))])
    (apply min (foldl (lambda (seed-map seeds)
                        (map (lambda (seed) (x-to-y seed seed-map)) seeds))
                      (car parsed) (cadr parsed)))))

(define (solve2 in)
  (let ([parsed (parse make-pairs (file->string in))])
    (apply min (map car (foldl (lambda (seed-map seeds)
                                 (apply append (map (lambda (seed)
                                                      (x-to-y2 seed seed-map))
                                                    seeds)))
                               (car parsed) (cadr parsed))))))

(println (solve1 "input5-1.txt"))
(println (solve1 "input5-2.txt"))
(println (solve2 "input5-1.txt"))
(println (solve2 "input5-2.txt"))
