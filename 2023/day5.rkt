(define (match-nums l) (map string->number (regexp-match* #rx"[0-9]+" l)))

(define (parse in)
  (let* ([split (string-split in "\n\n")])
    (list (match-nums (car split))
          (map (lambda (sect) (map match-nums (cdr (string-split sect "\n"))))
               (cdr split)))))

(define (x-to-y seed seed-map)
  (let ([the-line (findf (lambda (line) (and (>= seed (cadr line))
                                             (<= seed (apply + (cdr line)))))
                         seed-map)])
    (if the-line
      (+ seed (- (car the-line) (cadr the-line)))
      seed)))

(define (solve1 in)
  (let* ([parsed (parse (file->string in))]
         [seeds (car parsed)]
         [maps (cadr parsed)])
    (apply min (foldl (lambda (seed-map seeds)
                        (map (lambda (seed) (x-to-y seed seed-map)) seeds))
                      seeds maps))))

;; a few cases to consider:
;;
;; |    |    map
;;    |    | seed
;;
;; |       | map
;;     |   | seed
;;
;; |       | map
;; |       | seed
;;
;;     |   | map
;; |       | seed
;;
;;     |   | map
;; |     |   seed
;;
;;     |   | map
;; |  |      seed
;;
;; |    |    map
;; |       | seed

;; a map is (dest start end)
(define (next2 seed-range line)
  (map (curry + (- (first line) (second line))) seed-range))

;; generate ranges based on seed (a-b) and map (x-y).
;; assume that the pairs are structured as (start-number end-number)
;; and not (start-number length)
;; range inside map is put at the start of the list.
(define (generate m s)
  (let ([a (car s)] [b (cadr s)] [x (cadr m)] [y (caddr m)])
    (cond ((and (< b x))         (list '() (list a b)))
          ((and (> a y))         (list '() (list a b)))
          ((and (> a x) (< b y)) (list (next2 (list a b) m)))
          ((and (> a x) (= b y)) (list (next2 (list a b) m)))
          ((and (> a x) (> b y)) (list (next2 (list a y) m) (list y b)))
          ((and (= a x) (< b y)) (list (next2 (list a b) m)))
          ((and (= a x) (= b y)) (list (next2 (list a b) m)))
          ((and (= a x) (> b y)) (list (next2 (list a y) m) (list y b)))
          ((and (< a x) (< b y)) (list (next2 (list x b) m) (list a x)))
          ((and (< a x) (= b y)) (list (next2 (list x b) m) (list a x)))
          ((and (< a x) (> b y)) (list (next2 (list x y) m) (list a x) (list y b)))
          (else 'unreachable))))

(define (x-to-y2 seed-range seed-map)
  (filter (lambda (x) (not (null? x)))
          (apply append
                 (foldl (lambda (line r)
                          (let* ([seeds (car r)]
                                 [results (cadr r)]
                                 [gen (map (curry generate line) seeds)]
                                 [new-r (map car gen)]
                                 [new-s (apply append (map cdr gen))])
                            (list new-s (append new-r results))))
                        (list (list seed-range) '()) seed-map))))

(define (make-pairs nums)
  (if (null? nums) '()
    (cons (list (car nums) (+ (car nums) (cadr nums)))
          (make-pairs (cddr nums)))))

(define (make-map nums) (list-set nums 2 (+ (cadr nums) (caddr nums))))

(define (parse2 in)
  (let* ([split (string-split in "\n\n")])
    (list (make-pairs (match-nums (car split)))
          (map (lambda (sect) (map (compose make-map match-nums) (cdr (string-split sect "\n"))))
               (cdr split)))))

(define (solve2 in)
  (let* ([parsed (parse2 (file->string in))]
         [seeds (car parsed)]
         [maps (cadr parsed)]
         [locs (foldl (lambda (seed-map seeds)
                        ; (printf "seeds = ~a\n" seeds)
                        (apply append (map (lambda (seed) (x-to-y2 seed seed-map)) seeds)))
                      seeds maps)])
    (caar (sort locs (lambda (x y) (< (car x) (car y)))))))

(println (solve1 "input5-1.txt"))
(println (solve1 "input5-2.txt"))
(println (solve2 "input5-1.txt"))
(println (solve2 "input5-2.txt"))
