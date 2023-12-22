(define (parse in)
  (map (lambda (line)
         (map (lambda (p) (map string->number (string-split p ",")))
              (string-split line "~")))
       (file->lines in)))

(define mins first)
(define maxs second)

(define (overlap? a b)
  (andmap
    (lambda (a-min a-max b-min b-max) (and (>= a-max b-min) (<= a-min b-max)))
    (mins a) (maxs a) (mins b) (maxs b)))

(define (move-brick brick)
  (map (lambda (x) (list-update x 2 sub1)) brick))

(define (set-z brick max-z)
  (let ([diff (- (third (mins brick)) (add1 max-z))])
    (if (> diff 1)
      (map (lambda (x) (list-update x 2 (lambda (v) (- v diff)))) brick)
      brick)))

(define (find-supporting brick bricks max-z)
  (define (loop brick)
    (if (= (third (mins brick)) 1)
      (values brick '())
      (let* ([moved (move-brick brick)]
             [supporting (filter (curry overlap? moved) bricks)])
        (if (not (empty? supporting))
          (values brick supporting)
          (loop moved)))))
  (loop (set-z brick max-z)))

(define (put-graph graph brick supporting)
  (hash-set graph brick supporting))

(define (put-graph2 graph brick supporting)
  (foldl (lambda (b g) (hash-update g b (lambda (x) (cons brick x)) '()))
         (hash-set graph brick '()) supporting))

(define (make-graph bricks)
  (let* ([sorted (sort bricks (lambda (a b) (< (third (mins a))
                                               (third (mins b)))))])
    (foldl (lambda (brick i r)
             (let ([processed (car r)] [graph (cadr r)]
                   [graph2 (third r)]  [max-z (fourth r)])
               (let-values ([(new-brick supporting)
                             (find-supporting brick processed max-z)])
                 (list (cons new-brick processed)
                       (put-graph graph new-brick supporting)
                       (put-graph2 graph2 new-brick supporting)
                       (max max-z (third (maxs new-brick)))))))
           (list '() (hash) (hash) 0) sorted (range (length sorted)))))

(define (get-non-disintegrating g)
  (remove-duplicates
    (map car (filter (lambda (x) (= (length x) 1))
                     (hash-values (cadr g))))))

(define (solve1 in)
  (let ([g (make-graph (parse in))])
    (- (length (car g))
       (length (get-non-disintegrating g)))))

(define (walk-graph g node)
  (define visited (mutable-set))
  (define (loop n)
    (unless (set-member? visited n)
      (set-add! visited n)
      (for-each loop (hash-ref g n))))
  (loop node)
  (sub1 (set-count visited)))

(define (solve2 in)
  (let ([g (make-graph (parse in))])
    (apply + (map (curry walk-graph (third g))
                  (get-non-disintegrating g)))))

(println (solve1 "input22-1.txt"))
(println (solve1 "input22-2.txt"))
(println (solve2 "input22-1.txt"))
(println (solve2 "input22-2.txt"))
