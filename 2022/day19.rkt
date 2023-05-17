(define (ints s) (map string->number (regexp-match* "[-?0-9]+" s)))
(define (arrayify x i) (build-list 4 (lambda (j) (if (= i j) x 0))))
(define (arrayify-2 x y i j)
  (build-list 4 (lambda (k) (cond ((= k i) x) ((= k j) y) (else 0)))))
(define (array+ . args) (apply (curry map +) args))
(define (array- . args) (apply (curry map -) args))
(define (array-id n i) (build-list n (lambda (j) (if (= i j) 1 0))))

(define (parse input)
  (map (lambda (ns) (list (car ns)
                         (arrayify (second ns) 0)
                         (arrayify (third ns) 0)
                         (arrayify-2 (fourth ns) (fifth ns) 0 2)
                         (arrayify-2 (sixth ns) (seventh ns) 0 3)))
       (map ints input)))

;; for each resource type, check if we can build a robot.
;; if we can, we make a new state
;; only one robot can be constructed at any given minute,
;; so we do not need to worry about constructing two bots at the same time

;; cost is an array
(define (can-build? resources cost)
  (andmap (lambda (x) (>= x 0)) (array- resources cost)))

(define (build-loop resources robots costs [i 0])
  (append (list (list (array+ resources robots) robots))
          (map (lambda (i)
                 (let ([robot-cost (list-ref costs i)])
                   (if (can-build? resources robot-cost)
                     (list (array+ robots (array- resources robot-cost))
                           (array+ robots (array-id 4 i)))
                     '())))
               (range 4))))

(define (gen-states costs state)
  (let ([possible-states (build-loop (car state) (cadr state) costs)])
    possible-states))

(define (filter-states states costs)
  (filter (lambda (state)
            (not (equal? state '())))
          states))

(define (minute-loop init-state costs)
  (foldl (lambda (minute states)
           (println minute)
           (let ([new-states (map (curry gen-states costs) states)])
             (filter-states (apply append new-states) costs)))
         init-state
         (range 24)))

(define input1 (parse (file->lines "input19-1.txt")))
(define input2 (parse (file->lines "input19-1.txt")))
(define bp-test (car input1))
(println bp-test)
(println (minute-loop '(((0 0 0 0) (1 0 0 0))) (cdr bp-test)))
; (println (minute-loop '(((1 0 0 0) (1 0 0 0))) (cdr bp-test)))
; (println (minute-loop '(((2 0 0 0) (1 0 0 0))) (cdr bp-test)))
