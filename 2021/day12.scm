(define (graph-add-path! g p) (begin (hash-update! g (car  p) (lambda (x) (cons (cadr p) x)) (lambda () '()))
                                     (hash-update! g (cadr p) (lambda (x) (cons (car  p) x)) (lambda () '()))))

(define (parse name)
  (define graph (make-hash))
  (for-each (lambda (p) (graph-add-path! graph p))
            (map (lambda (l) (string-split l "-")) (file->lines name)))
  graph)

(define (small? cave) (andmap char-lower-case? (string->list cave)))

(define (visit graph node locked locked? result-fn combine)
  (if (string=? node "end")
    (result-fn node)
    (let* ([new-locked (if (small? node)
                        (hash-update locked node add1 (lambda () 0))
                        locked)]
           [options (filter (lambda (x) (not (locked? x (hash-ref new-locked x (lambda () 0)) new-locked)))
                           (hash-ref graph node))])
      (combine (lambda (n) (visit graph n new-locked locked? result-fn combine)) node options))))

(define (num-paths graph locked?)
  (visit graph "start" (hash) locked?
         (lambda (x) 1)
         (lambda (f node o) (foldl (lambda (n r) (+ r (f n))) 0 o))))

(define (display-paths name locked-pred)
  (define paths (visit (parse name) "start" (hash) locked-pred
                       (lambda (x) (list (list x)))
                       (lambda (f node o) (map (lambda (p) (cons node p))
                                          (apply append (map f o))))))
  (define paths-str
    (sort (map (lambda (p) (apply string-append (map (lambda (n) (format "~a," n)) p))) paths) string<?))
  (for-each (lambda (p) (displayln (substring p 0 (sub1 (string-length p)))))
            paths-str))

(define (sol name print-paths? pred)
  (if print-paths? (display-paths name pred) 'noprint)
  (displayln (num-paths (parse name) pred)))

(define (sol1 name [print-paths? #f]) (sol name print-paths? (lambda (k v t) (= v 1))))
(define (sol2 name [print-paths? #f])
  (sol name print-paths? (lambda (k v t)
                           (or (and (string=? k "start") (= v 1))
                               (>= v (if (findf (lambda (x) (= x 2)) (hash-values t)) 1 2))))))

(sol1 "input12-1.txt" #t)
(sol1 "input12-3.txt")
(sol1 "input12-4.txt")
(sol1 "input12-2.txt")

(sol2 "input12-1.txt")
(sol2 "input12-3.txt")
(sol2 "input12-4.txt")
(sol2 "input12-2.txt")
