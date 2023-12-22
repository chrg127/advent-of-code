(define (parse in)
  (list->vector (map (compose list->vector string->list) (file->lines in))))

(define (grid-ref2 g p) (vector-ref (vector-ref g (cadr p)) (car p)))
(define (grid-ref g p)
  (let ([x (car p)] [y (cadr p)] [len (vector-length g)])
    (vector-ref (vector-ref g (modulo y len)) (modulo x len))))

(define (start-pos g [half (quotient (vector-length g) 2)]) (list half half))

(define (get-neighbors g p)
  (filter (lambda (n) (not (char=? (grid-ref g n) #\#)))
          (map (curry map + p) '((1 0) (-1 0) (0 1) (0 -1)))))

(define (walk g ns)
  (remove-duplicates (append* (map (curry get-neighbors g) ns))))

(define (walk-steps g ns steps)
  (foldl (lambda (step ns) (walk g ns)) ns (range steps)))

(define (solve in n)
  (let ([input (parse in)])
    (length (walk-steps input (list (start-pos input)) n))))

(println (solve "input21-1.txt" 6))
(println (solve "input21-2.txt" 64))

;(define (print-grid g ns)
;  (for ([y (in-range (vector-length g))])
;       (for ([x (in-range (vector-length g))])
;            (printf "~a" (if (member (list x y) ns)
;                           #\O
;                           (grid-ref g (list x y)))))
;       (printf "\n")))

