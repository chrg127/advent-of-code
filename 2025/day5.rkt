(define (in-range? id a b) (and (>= id a) (<= id b)))

(define (range-in-range? r s)
  (or (apply in-range? (car  r) s)
      (apply in-range? (cadr r) s)
      (and (apply in-range? (car  s) r)
           (apply in-range? (cadr s) r))))

(define (merge r rs)
  (let-values ([(todo keep) (partition (curry range-in-range? r) rs)])
    (cons (foldl (lambda (x r)
                   (let ([args (append r x)])
                     (list (apply min args) (apply max args))))
                 r todo)
          keep)))

(define (solve name)
  (let* ([in (map (lambda (s) (string-split s "\n"))
                  (string-split (file->string name) "\n\n"))]
         [ranges (map (lambda (s) (map string->number (string-split s "-"))) (car in))]
         [ids    (map string->number (cadr in))])
    (list (length (filter (lambda (id)
                            (memf (lambda (r) (apply in-range? id r))
                                  ranges))
                          ids))
          (apply + (map (lambda (r) (+ 1 (- (cadr r) (car r))))
                        (foldl merge '() ranges))))))
