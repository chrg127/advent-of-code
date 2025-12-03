(define (find-joltage-2 digits n)
  (define results (make-hash))
  (define (loop digits n)
    (cond [(or (= n 0) (empty? digits)) '()]
          [(hash-has-key? results (list digits n)) (hash-ref results (list digits n))]
          [else (let* ([digit (car digits)]
                       [res1 (cons digit (loop (cdr digits) (- n 1)))] ; took the digit
                       [res2 (loop (cdr digits) n)]                    ; didn't take it
                       [res (argmax (lambda (x)
                                      (let ([num (string->number (list->string x))])
                                        (if num num 0)))
                                    (list res1 res2))])
                  (hash-set! results (list digits n) res)
                  res)]))
  (string->number (list->string (loop digits n))))

(define (solve name)
  (let* ([in (map string->list (file->lines name))]
         [joltages (map (lambda (ds) (find-joltage-2 ds 2)) in)]
         [joltages2 (map (lambda (ds) (find-joltage-2 ds 12)) in)])
    (list (apply + joltages)
          (apply + joltages2))))
