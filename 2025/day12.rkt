(define (solve name)
  (length
   (filter (lambda (r)
             (let* ([nums (map string->number (regexp-match* #rx"[0-9]+" r))])
               (<= (apply + (map (lambda (n) (* n 9)) (drop nums 2)))
                   (apply * (map (lambda (x) (- x (modulo x 3))) (take nums 2))))))
           (string-split (last (string-split (file->string name) "\n\n")) "\n"))))
