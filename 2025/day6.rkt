(define (parse-nums-1 nums)
  (map (lambda (line)
         (map (lambda (num) (string->number (string-trim num))) line))
       nums))

(define (parse-nums-2 nums)
  (apply map (lambda args
               (apply map (lambda args (string->number (string-trim (apply string args))))
                      (map string->list args)))
         nums))

(define (solve name)
  (let* ([lines (file->lines name)]
         [indexes (filter identity (map (lambda (c i)
                                         (if (or (char=? c #\+) (char=? c #\*)) i #f))
                                       (string->list (last lines))
                                       (range (string-length (last lines)))))]
         [indexes (append indexes (list (+ 1 (string-length (first lines)))))]
         [splitted (map (lambda (line)
                          (map (lambda (start end)
                                 (substring line start (- end 1)))
                               (drop-right indexes 1)
                               (drop       indexes 1)))
                        lines)]
         [ops (map (lambda (op) (if (string=? op "+") + *))
                   (string-split (last lines)))])
    (list
      (apply + (apply map (lambda args (eval args))
                      ops (parse-nums-1 (drop-right splitted 1))))
      (apply + (map (lambda args (apply (car args) (cadr args)))
                    ops (parse-nums-2 (drop-right splitted 1)))))))
