(define (match-nums s) (map string->number (string-split s " ")))
(define (parse in) (map match-nums (file->lines in)))
(define (differences ns) (map - (drop ns 1) (take ns (- (length ns) 1))))

(define (make-hist ns)
  (if (andmap (curry = 0) ns)
    (list ns)
    (cons ns (make-hist (differences ns)))))

(define (solve add which in)
  (apply + (map (lambda (ns) (foldr (lambda (h diff) (add (which h) diff))
                                    0 (make-hist ns))) (parse in))))

(println (solve + last "input9-1.txt"))
(println (solve + last "input9-2.txt"))
(println (solve - first "input9-1.txt"))
(println (solve - first "input9-2.txt"))
