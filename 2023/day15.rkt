(define (do-hash s)
  (foldl (lambda (x r) (remainder (* (+ r (char->integer x)) 17) 256)) 0 (string->list s)))

(define (box=? k x) (string=? (car x) k))

(define (lens-hash-set h k v)
  (list-update h (do-hash k) (lambda (box)
                               (let ([i (index-where box (curry box=? k))])
                                 (if i
                                   (list-set box i (list k v))
                                   (append box (list (list k v))))))))

(define (lens-hash-remove h k)
  (list-update h (do-hash k) (lambda (box) (remf (curry box=? k) box))))

(define (focusing-power h)
  (apply + (map (lambda (box n)
                  (apply + (map (lambda (slot i) (* n i (cadr slot)))
                                box (range 1 (add1 (length box))))))
                h (range 1 (add1 (length h))))))

(define (parse in) (string-split (car (file->lines in)) ","))
(define (solve1 in) (apply + (map do-hash (parse in))))

(define (solve2 in)
  (focusing-power
    (foldl (lambda (s h)
             (if (char=? (last (string->list s)) #\-)
               (lens-hash-remove h (car (string-split s "-")))
               (let ([split (string-split s "=")])
                 (lens-hash-set h (car split) (string->number (cadr split))))))
           (make-list 256 '()) (parse in))))

(println (solve1 "input15-1.txt"))
(println (solve1 "input15-2.txt"))
(println (solve2 "input15-1.txt"))
(println (solve2 "input15-2.txt"))
