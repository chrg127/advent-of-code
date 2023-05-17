(define (print x) (begin (display x) (newline)))

(define (read-line-by-line file)
  (let ((line (read-line file)))
    (if (eof-object? line)
      (begin
        (close-input-port file)
        '())
      (cons (string->number line) (read-line-by-line file)))))

(define (get-diffs lst mst)
  (if (eq? lst '()) '()
    (cons (- (car lst) (car mst)) (get-diffs (cdr lst) (cdr mst)))))

; part 1
(define (get-res lst n) (length (filter (lambda (x) (= x n)) lst)))

(define inlist (sort (call-with-input-file "input10.txt" read-line-by-line) <))
(define res (get-diffs (append inlist (list (+ (last inlist) 3))) (cons 0 inlist)))
(define full-list (cons 0 (append inlist (list (+ (last inlist) 3)))))

; part 2
; n = number to reach, lst = numbers we can use to reach n
(define (possibilities lst n)
  (cond ((eq? lst '()) '())
        ((<= (- n (car lst)) 3) (- (expt 2 (length lst)) 1))
        (else (* 2 (possibilities (cdr lst) n)))))

; pretty messy
; collect a group until (car lst) - n <= 3, then run (possibilities) and multiply to product
; after that, collect a new group, starting from (last group)
; an explanation of the algorithm does not fit the page
(define (get-res2 lst group n product)
  (cond ((eq? lst '()) product)
        ((<= (- (car lst) n) 3)
         (get-res2 (cdr lst)
                (append group (list (car lst)))
                n
                product))
        (else (get-res2 lst
                     '()
                     (last group)
                     (* product (possibilities group (car lst)))))))

(* (get-res res 3) (get-res res 1))
(get-res2 (cdr full-list) '() (car full-list) 1)
