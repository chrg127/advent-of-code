(define (ints s) (map string->number (regexp-match* "[-?0-9]+" s)))

(define (list-index f l [i 0])
  (cond ((empty? l) #f)
        ((f (car l)) i)
        (else (list-index f (cdr l) (+ 1 i)))))

(define (remove-index i lst)
  (cond ((empty? lst) '())
        ((= i 0) (cdr lst))
        (else (cons (car lst) (remove-index (- i 1) (cdr lst))))))

(define (add-index v i lst)
  (cond ((= i 0) (cons v lst))
        ((empty? lst) '())
        (else (cons (car lst) (add-index v (- i 1) (cdr lst))))))

(define (add-between v i j lst)
  (if (> i j) ; we are on the border (this is seriously the solution?)
    (add-index v (+ i 1) lst)
    (add-index v j lst)))

(define (move n from to-1 to-2 nums)
  (add-between n to-1 to-2 (remove-index from nums)))

(define (get-pos n i len)
  (list (modulo (+ n i -1) (- len 1))
        (modulo (+ n i) (- len 1))))

(define (move-num nums indexes i [len (length nums)])
  (let* ([from (list-index (lambda (x) (= x i)) indexes)]
         [n (list-ref nums from)]
         [to (get-pos n from len)]
         [res (move n from (car to) (cadr to) nums)]
         [is  (move i from (car to) (cadr to) indexes)])
    (list res is)))

(define (step-n input [steps (length input)])
  (foldl (lambda (i r)
           (let ([nums (car r)] [indexes (cadr r)])
             (move-num nums indexes i steps)))
         (list input (range (length input)))
         (range steps)))

(define (get-nth n nums)
  (let ([zero-index (list-index zero? nums)])
    (list-ref nums (modulo (+ n zero-index) (length nums)))))

(define (part1 input)
  (let* ([result (car (step-n input))]
         [one   (get-nth 1000 result)]
         [two   (get-nth 2000 result)]
         [three (get-nth 3000 result)])
    (printf "~a ~a ~a\n" one two three)
    (+ one two three)))

(define input1 (ints (file->string "input20-1.txt")))
(define input2 (ints (file->string "input20-2.txt")))
(println (part1 input1))
(println (part1 input2))
