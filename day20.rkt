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
  (if (= from to-2)
    nums
    (add-between n to-1 to-2 (remove-index from nums))))

(define (get-pos n i len)
  (list (modulo (+ n i -1) (- len 1))
        (modulo (+ n i) (- len 1))))

(define (move-num nums indexes i len)
  (let* ([from (list-index (lambda (x) (= x i)) indexes)]
         [n (list-ref nums from)]
         [to (get-pos n from len)]
         [res (move n from (car to) (cadr to) nums)]
         [is  (move i from (car to) (cadr to) indexes)])
    ;(printf "i = ~a, move ~a from ~a to ~a, res = ~a, is = ~a\n" i n from to res is)
    (list res is)))

(define (step-n input [indexes (range (length input))]
                [steps (length input)] [len (length input)])
  (foldl (lambda (i r)
           (let ([nums (car r)] [indexes (cadr r)])
             (move-num nums indexes i len)))
         (list input indexes)
         (range steps)))

(define (get-nth n nums)
  (let ([zero-index (list-index zero? nums)])
    (list-ref nums (modulo (+ n zero-index) (length nums)))))

(define (find-grove nums)
  (+ (get-nth 1000 nums) (get-nth 2000 nums) (get-nth 3000 nums)))

(define (part1 input)
  (println (find-grove (car (step-n input)))))

(define (rounds nums num-rounds)
  (foldl (lambda (i ns)
           ; (printf "round ~a: ~a\n" i ns)
           (step-n (car ns) (cadr ns)))
         (list nums (range (length nums)))
         (range num-rounds)))

(define (part2 input)
  (let ([decrypted (map (lambda (x) (* x 811589153)) input)])
    (println (find-grove (car (rounds decrypted 10))))))

(define input1 (ints (file->string "input20-1.txt")))
(define input2 (ints (file->string "input20-2.txt")))
(part1 input1)
(part1 input2)
(part2 input1)
(part2 input2)
