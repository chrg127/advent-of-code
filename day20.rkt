(define (ints s) (map string->number (regexp-match* "[-?0-9]+" s)))

(define (list-index f l [i 0])
  (cond ((empty? l) #f)
        ((f (car l)) i)
        (else (list-index f (cdr l) (+ 1 i)))))

(define (move n from to nums [i 0])
  (cond ((= from to) nums)
        ((empty? nums) '())
        ((= i from) (move n from to (cdr nums) (+ i 1)))
        ((= i to) (append (list (car nums) n) (move n from to (cdr nums) (+ i 1))))
        (else (cons (car nums) (move n from to (cdr nums) (+ i 1))))))

(define (get-pos n i len)
  (if (< n 0)
    (modulo (- (+ n i) 1) len)
    (modulo (+ n i) len)))

(define (move-num nums indexes i [len (length nums)])
  (let* ([from (list-index (lambda (x) (= x i)) indexes)]
         [n (list-ref nums from)]
         [to (get-pos n from len)]
         [res (move n from to nums)]
         [is  (move i from to indexes)])
    ; (printf "i = ~a, move ~a from ~a to ~a, res = ~a, is = ~a\n" i n from to res is)
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

; (define test-list1 '(4 5 6 1 7 8 9))
; (define test-list2 '(4 -2 5 6 7 8 9))
(define test-list3 '(4 5 6 7 8 -2 9))
; (println (move 1 3 (get-pos 1 3 7) test-list1))
; (println (move -2 1 (get-pos -2 1 7) test-list2))
(printf "before = ~a\n" test-list3)
(printf "after = ~a\n" (move -2 5 (get-pos -2 5 7) test-list3))

(println (part1 input1))
(println (part1 input2))
