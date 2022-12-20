(define (ints s) (map string->number (regexp-match* "[-?0-9]+" s)))

(define (list-index f l [i 0])
  (cond ((empty? l) #f)
        ((f (car l)) i)
        (else (list-index f (cdr l) (+ 1 i)))))

(define (remove-index i l)
  (cond ((empty? l) '())
        ((= i 0) (cdr l))
        (else (cons (car l) (remove-index (- i 1) (cdr l))))))

(define (add-after v n l)
  (cond ((= n 0) (cons v l))
        ((empty? l) '())
        (else (cons (car l) (add-after v (- n 1) (cdr l))))))

(define (add-between v i j lst)
  (add-after v (if (> i j) (+ i 1) j) lst))

(define (move n from to-1 to-2 nums)
  (if (= from to-2) nums (add-between n to-1 to-2 (remove-index from nums))))

(define (get-pos n i len)
  (list (modulo (+ n i -1) (- len 1)) (modulo (+ n i) (- len 1))))

(define (move-num nums indexes i len)
  (let* ([from (list-index (lambda (x) (= x i)) indexes)]
         [n (list-ref nums from)]
         [to (get-pos n from len)])
    (list (move n from (car to) (cadr to) nums)
          (move i from (car to) (cadr to) indexes))))

(define (mix input [indexes (range (length input))])
  (foldl (lambda (i r) (move-num (car r) (cadr r) i (length input)))
         (list input indexes)
         (range (length input))))

(define ((get-nth nums) n)
  (list-ref nums (modulo (+ n (list-index zero? nums)) (length nums))))
(define (find-grove nums) (apply + (map (get-nth nums) '(1000 2000 3000))))

(define (rounds nums num-rounds)
  (foldl (lambda (i ns) (mix (car ns) (cadr ns)))
         (list nums (range (length nums)))
         (range num-rounds)))

(define (part1 input) (println (find-grove (car (mix input)))))

(define (part2 input)
  (println (find-grove (car (rounds (map (lambda (x) (* x 811589153))
                                         input) 10)))))
(define input1 (ints (file->string "input20-1.txt")))
(define input2 (ints (file->string "input20-2.txt")))
(part1 input1)
(part1 input2)
(part2 input1)
(part2 input2)
