;; input gets converted to numbers
;; where rock is 0, paper is 1 and scissors is 3
(define (parse input)
  (map (lambda (l)
         (list (- (char->integer (string-ref l 0)) 65)
               (- (char->integer (string-ref l 2)) 88))) input))

(define (shape-add x) (modulo (add1 x) 3))
(define (shape-sub x) (modulo (sub1 x) 3))

(define (calc-score opp-shape my-shape)
  (+ (add1 my-shape)                              ;; shape score
     (cond ((= opp-shape my-shape) 3)             ;; a draw
           ((= (shape-add opp-shape) my-shape) 6) ;; a win
           (else 0))))                            ;; a loss

(define (choose-shape opp-shape outcome)
  ((hash-ref (hash 0 shape-sub 1 identity 2 shape-add) outcome) opp-shape))

(define ((total-score get-shape) input)
  (println (apply + (map (lambda (x) (calc-score (car x) (get-shape x))) input))))

(define part1 (total-score cadr))
(define part2 (total-score (lambda (x) (apply choose-shape x))))

(time (begin
        (define input1 (parse (file->lines "input2-1.txt")))
        (define input2 (parse (file->lines "input2-2.txt")))
        (part1 input1)
        (part1 input2)
        (part2 input1)
        (part2 input2)))
