(define (parse input)
  (map (lambda (l) (list (string-ref l 0)
                         (string->number (substring l 2)))) input))

(define (array+ . args) (apply (curry map +) args))
(define (array- . args) (apply (curry map -) args))
(define (make-move instr)
  (hash-ref (hash #\R '( 1  0) #\L '(-1  0) #\U '( 0 -1) #\D '( 0  1)) instr))
(define (touching? h t)
  (andmap (lambda (x) (<= x 1)) (map abs (array- h t))))
(define (normalize x) (cond ((> x 1) 1) ((< x -1) -1) (else x)))
(define (move-tail t h) (array+ t (map normalize (array- h t))))

(define (move-rope rope move)
  (if (empty? (cdr rope)) ; head?
    (list (array+ (car rope) move)) ; move head
    (let* ([rest (move-rope (cdr rope) move)]
           [t (car rope)] [h (car rest)])
      (cons (if (touching? h t) t (move-tail t h)) rest))))

(define (move-in-steps data move steps)
  (foldl (lambda (x r)
           (let ([rope (move-rope (car r) (make-move move))])
             (list rope (set-add (cadr r) (car rope)))))
         data
         (range steps)))

(define (apply-instrs instrs rope-length)
  (foldl (lambda (x r) (move-in-steps r (car x) (cadr x)))
         (list (build-list rope-length (lambda (x) '(0 4))) (set))
         instrs))

(define ((solve len) input)
  (println (set-count (second (apply-instrs input len)))))
(define part1 (solve 2))
(define part2 (solve 10))

(time (begin (define input1 (parse (file->lines "input9-1.txt")))
             (define input2 (parse (file->lines "input9-2.txt")))
             (define input3 (parse (file->lines "input9-3.txt")))
             (part1 input1)
             (part1 input2)
             (part2 input1)
             (part2 input3)
             (part2 input2)))
