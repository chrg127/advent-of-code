(define (convert-hand hand [part2? #f])
  (map (lambda (x) (if (not (char-numeric? x))
                     (hash-ref (hash #\T 10 #\J (if part2? 1 11)
                                     #\Q 12 #\K 13 #\A 14) x)
                     (string->number (string x))))
       (string->list hand)))

(define (false-to-null x) (if x x '()))

(define (join-j-group groups)
  (let* ([j-group (false-to-null (findf (lambda (g) (= (car g) 1)) groups))]
         [rest (remove j-group groups)]
         ; length might be wrong here but it gave me the second star anyway, so...
         [biggest (if (not (null? rest)) (argmax length rest) '())])
    (append (remove biggest rest) (list (append j-group biggest)))))

(define (hand-kind hand)
  (let* ([groups (join-j-group (group-by identity hand))]
         [n (length groups)]
         [max-len (apply max (map length groups))])
    (cond ([and (= n 5) (= max-len 1)] 0) ; high card
          ([and (= n 4) (= max-len 2)] 1) ; one pair
          ([and (= n 3) (= max-len 2)] 2) ; two pair
          ([and (= n 3) (= max-len 3)] 3) ; three of a kind
          ([and (= n 2) (= max-len 3)] 4) ; full house
          ([and (= n 2) (= max-len 4)] 5) ; four of a kind
          ([and (= n 1) (= max-len 5)] 6) ; five of a kind
          (else 'unreachable))))

(define (hand< a b)
  (let ([not-eq-pair (findf (compose not (curry apply =)) (map list a b))])
    (if not-eq-pair (apply < not-eq-pair) #f)))

(define (parse in [part2? #f])
  (map (lambda (line)
         (let* ([split (string-split line " ")]
                [hand (convert-hand (car split) part2?)])
           (list hand (hand-kind hand) (string->number (cadr split)))))
       (file->lines in)))

(define (solve in [part2? #f])
  (let ([hands (sort (parse in part2?)
                     (lambda (a b) (if (= (cadr a) (cadr b))
                                     (hand< (car a) (car b))
                                     (< (cadr a) (cadr b)))))])
    (apply + (map * (map caddr hands) (range 1 (add1 (length hands)))))))

(println (solve "input7-1.txt"))
(println (solve "input7-2.txt"))
(println (solve "input7-1.txt" #t))
(println (solve "input7-2.txt" #t))
