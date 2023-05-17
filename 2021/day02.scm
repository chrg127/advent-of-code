(define (parse in)
  (map (lambda (line)
         (let ((tmp (string-split line)))
  	       (let ((type (car tmp)) (num (cadr tmp)))
             (cond
	           ((string=? type "forward") (cons (string->number num) 'forward))
	           ((string=? type "down")    (cons (string->number num) 'depth))
	           ((string=? type "up")      (cons (- (string->number num)) 'depth))))))
     (port->lines in)))

(define (sol1 name)
  (define (add-up-and-mul forward-list depth-list)
    (* (apply + forward-list) (apply + depth-list)))
  (define-values (a b)
    (partition
      (lambda (x) (eq? (cdr x) 'forward))
      (parse (open-input-file name))))
  (displayln (add-up-and-mul (map car a) (map car b))))

(sol1 "input2-1.txt")
(sol1 "input2-2.txt")

(define (calc-all lst pos aim depth)
  (if (null? lst)
    (list pos depth aim)
    (let ((type (cdr (car lst))) (num (caar lst)))
      (let ((new-aim   (if (eq? type 'depth)   (+ aim num) aim))
            (new-pos   (if (eq? type 'forward) (+ pos num) pos))
            (new-depth (if (eq? type 'forward) (+ depth (* aim num)) depth)))
        (calc-all (cdr lst) new-pos new-aim new-depth)))))

(define (sol2 name)
  (define sol (calc-all (parse (open-input-file name)) 0 0 0))
  (displayln (* (car sol) (cadr sol))))

(sol2 "input2-1.txt")
(sol2 "input2-2.txt")
