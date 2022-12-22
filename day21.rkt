(define (make-pair s)
  (let ([splitted (string-split s " ")])
    (list (substring (car splitted) 0 4) (cdr splitted))))
(define (parse input) (apply hash (apply append (map make-pair input))))

(define (is-number? expr) (= (length expr) 1))
(define (make-number expr) (string->number (car expr)))
(define (is-var? expr) (string=? (car expr) "x"))
(define (make-var expr) 'x)
(define (make-op op) (hash-ref (hash "+" '+ "-" '- "*" '* "/" '/ "=" '=) op))
(define (inverse-of op) (hash-ref (hash '+ '- '- '+ '* '/ '/ '*) op))

(define (decode key jobs)
  (let ([expr (hash-ref jobs key)])
    (cond ((is-var? expr) (make-var expr))
          ((is-number? expr) (make-number expr))
          (else (list (make-op (cadr expr))
                      (decode (car expr) jobs)
                      (decode (caddr expr) jobs))))))

(define (constant-folding tree)
  (if (or (number? tree) (symbol? tree))
    tree
    (let ([l (constant-folding (cadr  tree))]
          [r (constant-folding (caddr tree))])
      (if (or (symbol? l) (list? l) (symbol? r) (list? r) (eq? (car tree) =))
        (list (car tree) l r)
        ((eval (car tree)) l r)))))

(define (eval-inv op var const order)
  (cond ((or (eq? op '+) (eq? op '*)) ((eval (inverse-of op)) var const))
        ((and (eq? op '-) (eq? order 'left)) (+ (- var) const))
        ((and (eq? op '-) (eq? order 'rite)) (+ var const))
        ((and (eq? op '/) (eq? order 'left)) (/ const var))
        ((and (eq? op '/) (eq? order 'rite)) (* var const))
        (else (error "invalid values: eval-inv"))))

(define (find-answer tree)
  (define (loop tree n)
    (let* ([operands (if (number? (cadr tree)) (cdr tree) (reverse (cdr tree)))]
           [order (if (number? (cadr tree)) 'left 'rite)]
           [res (eval-inv (car tree) n (car operands) order)])
      (if (symbol? (cadr operands))
        res
        (loop (cadr operands) res))))
  (apply loop (if (number? (cadr tree)) (reverse (cdr tree)) (cdr tree))))

(define (update-input h)
  (hash-update (hash-set h "humn" '("x")) "root"
               (lambda (x) (list (car x) "=" (caddr x)))))

(define (update-x tree n)
  (cond ((symbol? tree) n)
        ((number? tree) tree)
        (else (list (car tree)
                    (update-x (cadr tree) n)
                    (update-x (caddr tree) n)))))

(define (part1 input) (println (eval (decode "root" input))))
(define (part2 input)
  (println (find-answer (constant-folding (decode "root" (update-input input))))))

(define input1 (parse (file->lines "input21-1.txt")))
(define input2 (parse (file->lines "input21-2.txt")))
(part1 input1)
(part1 input2)
(part2 input1)
(part2 input2)
