(define (parse name)
  (define (make-rule rule-string)
    (let* ([splitted (string-split rule-string " -> ")]
           [from (string->list (car splitted))]
           [to (string-ref (cadr splitted) 0)])
      (list from (list (car from) to) (list to (cadr from)))))
  (let* ([splitted (string-split (file->string name) "\n\n")]
         [template (string->list (car splitted))]
         [rules (map make-rule (string-split (cadr splitted) "\n"))])
    (values template rules)))

(define (make-pair-tab l)
  (define (make-pair-list l)
    (if (or (null? l) (null? (cdr l)))
      '()
      (cons (list (car l) (cadr l)) (make-pair-list (cdr l)))))
  (let ((res (make-hash)))
    (for-each (lambda (x) (hash-update! res x add1 (lambda () 0))) (make-pair-list l))
    res))

(define ((addn n) x) (+ x n))
(define ((subn n) x) (- x n))

(define (step polymer rules)
  (let* ([matching (filter-map (lambda (r) (if (hash-has-key? polymer (car r))
                                             (cons (hash-ref polymer (car r)) r)
                                             #f))
                               rules)]
         [to-subtract (map (lambda (x) (list (first x) (second x))) matching)]
         [to-update   (apply append (map (lambda (x) (list (list (first x) (third x))
                                                           (list (first x) (fourth x))))
                                       matching))])
    (for-each (lambda (p) (hash-update! polymer (cadr p) (addn (car p)) (lambda () 0))) to-update)
    (for-each (lambda (p) (hash-update! polymer (cadr p) (subn (car p))))               to-subtract)
    (define to-remove (filter-map (lambda (x) (if (= (cdr x) 0) (car x) #f)) (hash->list polymer)))
    (for-each (lambda (k) (hash-remove! polymer k)) to-remove)))

(define (count-elems polymer first-letter)
  (define h (make-hash))
  (hash-for-each polymer (lambda (k v) (hash-update! h (cadr k) (addn v) (lambda () 0))))
  (hash-update! h first-letter add1 (lambda () 0))
  h)

(define (sol name steps)
  (define-values (template rules) (parse name))
  (define first-letter (car template))
  (define pair-tab (make-pair-tab template))
  (for ([i (in-range steps)])
       (step pair-tab rules))
  (define counts (count-elems pair-tab first-letter))
  (displayln (- (apply max (hash-values counts)) (apply min (hash-values counts)))))

(sol "input14-1.txt" 10)
(sol "input14-2.txt" 10)
(sol "input14-1.txt" 40)
(sol "input14-2.txt" 40)
