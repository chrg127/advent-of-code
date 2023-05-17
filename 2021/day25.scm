(define (normalize pos w h) (list (modulo (car  pos) h) (modulo (cadr pos) w)))
(define (inc-right pos w h) (normalize (list-update pos 1 add1) w h))
(define (inc-down  pos w h) (normalize (list-update pos 0 add1) w h))

(define (blocked? mat pos w h incfn)
  (let* ([val (hash-ref mat (incfn pos w h) #\.)])
    (or (char=? val #\>) (char=? val #\v))))

(define (parse name)
  (let* ([lines (file->lines name)]
         [width (string-length (car lines))]
         [height (length lines)]
         [mat (make-hash)])
    (for ([i (in-range height)])
      (for ([j (in-range width)])
        (let ([val (string-ref (list-ref lines i) j)])
          (if (char=? val #\.)
            (void)
            (hash-set! mat (list i j) val)))))
    (values mat width height)))

(define (move mat w h symbol)
  (define new (make-hash))
  (define incfn (if (char=? symbol #\>) inc-right inc-down))
  (hash-for-each mat
    (lambda (k v)
      (let ([new-pos (if (and (char=? v symbol) (not (blocked? mat k w h incfn)))
                       (incfn k w h)
                       k)])
        (hash-set! new new-pos v))))
  new)

(define (step mat w h) (move (move mat w h #\>) w h #\v))

(define (loop mat w h num-steps)
  (let ([next (step mat w h)])
    (if (equal? mat next)
      (values mat (add1 num-steps))
      (loop next w h (add1 num-steps)))))

(define (sol1 name)
  (define-values (m w h) (parse name))
  (define-values (n s) (loop m w h 0))
  (displayln s))

(sol1 "input25-1.txt")
(sol1 "input25-2.txt")
