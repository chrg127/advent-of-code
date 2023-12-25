(define (parse in)
  (map (lambda (line)
         (let* ([nums (map string->number (regexp-match* #rx"-?[0-9]+" line))])
           (list (take nums 3) (drop nums 3))))
       (file->lines in)))

(define (intersect eq1 eq2)
  (let* ([a1 (car   eq1)] [a2 (car   eq2)]
         [b1 (cadr  eq1)] [b2 (cadr  eq2)]
         [c1 (caddr eq1)] [c2 (caddr eq2)]
         [denomx (- (* a1 b2) (* a2 b1))]
         [denomy (- (* a1 b2) (* a2 b1))])
    (if (or (= denomx 0) (= denomy 0))
      #f
      (list (/ (- (* b1 c2) (* b2 c1)) denomx)
            (/ (- (* a2 c1) (* a1 c2)) denomy)))))

(define (find-point h1 h2)
  (define (get-t p v x) (/ (- x p) v))
  (let* ([eq1 (caddr h1)] [eq2 (caddr h2)]
         [p1 (car h1)]  [p2 (car h2)]
         [v1 (cadr h1)]  [v2 (cadr h2)]
         [point (intersect eq1 eq2)])
    (cond ([not point] #f)
          ([< (get-t (car  p1) (car  v1) (car point)) 0] #f)
          ([< (get-t (cadr p1) (cadr v1) (cadr point)) 0] #f)
          ([< (get-t (car  p2) (car  v2) (car point)) 0] #f)
          ([< (get-t (cadr p2) (cadr v2) (cadr point)) 0] #f)
          (else point))))

; x = x0 + vx0 * t
; y = y0 + vy0 * t
;
; x - x0 = vx0 * t
; t = (x - x0) / vx0
;
; y = y0 + vy0 * (x - x0) / vx0
; y - y0 = vy0 * (x - x0) / vx0
; (y - y0)*vx0 = vy0*x - vy0*x0
; vx0*y - vx0*y0 = vy0*x - vy0*x0
; vy0*x - vx0*y + vx0*y0 - vy0*x0 = 0
(define (make-eq p v)
  (list (cadr v)
        (- (car v))
        (+ (* (car v) (cadr p)) (- (* (cadr v) (car p))))))

(define (solve in low hi)
  (let* ([eqs (map (lambda (x) (list (car x) (cadr x) (apply make-eq x))) (parse in))]
         [eq-pairs (combinations eqs 2)]
         [points (map (lambda (x) (apply find-point x)) eq-pairs)])
    (length (filter (lambda (p)
              (and p (andmap (lambda (a b c) (and (>= a b) (<= a c)))
                             p (list low low) (list hi hi))))
            points))))

(println (solve "input24-1.txt" 7 27))
(println (solve "input24-2.txt" 200000000000000 400000000000000))
