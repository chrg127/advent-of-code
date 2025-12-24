(require math)

(define (drop-ends l) (drop (drop-right l 1) 1))

(define (count-presses-diagram diagram buttons)
  (let loop ([machines '(0)] [depth 0])
    (if (member diagram machines)
        depth
        (loop (remove-duplicates
               (append*
                 (map (lambda (m)
                        (map (lambda (b) (bitwise-xor b m))
                             buttons))
                      machines)))
              (add1 depth)))))

(define (find-upper-bound col B)
  (apply min
         (map cadr
              (filter (lambda (x) (= (car x) 1))
                      (map list col B)))))

(define (matrix-solve-general M B)
  ;; using gaussian elimination, split variables in the system of equations defined by M
  ;; into free variables and dependent ones. then build equations for each dependent
  ;; variables.
  (let-values ([(G no-pivots) (matrix-gauss-elim (matrix-augment (list M B)) #t #t)])
           ;; last one is always the index of the B columnn: remove it
    (let* ([free-vars (drop-right no-pivots 1)]
           ;; build up equations for the dependent vars
           [coeffs (map (lambda (row)
                          (cons (last row)
                                (map (lambda (i) (- (list-ref row i)))
                                     free-vars)))
                        (matrix->list* G))]
           ;; given a list of values for the free vars, finds the values
           ;; for the dependent vars
           [function (lambda (args)
                       (map (lambda (l)
                              (apply + (map * l (cons 1 args))))
                            coeffs))]
           ;; each button has an upper bound: the minimum of the corresponding counters
           ;; the way i calculate this is clunky because i don't want to pass the original
           ;; buttons and joltages params, so this will have to do
           [upper-bounds (map (lambda (i)
                                (find-upper-bound (matrix->list (matrix-col M i))
                                                  (matrix->list B)))
                              free-vars)]
           ;; find the right values for the free vars by just trying all of them...
           [possible-values (apply cartesian-product
                                   (map (lambda (u) (inclusive-range 0 u))
                                        upper-bounds))]
           ;; ...and see which ones minimize the function above. values found may
           ;; be different from the values given in the sample input, but they should
           ;; always sum to the right result
           [vals (argmin (lambda (vals)
                           (let ([other-vals (function vals)])
                             (if (andmap (lambda (x) (and (integer? x) (>= x 0)))
                                         other-vals)
                                 (apply + (append other-vals vals))
                                 +inf.0)))
                         possible-values)])
      (append vals (function vals)))))

(define (as-matrix buttons)
  (let ([num-bits (add1 (apply max (flatten buttons)))])
    (list*->matrix
      (map (lambda (b)
             (foldl (lambda (n r) (list-set r n 1))
                    (make-list num-bits 0) b))
           buttons))))

(define (count-presses-joltages buttons joltages)
  (let* ([M (matrix-transpose (as-matrix buttons))]
         [B (->col-matrix joltages)])
    (apply +
      (if (and (square-matrix? M) (matrix-invertible? M))
          (matrix->list (matrix-solve M B))
          (matrix-solve-general M B)))))

(define (solve name)
  (apply map +
    (map (lambda (line)
           (let* ([splitted (string-split line " ")]
                  [diagram (string->number
                            (list->string
                              (reverse (map (lambda (c) (if (char=? c #\#) #\1 #\0))
                                            (drop-ends (string->list (car splitted))))))
                            2)]
                  [buttons (map (lambda (b) (read (open-input-string (string-replace b "," " "))))
                                (drop-ends splitted))]
                  [nums (map (lambda (b)
                               (foldl (lambda (n r)
                                        (bitwise-ior r (arithmetic-shift 1 n)))
                                      0 b))
                             buttons)]
                  [tmp (string-replace (string-replace (last splitted) "," " ") "{" "(")]
                  [joltages (read (open-input-string (string-replace tmp "}" ")")))])
             (list
               (count-presses-diagram diagram nums)
               (count-presses-joltages buttons joltages))))
         (file->lines name))))
