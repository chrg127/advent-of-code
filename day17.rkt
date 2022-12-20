(define (make-grid w [h 0]) (list (hash) w h h)) ;; infinitely tall grid: hashtable
(define (in-grid? grid pos)              ;; of sets, also keeps w and h
  (let ([the-grid (car grid)] [x (car pos)] [y (cadr pos)] [h (caddr grid)])
    (set-member? (hash-ref the-grid y (set)) x)))

(define (valid-pos? grid pos)
  (and (>= (car pos) 0)
       (<  (car pos) (cadr grid))
       (not (in-grid? grid pos))))

(define (add-pos pos grid)
  (let ([y (cadr pos)] [h (caddr grid)])
    (list (hash-update (car grid) y (lambda (s) (set-add s (car pos))) (set))
          (cadr grid) (if (> (+ 1 y) h) (+ 1 y) h) (cadddr grid))))

(define rocks '(((0 0) (1 0) (2 0) (3 0))       ; -
                ((1 0) (0 1) (1 1) (2 1) (1 2)) ; +
                ((0 0) (1 0) (2 0) (2 1) (2 2)) ; mirrored L
                ((0 0) (0 1) (0 2) (0 3))       ; I
                ((0 0) (1 0) (0 1) (1 1))))     ; square

(define (make-hist) (hash))
(define (in-hist? v h) (member v (hash-keys h)))
(define (entry-of v h) (hash-ref h v))
(define (add-hist hist v n h) (hash-set hist v (list n h)))

(define (array+ . args) (apply (curry map +) args))
(define (make-ps r p) (map (curry array+ p) r))
(define (add-rock rock pos grid) (foldl add-pos grid (make-ps rock pos)))
(define (adjust pos last rock grid)
  (if (andmap (curry valid-pos? grid) (make-ps rock pos)) pos last))
(define (can-rest? rock pos grid)
  (if (= (cadr pos) (- (fourth grid) 1)) #t (findf (curry in-grid? grid) (make-ps rock pos))))

(define (check-pattern num-rocks mi ri grid)
  (when (zero? ri)
    (printf "~a ~a (~a, ~a)\n" mi ri num-rocks (hash-count (car grid))))
  (when (and (zero? mi) (zero? ri))
    (printf (hash-count (car grid)))
    (exit)))

(define (grid-height g) (caddr g))

;; npattern = (1000000000000 - rockbeforepattern) / rocksinpattern
;; height-before-lefties = height-before-pattern + (height-pattern * npattern)


(define (tetris movements until-val)
  (define grid (make-grid 7))
  (define mi 0)
  (define hist (make-hist))

  (define (next-pos pos rock grid)
    (let* ([move (if (char=? (string-ref movements mi) #\<) '(-1 0) '(1 0))]
           [pos-hor (adjust (array+ pos move) pos rock grid)])
      (set! mi (modulo (add1 mi) (string-length movements)))
      (array+ pos-hor '(0 -1))))

  ;; rocks-leftovers = (1000 - num-rocks-before) % pattern-rocks
  ;; npatterns       = 
  ;; height-after    = height-before + pattern-height * ((1000 - num-rocks-before) // pattern-rocks)

  (define (hist-stuff mi ri num-rocks grid)
    (if (in-hist? (list mi ri) hist)
      (let* ([value (entry-of (list mi ri) hist)]
             [num-rocks-before (car value)]
             [height-before (cadr value)]
             [pattern-rocks (- num-rocks num-rocks-before)]
             [pattern-height (- (grid-height grid) height-before)]
             [rocks-leftovers (modulo (- 1000000000000 num-rocks-before) pattern-rocks)]
             [rocks-after (- 1000000000000 num-rocks-before)]
             [num-patterns (quotient 1000000000000 pattern-rocks)]
             [height-after (+ height-before (* pattern-height num-patterns))])
        (printf "detected pattern: ~a, ~a, ~a -> ~a, ~a ~a\n"
                (entry-of (list mi ri) hist) num-rocks-before height-before
                (list mi ri) num-rocks (grid-height grid))
        (printf "detected pattern: ~a ~a ~a ~a ~a ~a ~a ~a\n"
                num-rocks-before pattern-rocks
                height-before pattern-height
                rocks-after height-after
                rocks-leftovers num-patterns)
        (begin
          (set! hist (make-hist))
          (values (make-grid 7 0) 1000000000000)))
      (begin
        (set! hist (add-hist hist (list mi ri) num-rocks (grid-height grid)))
        (values grid num-rocks))))

  (do ([ri 0]) ((= ri until-val))
    (do ([rock (list-ref rocks (modulo ri (length rocks)))]
         [pos (list 2 (+ 3 (caddr grid)))])
      ((can-rest? rock pos grid)
       (set! grid (add-rock rock (array+ pos '(0 1)) grid)))
      (set! pos (next-pos pos rock grid)))
    (set! ri (add1 ri))
    (let-values ([(new-grid new-ri) (hist-stuff mi (modulo ri (length rocks)) ri grid)])
      (set! ri new-ri)
      (set! grid new-grid)))
    ; (printf "~a, ~a\n" ri (grid-height grid)))

  (println (grid-height grid))
  (println grid))

(define input1 ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>")
(define input2 (string-replace (file->string "input17-2.txt") "\n" ""))
; (tetris input1 2022)
; (tetris input2 2022)
(tetris input1 1000000000000)
(tetris input2 1000000000000)
