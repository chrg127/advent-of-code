(define rocks '(((0 0) (1 0) (2 0) (3 0))       ; -
                ((1 0) (0 1) (1 1) (2 1) (1 2)) ; +
                ((0 0) (1 0) (2 0) (2 1) (2 2)) ; mirrored L
                ((0 0) (0 1) (0 2) (0 3))       ; I
                ((0 0) (1 0) (0 1) (1 1))))     ; square

(define (make-grid w [h 0]) (list (hash) w h h)) ;; infinitely tall grid: hashtable
(define grid-height caddr)                       ;; of sets, also keeps w and h
(define (in-grid? grid pos)
  (set-member? (hash-ref (car grid) (cadr pos) (set)) (car pos)))
(define (valid-pos? grid pos)
  (and (>= (car pos) 0) (<  (car pos) (cadr grid)) (not (in-grid? grid pos))))
(define (add-pos pos grid)
  (let ([y (cadr pos)] [h (caddr grid)])
    (list (hash-update (car grid) y (lambda (s) (set-add s (car pos))) (set))
          (cadr grid) (if (> (+ 1 y) h) (+ 1 y) h) (cadddr grid))))

(define (array+ . args) (apply (curry map +) args))
(define (make-ps r p) (map (curry array+ p) r))
(define (add-rock rock pos grid) (foldl add-pos grid (make-ps rock pos)))
(define (adjust pos last rock grid)
  (if (andmap (curry valid-pos? grid) (make-ps rock pos)) pos last))
(define (can-rest? rock pos grid)
  (if (= (cadr pos) (- (fourth grid) 1)) #t (findf (curry in-grid? grid) (make-ps rock pos))))

;; keep a history tracking the positions of the indexes for rocks
;; and movements. for each pair of (rocks, movements), keep the number
;; of rocks and corresponding height.

(define (find-consecutive-elems e1 e2 e3 l [eq-fn equal?])
  (cond ((or (empty? l)
             (empty? (cdr l))
             (empty? (cddr l))) #f)
        ((and (eq-fn e1 (car l))
              (eq-fn e2 (cadr l))
              (eq-fn e3 (caddr l))) l)
        (else (find-consecutive-elems e1 e2 e3 (cdr l) eq-fn))))

(define (index-positions=? a b) (and (= (car a) (car b)) (= (cadr a) (cadr b))))
(define (make-hist) '())
(define (add-hist hist mi ri nr h)
  (cons (list mi ri nr h) hist))
(define (compare-last-two hist)
  (if (or (empty? hist) (empty? (cdr hist)) (empty? (cddr hist)))
    #f
    (find-consecutive-elems (car hist) (cadr hist) (caddr hist) (cdddr hist) index-positions=?)))

(define (fuck-you start end hist-len)
  (let* ([nr-start (third start)]
         [nr-end (third end)]
         [h-start (fourth start)]
         [h-end (fourth end)]
         [nr-diff (- nr-end nr-start)]
         [h-diff  (- h-end h-start)]
         [num-cycles (quotient (- 1000000000000 nr-start) nr-diff)]
         [num-cycles-inexact (exact->inexact (/ (- 1000000000000 nr-start) nr-diff))]
         [new-nr (+ nr-start (* nr-diff num-cycles))]
         [new-h  (+ h-start  (* h-diff  num-cycles-inexact))])
    (printf "hist len = ~a\n" hist-len)
    (printf "start of pattern: ~a, ~a\n" nr-start h-start)
    (printf "after first cycle: ~a, ~a\n" nr-end h-end)
    (printf "stats: ~a ~a\n" nr-diff h-diff)
    (printf "num cycles: ~a ~a\n" num-cycles num-cycles-inexact)
    (printf "after skipping: ~a, ~a\n" new-nr new-h)
    (values new-nr (make-grid 7 new-h))))

(define (tetris movements until-val)
  (define grid (make-grid 7))
  (define mi 0)
  (define hist (make-hist))

  (define (next-pos pos rock grid)
    (let* ([move (if (char=? (string-ref movements mi) #\<) '(-1 0) '(1 0))]
           [pos-hor (adjust (array+ pos move) pos rock grid)])
      (set! mi (modulo (add1 mi) (string-length movements)))
      (array+ pos-hor '(0 -1))))

  (define (hist-stuff mi ri num-rocks grid)
    (set! hist (add-hist hist mi ri num-rocks (grid-height grid)))
    (let ([r (compare-last-two hist)])
      (if r
        (let ([i-want-to-finish-this-problem (caddr hist)]
              [len (length hist)])
          (set! hist '())
          (fuck-you (caddr r) i-want-to-finish-this-problem len))
        (values num-rocks grid))))

  (do ([ri 0]) ((= ri until-val))
    (do ([rock (list-ref rocks (modulo ri (length rocks)))]
         [pos (list 2 (+ 3 (caddr grid)))])
      ((can-rest? rock pos grid)
       (set! grid (add-rock rock (array+ pos '(0 1)) grid)))
      (set! pos (next-pos pos rock grid)))
    (set! ri (add1 ri))
    (let-values ([(new-nr new-grid) (hist-stuff mi (modulo ri (length rocks)) ri grid)])
      (set! ri new-nr)
      (set! grid new-grid)))
  (println (grid-height grid)))

(define input1 ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>")
(define input2 (string-replace (file->string "input17-2.txt") "\n" ""))
; (tetris input1 2022)
; (tetris input2 2022)
(tetris input1 1000000000000)
(tetris input2 1000000000000)
