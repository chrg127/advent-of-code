(define (make-grid w) (list (hash) w 0)) ;; infinitely tall grid: hashtable
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
          (cadr grid) (if (> (+ 1 y) h) (+ 1 y) h))))

(define rocks '(((0 0) (1 0) (2 0) (3 0))       ; -
                ((1 0) (0 1) (1 1) (2 1) (1 2)) ; +
                ((0 0) (1 0) (2 0) (2 1) (2 2)) ; mirrored L
                ((0 0) (0 1) (0 2) (0 3))       ; I
                ((0 0) (1 0) (0 1) (1 1))))     ; square

(define (array+ . args) (apply (curry map +) args))
(define (make-ps r p) (map (curry array+ p) r))
(define (add-rock rock pos grid) (foldl add-pos grid (make-ps rock pos)))
(define (adjust pos last rock grid)
  (if (andmap (curry valid-pos? grid) (make-ps rock pos)) pos last))
(define (can-rest? rock pos grid)
  (if (= (cadr pos) -1) #t (findf (curry in-grid? grid) (make-ps rock pos))))

(define (check-pattern num-rocks mi ri grid)
  (when (zero? ri)
    (printf "~a ~a (~a, ~a)\n" mi ri num-rocks (hash-count (car grid))))
  (when (and (zero? mi) (zero? ri))
    (printf (hash-count (car grid)))
    (exit)))

(define (tetris movements until-val)
  (define grid (make-grid 7))
  (define mi 0)
  (define (next-pos pos rock grid)
    (let* ([move (if (char=? (string-ref movements mi) #\<) '(-1 0) '(1 0))]
           [pos-hor (adjust (array+ pos move) pos rock grid)])
      (set! mi (modulo (add1 mi) (string-length movements)))
      (array+ pos-hor '(0 -1))))
  (do ([ri 0]) ((= ri until-val))
    (do ([rock (list-ref rocks (modulo ri (length rocks)))]
         [pos (list 2 (+ 3 (caddr grid)))])
      ((can-rest? rock pos grid)
       (set! grid (add-rock rock (array+ pos '(0 1)) grid)))
      (set! pos (next-pos pos rock grid)))
    (set! ri (add1 ri))
    (check-pattern ri mi (modulo ri (length rocks)) grid))
  (println (hash-count (car grid))))

(define input1 ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>")
(define input2 (string-replace (file->string "input17-2.txt") "\n" ""))
; (tetris input1 2022)
; (tetris input2 2022)
; (tetris input1 10000)
(tetris input2 10000)
