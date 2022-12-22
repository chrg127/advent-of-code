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
  (if (= (cadr pos) (- (fourth grid) 1)) #t
    (findf (curry in-grid? grid) (make-ps rock pos))))

(define (findf-consecutive n proc elems l)
  (cond ((empty? (list-tail l n)) #f)
        ((andmap proc elems (take l n)) l)
        (else (findf-consecutive n proc elems (cdr l)))))

(define (find-pattern hist [n 3])   ;; take first 3 elems and find them
  (if (< (length hist) (* n 2)) #f  ;; in the rest of the history
    (findf-consecutive n (lambda (a b) (andmap = (take a 2) (take b 2)))
                       (take hist n) (drop hist n))))

(define (do-skip start end target)
  (let* ([r-start (third start)]  [r-diff (- (third  end) r-start)]
         [h-start (fourth start)] [h-diff (- (fourth end) h-start)]
         [cycles (quotient (- target r-start) r-diff)]
         [new-r (+ r-start (* r-diff cycles))]
         [new-h (+ h-start (* h-diff cycles))])
    (values (cadr start) new-r (make-grid 7 new-h) '())))

(define (start-pos g) (list 2 (+ 3 (grid-height g))))
(define (rock-step rock grid moves mi [pos (start-pos grid)])
  (if (can-rest? rock pos grid)
    (values mi (add-rock rock (array+ pos '(0 1)) grid))
    (let* ([move (if (char=? (string-ref moves mi) #\<) '(-1 0) '(1 0))])
      (rock-step rock grid moves (modulo (+ mi 1) (string-length moves))
                 (array+ (adjust (array+ pos move) pos rock grid) '(0 -1))))))

(define (update-hist hist mi nr grid target) ;; history is defined by groups
  (let* ([ri (modulo nr 5)]                  ;; of (ri mi nr h)
         [new-hist (cons (list ri mi nr (grid-height grid)) hist)]
         [skip (find-pattern new-hist)])
    (if skip
      (do-skip (caddr skip) (caddr new-hist) target)
      (values mi nr grid new-hist))))

(define (tetris target moves [nr 0] [mi 0] [grid (make-grid 7)] [hist '()])
  (if (= nr target)
    (grid-height grid)
    (let*-values ([(new-mi new-grid)
                   (rock-step (list-ref rocks (modulo nr 5)) grid moves mi)]
                  [(new-mi2 new-nr new-grid2 new-hist)
                   (update-hist hist new-mi (+ nr 1) new-grid target)])
      (tetris target moves new-nr new-mi2 new-grid2 new-hist))))

(define part1 (curry tetris 2022))
(define part2 (curry tetris 1000000000000))

(define input1 ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>")
(define input2 (string-replace (file->string "input17-2.txt") "\n" ""))
(println (part1 input1))
(println (part1 input2))
(println (part2 input1))
(println (part2 input2))
