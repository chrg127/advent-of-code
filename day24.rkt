(define (array+ . args) (apply (curry map +) args))
(define (modulo-walls x a b) (+ a (modulo (- x a) (- b a))))

(define ((move-blizzard w h) b)
  (list-set b 0 (map modulo-walls (apply array+ b) '(1 1) (list w h))))

(define ((valid? board w h start end) pos)
  (or (equal? pos start) (equal? pos end)
      (and (> (car pos)  0) (< (car pos)  w)
           (> (cadr pos) 0) (< (cadr pos) h)
           (not (set-member? board pos)))))

(define neighbors '((0 0) (-1 0) (1 0) (0 -1) (0 1)))
(define ((advance board w h start end) pos)
  (list->set (filter (valid? board w h start end)
                     (map (curry array+ pos) neighbors))))

(define (do-sim start end board w h)
  (define (step positions board [count 0])
    (if (set-member? positions end)
      (list count board)
      (let* ([new-board (map (move-blizzard w h) board)]
             [bs (list->set (map car new-board))]
             [new-pos (apply set-union
                             (set-map positions
                                      (advance bs w h start end)))])
        (step new-pos new-board (+ count 1)))))
  (step (set start) board))

(define (char->blizzard c pos)
  (hash-ref (hash #\< (list pos '(-1  0)) #\> (list pos '( 1  0))
                  #\^ (list pos '( 0 -1)) #\v (list pos '( 0  1))) c #f))

(define (parse-blizzards grid w h)
  (define l '())
  (for ([y (in-range h)])
    (for ([x (in-range w)])
      (let* ([v (char->blizzard (string-ref (list-ref grid y) x) (list x y))])
        (when v (set! l (cons v l))))))
  l)

(define (parse input)
  (let* ([h (length input)] [w (string-length (car input))])
    (list (- w 1) (- h 1) (parse-blizzards input w h))))

(define (part1 board)
  (let* ([w (car board)] [h (cadr board)])
    (println (car (do-sim '(0 1) (list (- w 1) h) (caddr board) w h)))))

(define (part2 board)
  (let* ([w (car board)] [h (cadr board)]
         [start '(0 1)] [end (list (- w 1) h)]
         [trip1 (do-sim start end (caddr board) w h)]
         [trip2 (do-sim end start (cadr trip1) w h)]
         [trip3 (do-sim start end (cadr trip2) w h)])
    (println (+ (car trip1) (car trip2) (car trip3)))))

(define input1 (parse (file->lines "input24-1.txt")))
(define input2 (parse (file->lines "input24-2.txt")))
(define input3 (parse (file->lines "input24-3.txt")))
(part1 input2)
(part1 input3)
(part2 input2)
(part2 input3)
