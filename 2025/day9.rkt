(define (vec- . args) (apply map - args))

(define (area a b)
  (apply * (map (lambda (x) (+ 1 (abs x))) (vec- a b))))

(define (parse name)
  (map (lambda (l) (map string->number (string-split l ",")))
       (file->lines name)))

(define (consecutive-pairs l)
  (map list (drop-right l 1) (drop l 1)))

(define (in-rectangle? p rec)
  (and (>= (car  p) (car  (car  rec)))
       (<= (car  p) (car  (cadr rec)))
       (>= (cadr p) (cadr (car  rec)))
       (<= (cadr p) (cadr (cadr rec)))))

(define (in-rectangles? p recs)
  ;; (printf "~a ~a\n" p recs)
  (findf (lambda (r) (in-rectangle? p r)) recs))

;; some visualizations
(define (draw-line! grid line)
  (for-each (lambda (p)
              (vector-set! (vector-ref grid (cadr p)) (car p) #\#))
            (if (apply = (map car line))
                (map (lambda (y) (list (car (car line)) y))
                     (apply inclusive-range (sort (map cadr line) <)))
                (map (lambda (x) (list x (cadr (car line))))
                     (apply inclusive-range (sort (map car  line) <))))))

(define (visualize-contour ps)
  (let* ([ps (map (lambda (p) (map (lambda (x) (floor (/ x 600))) p)) ps)]
         [width  (+ 1 (apply max (map car  ps)))]
         [height (+ 1 (apply max (map cadr ps)))]
         [grid (for/vector ([i (in-range height)])
                 (make-vector width #\.))])
    (begin
      (for-each (lambda (pair)
                  (draw-line! grid pair))
                (cons (list (last ps) (first ps))
                      (consecutive-pairs ps)))
      (for ([y (in-range height)])
        (for ([x (in-range width)])
          (printf "~a" (vector-ref (vector-ref grid y) x)))
        (printf "\n")))))

(define (visualize-recs recs)
  (let* ([ps (append* recs)]
         [width (apply max (map car ps))]
         [height (apply max (map cadr ps))])
    (for ([y (in-inclusive-range 0 height)])
      (for ([x (in-inclusive-range 0 width)])
        (printf "~a" (if (in-rectangles? (list x y) recs) #\X #\.)))
      (printf "\n"))))

;; group each point by x, making groups
;; sort each group by y
;;
;; begin "sweeping a line" through each group of points:
;; - the goal is to create rectangles that describe the big polygon
;; - for each point in a group, find if it connects to a point of
;;   the last group
;; - if such a connection has been found, try creating a rectangle
;;   by finding if there's a higher/lower connection
;; - put rectangle in a list

(define (build-rec conns x2 y other-y)
  (printf "build-rec ~a ~a ~a ~a\n" conns x2 y other-y)
  (let ([x1 (max (hash-ref conns y)
                 (hash-ref conns other-y))]
        [y1 (min y other-y)]
        [y2 (max y other-y)])
    (list (list x1 y1) (list x2 y2)))) ;; my rectangles are AABBs

(define (find-nearest-conn y conns)
  (let* ([ys (sort (hash-keys conns) <)]
         [ind (index-of ys y)])
    (if (even? ind)
        (list-ref ys (+ ind 1))
        (list-ref ys (- ind 1)))))

;; TODO:
;; 1. test for triangles should use area: area must be < than the total area of the polygon
;;    - this means we need to calculate rectangles so they never overlap, this might be difficult
;; 3. we need to process even points that aren't at the end of a connection (i.e. they're creating one)
;;    - these points create connections. figure out the index of the new connection, and
;;      use that to create a new connection
;;    - can we add and remove connections before creating rectangles?
(define (build-polygon-recs ps)
  (let* ([groups (group-by car ps)]
         ;; sort each element of a group by y
         [groups (map (lambda (g) (sort g (lambda (x y) (< (cadr x) (cadr y))))) groups)]
         ;; sort each group by x
         [groups (sort groups (lambda (x y) (< (car (car x)) (car (car y)))))])
    (let loop ([groups groups]
               [connections (hash)]
               [rectangles '()])
      (if (empty? groups)
          rectangles
          (let* ([cur (car groups)]
                 [cur-x (car (car cur))]
                 [cur-ys (map cadr cur)]
                 [candidates (filter (lambda (y) (hash-has-key? connections y)) cur-ys)]
                 [other-ys (map (lambda (y) (find-nearest-conn y connections)) candidates)]
                 [new-recs (map (lambda (y other-y) (build-rec connections cur-x y other-y))
                                candidates other-ys)]
                 ;; other-ys must update the connection to the current x
                 [new-conns (foldl (lambda (y conns) (hash-set conns y cur-x))
                                   connections other-ys)]
                 ;; the current ys either add or remove a connection
                 [new-conns (foldl (lambda (y conns)
                                     (if (hash-has-key? conns y)
                                         (hash-remove conns y)
                                         (hash-set conns y cur-x)))
                                   new-conns cur-ys)])
            (printf "group = ~a ~a\nconnections = ~a\ncandidates = ~a\nrecs = ~a\nnew-recs = ~a\n\n"
                    cur-x cur-ys connections candidates rectangles new-recs)
            (loop (cdr groups) new-conns (append rectangles (remove-duplicates new-recs))))))))

;; afterwards, for each pair, we check if the missing corners are in
;; this list of rectangles. if both of them are, then the rectangle is valid,
;; and we return its area. (if it isn't, we return #f)

(define (part2 ps)
  (let ([pairs (combinations ps 2)]
        [recs  (build-polygon-recs ps)])
    (visualize-recs recs)
    recs))
    ;; (apply max
    ;;   (map (lambda (pair) (apply area pair))
    ;;        (filter (lambda (pair)
    ;;                  (let ([c1 (list (car (car pair))  (cadr (cadr pair)))]
    ;;                        [c2 (list (car (cadr pair)) (cadr (car  pair)))])
    ;;                    ;; (printf "~a ~a ~a\n" pair c1 c2)
    ;;                    (and (in-rectangles? c1 recs) (in-rectangles? c2 recs))))
    ;;                pairs)))))

(define (solve name)
  (let* ([in (parse name)])
    ;; (list
    ;;   (apply max (map (lambda (p) (apply area p))
    ;;                   (combinations in 2)))
      (part2 in)))

;; OTHER INPUTS
;;
;; 1,0
;; 3,0
;; 3,6
;; 16,6
;; 16,0
;; 18,0
;; 18,9
;; 13,9
;; 13,7
;; 6,7
;; 6,9
;; 1,9
;;
;; ==> 30
;;
;;
;; 1,1
;; 8,1
;; 8,3
;; 3,3
;; 3,4
;; 8,4
;; 8,9
;; 18,9
;; 18,11
;; 5,11
;; 5,9
;; 4,9
;; 4,11
;; 1,11
;; 1,7
;; 6,7
;; 6,6
;; 1,6
;;
;; ==> 88
;;
;;
;; 1,5
;; 3,5
;; 3,8
;; 7,8
;; 7,5
;; 9,5
;; 9,10
;; 11,10
;; 11,3
;; 6,3
;; 6,7
;; 4,7
;; 4,1
;; 13,1
;; 13,12
;; 1,12
;;
;; ==> 72
;;
;;
;; 1,1
;; 10,1
;; 10,4
;; 5,4
;; 5,6
;; 10,6
;; 10,10
;; 1,10
;;
;; ==> i have no idea, but it's an interesting shape
