(define (vec- . args) (apply map - args))

(define (area r)
  (apply * (map (lambda (x) (+ 1 (abs x))) (apply vec- r))))

(define (consecutive-pairs l)
  (map list (drop-right l 1) (drop l 1)))

(define (in-rectangle? p rec)
  (and (>= (car  p) (car  (car  rec)))
       (<= (car  p) (car  (cadr rec)))
       (>= (cadr p) (cadr (car  rec)))
       (<= (cadr p) (cadr (cadr rec)))))

(define (in-rectangles? p recs)
  (findf (lambda (r) (in-rectangle? p r)) recs))

(define (overlaps? a b)
  (not (or  (or (< (car  (cadr b)) (car  (car  a)))
                (> (car  (car  b)) (car  (cadr a))))
            (or (< (cadr (cadr b)) (cadr (car  a)))
                (> (cadr (car  b)) (cadr (cadr a)))))))

(define (clamp x a b) (max (min x b) a))

(define (build-rec conns x2 y other-y)
  (let ([x1 (max (hash-ref conns y)
                 (hash-ref conns other-y))]
        [y1 (min y other-y)]
        [y2 (max y other-y)])
    (list (list x1 y1) (list x2 y2)))) ;; my rectangles are AABBs

;; groups each point by x
;; sorts groups by y
;; sorts each element of a group by x
(define (make-groups ps)
  (let* ([groups (group-by car ps)]
         [groups (map (lambda (g) (sort g (lambda (x y) (< (cadr x) (cadr y))))) groups)]
         [groups (sort groups (lambda (x y) (< (car (car x)) (car (car y)))))])
    groups))

(define (make-rec-groups recs)
  (let* ([groups (group-by caar recs)]
         [groups (sort groups (lambda (a b) (< (caaar a) (caaar b))))])
    groups))

(define (create-pairs l)
  (let loop ([l l])
    (if (empty? l)
        '()
        (cons (take l 2) (loop (cdr (cdr l)))))))

;; split the polygon formed by points `ps` into a list of rectangles
;; these rectangles will be slightly overlapped, will fix them later
;; how it works:
;; 1) group points into sorted groups
;; 2) keep track of horizontal connections between points
;; 3) for each connection, keep track of the x when they were last used
;; 4) on each group, create a new rectangle formed by the current
;; connections, by the x of the current group and by the older x in 3)
(define (split-in-recs ps)
  (let loop ([groups (make-groups ps)]
             [connections (hash)]
             [rectangles '()])
    (if (empty? groups)
        rectangles
        (let* ([cur (car groups)]
               [cur-x (car (car cur))]
               [cur-ys (map cadr cur)]
               [conn-ys (sort (hash-keys connections) <)]
               ; note that # of connections is always even
               [new-recs (map (lambda (ys) (apply build-rec connections cur-x ys))
                              (create-pairs conn-ys))]
               [new-conns (foldl (lambda (y conns) (hash-set conns y cur-x))
                                 connections conn-ys)]
               ; if a connection is alive and hits a point, remove it
               ; otherwise, points not hit by a connection create new ones
               [new-conns (foldl (lambda (y conns)
                                   (if (hash-has-key? conns y)
                                       (hash-remove conns y)
                                       (hash-set conns y cur-x)))
                                 new-conns cur-ys)])
        (loop (cdr groups) new-conns (append rectangles (remove-duplicates new-recs)))))))

;;   *-*       **-*
;; *-* |     *-*| |
;; | | |     | || |
;; *-* | ==> *-*| | (can you see the little stars?)
;;   | |       *| | 
;; *-* |     *-*| | 
;; *-* |     *-*| |
;;   *-*       **-*
(define (resolve-overlaps-between r rs)
  (let* ([x (car (car r))]
         [y1 (cadr (car r))]
         [y2 (cadr (cadr r))]
         [rs (filter (lambda (r) (not (or (< (cadr (cadr r)) y1)
                                          (> (cadr (car  r)) y2)))) rs)])
    (if (empty? rs)
        (list r)
        (let* ([ys (append* (map (lambda (r) (map cadr r)) rs))]
               [ys (drop (drop-right ys 1) 1)]
               [pairs (filter (lambda (r) (>= (- (cadr r) (car r)) 0))
                              (map (lambda (r) (list (add1 (car r)) (sub1 (cadr r))))
                                   (create-pairs ys)))]
               [center-recs (map (lambda (pair)
                                   (map (lambda (y) (list x y)) pair))
                                 pairs)]
               [start-rec (if (< y1 (cadr (car (first rs))))
                              (list (list (list x y1)
                                          (list x (sub1 (cadr (car (first rs)))))))
                              '())]
               [end-rec   (if (> y2 (cadr (cadr (last rs))))
                              (list (list (list x (add1 (cadr (cadr (last rs)))))
                                          (list x y2)))
                              '())]
               [big-rec (list (list (add1 (car (car r))) (cadr (car r)))
                              (cadr r))])
          (append (list big-rec) start-rec center-recs end-rec)))))

(define (resolve-overlaps recs)
  (let ([groups (make-rec-groups recs)])
    (append*
      (foldl (lambda (g changed)
               (if (empty? changed)
                   (list g)
                   (let* ([last-changed (car changed)]
                          [recs (map (lambda (r) (resolve-overlaps-between r last-changed)) g)]
                          [big-recs (map car recs)]
                          [small-recs (append* (map cdr recs))])
                     (cons big-recs (cons small-recs changed)))))
             '() groups))))

(define (cut-rectangle cutter cuttee)
  (if (not (overlaps? cutter cuttee))
      #f
      (list (list (apply clamp (car  (car  cuttee)) (map car  cutter))
                  (apply clamp (cadr (car  cuttee)) (map cadr cutter)))
            (list (apply clamp (car  (cadr cuttee)) (map car  cutter))
                  (apply clamp (cadr (cadr cuttee)) (map cadr cutter))))))

;; test if `r`, built by two points, can fit into the polygon created by `rs`
;; (i.e. it fits without holes inside or at the edges
;; do it by cutting each rectangle in `rs` so they stay inside `r`'s area, then
;; summing the areas of these cutted rectangles. that sum must be <= `r`'s area.
(define (test-rec r rs)
  (<= (area r)
      (apply + (map area (filter identity
                                 (map (lambda (s) (cut-rectangle r s)) rs))))))

(define (make-proper-rec r)
  (let ([x1 (car  (car r))] [x2 (car  (cadr r))]
        [y1 (cadr (car r))] [y2 (cadr (cadr r))])
    (list (list (min x1 x2) (min y1 y2))
          (list (max x1 x2) (max y1 y2)))))

(define (solve name)
  (let* ([in (map (lambda (l) (map string->number (string-split l ",")))
                  (file->lines name))]
         [pairs (combinations in 2)])
    (list
      (apply max (map area (combinations in 2)))
      ;; 1) create rectangles that describe the bigger polygon
      ;; 2) fix overlaps between rectangles
      ;; 3) test each pair of points against the rectangles
      (let ([recs  (resolve-overlaps (split-in-recs in))])
        (apply max (map (lambda (r) (area r))
                        (filter (lambda (r) (test-rec (make-proper-rec r) recs))
                                pairs)))))))

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
