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
  (findf (lambda (r) (in-rectangle? p r)) recs))

(define (rectangle-overlaps? a b)
  (not (and (or (< (car  (cadr b)) (car  (car  a)))
                (> (car  (car  b)) (car  (cadr a))))
            (or (< (cadr (cadr b)) (cadr (car  a)))
                (> (cadr (car  b)) (car  (cadr a)))))))

(define (clamp x a b) (max (min x b) a))

;; cut rectangle b with a
(define (cut-rectangle a b)
  (if (not (rectangle-overlaps? a b))
      #f
      (list (list (apply clamp (car  (car  b)) (map car  a))
                  (apply clamp (cadr (car  b)) (map cadr a)))
            (list (apply clamp (car  (cadr b)) (map car  a))
                  (apply clamp (cadr (cadr b)) (map cadr a))))))

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
(define (build-rec conns x2 y other-y)
  (let ([x1 (max (hash-ref conns y)
                 (hash-ref conns other-y))]
        [y1 (min y other-y)]
        [y2 (max y other-y)])
    (list (list x1 y1) (list x2 y2)))) ;; my rectangles are AABBs

(define (make-groups ps)
  (let* ([groups (group-by car ps)]
         ;; sort each element of a group by y
         [groups (map (lambda (g) (sort g (lambda (x y) (< (cadr x) (cadr y))))) groups)]
         ;; sort each group by x
         [groups (sort groups (lambda (x y) (< (car (car x)) (car (car y)))))])
    groups))

(define (make-rec-groups recs)
  (let* ([groups (group-by caar recs)]
         [groups (sort groups (lambda (a b) (< (caaar a) (caaar b))))])
    groups))

(define (create-pairs l)
  (if (not (even? (length l)))
      (error "create-pairs: list length is not even")
      (let loop ([l l])
        (if (empty? l)
            '()
            (cons (take l 2) (loop (cdr (cdr l))))))))

(define (build-polygon-recs ps)
  (let loop ([groups (make-groups ps)]
             [connections (hash)]
             [rectangles '()])
    (if (empty? groups)
        rectangles
        (let* ([cur (car groups)]
               [cur-x (car (car cur))]
               [cur-ys (map cadr cur)]
               [conn-ys (sort (hash-keys connections) <)]
               [new-recs (map (lambda (ys) (apply build-rec connections cur-x ys))
                              (create-pairs conn-ys))]
               [new-conns (foldl (lambda (y conns) (hash-set conns y cur-x))
                                 connections conn-ys)]
               [new-conns (foldl (lambda (y conns)
                                   (if (hash-has-key? conns y)
                                       (hash-remove conns y)
                                       (hash-set conns y cur-x)))
                                 new-conns cur-ys)])
          ;(printf "group = ~a ~a\nconnections = ~a\nnew-conns = ~a\nrecs = ~a\nnew-recs = ~a\n\n"
                  ;cur-x cur-ys connections new-conns rectangles new-recs)
        (loop (cdr groups) new-conns (append rectangles (remove-duplicates new-recs)))))))

(define (resolve-overlaps-between r rs)
  (printf "r = ~a rs = ~a\n" r rs)
  (let* ([x (car (car r))]
         [y1 (cadr (car r))]
         [y2 (cadr (cadr r))]
         [ys (sort (append* (map (lambda (r)
                                   (let ([ys (map cadr r)])
                                     (list (sub1 (car ys)) (add1 (cadr ys)))))
                                 rs)) <)]
         [ys (if (<= y1 (first ys)) (cons y1 ys) ys)]
         [ys (if (>= y2 (last ys)) (append ys (list y2)) ys)]
         [ys (filter (lambda (y) (and (>= y y1) (<= y y2))) ys)]
         [ys (begin (printf "ys = ~a\n" ys) ys)]
         [new-recs (map (lambda (pair)
                          (map (lambda (y) (list x y)) pair))
                        (create-pairs ys))])
    new-recs))

         ;[ys (sort (append* (map (lambda (r) (map cadr r)) rs)) <)]
         ;[ys2 (filter (lambda (y) (and (> y y1) (< y y2))) ys)]
         ;[ys2 (if (< y1 (first ys)) (cons y1 ys2) ys2)]
         ;[ys2 (if (> y2 (last  ys)) (append ys2 (list y2)) ys2)])
    ;(printf "y1 = ~a y2 = ~a ys = ~a ys2 = ~a\n" y1 y2 ys ys2)

(define (resolve-overlaps recs)
  (let ([groups (make-rec-groups recs)])
    (append*
      (foldl (lambda (g changed)
               (if (empty? changed)
                   (list g)
                   (let ([small-recs (append*
                                      (map (lambda (r) (resolve-overlaps-between r (car changed)))
                                           g))]
                         [big-recs (map (lambda (r) (list (list (add1 (car (car r)))
                                                                (cadr (car r)))
                                                          (cadr r)))
                                        g)])
                     (printf "small-recs = ~a\n" small-recs)
                     (printf "big-recs = ~a\n" big-recs)
                     (cons big-recs (cons small-recs changed)))))
             '() groups))))

(define (part2 ps)
  (let ([pairs (combinations ps 2)]
        [recs  (build-polygon-recs ps)])
    ;(visualize-recs recs)
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
;;
