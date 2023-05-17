(define (str->coords str)
  (let ([splitted (string-split str "..")])
    (list (string->number (substring (car splitted) 2))
          (string->number (cadr splitted)))))

(define (parse name)
  (map (lambda (line)
         (let* ([splitted (string-split line)]
                [on-off (car splitted)]
                [coords (map str->coords (string-split (cadr splitted) ","))])
           (list on-off coords)))
       (file->lines name)))

;; part 1
(define (axis-in-bounds? coords bounds)
  (and (>= (car coords) (car bounds)) (<= (cadr coords) (cadr bounds))))

(define (in-bounds? coords bounds)
  (and (axis-in-bounds? (car coords)   (car bounds))
       (axis-in-bounds? (cadr coords)  (cadr bounds))
       (axis-in-bounds? (caddr coords) (caddr bounds))))

(define (update-cubes cubes coords area update-fn!)
  (if (not (in-bounds? coords area))
    cubes
    (let* ([xcoords (car coords)]
           [ycoords (cadr coords)]
           [zcoords (caddr coords)])
      (for ([x (in-inclusive-range (car xcoords) (cadr xcoords))])
           (for ([y (in-inclusive-range (car ycoords) (cadr ycoords))])
                (for ([z (in-inclusive-range (car zcoords) (cadr zcoords))])
                     (update-fn! cubes (list x y z))))))))

(define area '((-50 50) (-50 50) (-50 50)))

(define (sol1 name)
  (define cubes (make-hash))
  (for ([line (in-list (parse name))])
       (if (string=? (car line) "on")
         (update-cubes cubes (cadr line) area (lambda (h k) (hash-set! h k #t)))
         (update-cubes cubes (cadr line) area hash-remove!)))
  (displayln (length (hash-values cubes)))
  cubes)

(sol1 "input22-1.txt")
; (sol1 "input22-2.txt")
; (sol1 "input22-3.txt")
  ; (define sorted (sort (hash-keys cubes)
  ;                           (lambda (x y)
  ;                             (or (< (car x) (car y))
  ;                                 (and (= (car x) (car y)) (< (cadr x) (cadr y)))
  ;                                 (and (= (car x) (car y)) (= (cadr x) (cadr y)) (< (caddr x) (caddr y)))))))
  ; (for ([c (in-list sorted)])
  ;      (printf "~a\n" c))

;; part 2
(define (band-top x) (car x))
(define (band-bottom x) (cadr x))
(define (band-walls x) (caddr x))
(define (make-band top bottom walls) (list top bottom walls))
(define (make-region front back bands)
  (list front back
        (append
          (cons (make-band (- (expt 2 64)) (band-top (car bands)) '()) bands)
          (list (make-band (band-bottom (last bands)) (expt 2 64) '())))))
(define (region-front r) (car r))
(define (region-back  r) (cadr r))
(define (region-bands r) (caddr r))
(define (make-3d-region regions)
  (append
    (cons (make-region (- (expt 2 64)) (region-front (car regions)) '()) regions)
    (list (make-region (region-back (last regions)) (expt 2 64) '()))))

; https://magcius.github.io/xplain/article/regions.html
(define (combine-bands band-a band-b band-op)
  (define (elem l) (if (null? l) (expt 2 64) (car l)))
  (define (next l) (if (null? l) '() (cdr l)))
  (define (loop a b inside-a inside-b out)
    (if (and (null? a) (null? b))
      (reverse out)
      (let* ([wall (min (elem a) (elem b))]
             [was-inside (band-op inside-a inside-b)]
             [new-a        (if (= wall (elem a)) (next a) a)]
             [new-inside-a (if (= wall (elem a)) (not inside-a) inside-a)]
             [new-b        (if (= wall (elem b)) (next b) b)]
             [new-inside-b (if (= wall (elem b)) (not inside-b) inside-b)]
             [is-inside (band-op new-inside-a new-inside-b)])
        (loop new-a new-b new-inside-a new-inside-b
              (if (not (eq? was-inside is-inside))
                (cons wall out)
                out)))))
  (loop (band-walls band-a) (band-walls band-b) #f #f '()))

(define (band-union a b) (or a b))
(define (band-subtract a b) (and a (not b)))

(define (combine-region reg-a reg-b band-op)
  (define (elem l) (if (null? l) 'bad-food (car l)))
  (define (next l) (if (null? l) '() (cdr l)))
  (define (loop a b out)
    (if (and (null? a) (null? b))
      (reverse out)
      (let* ([top (max (band-top (elem a)) (band-top (elem b)))]
             [bottom (min (band-bottom (elem a)) (band-bottom (elem b)))]
             [new-a (if (= bottom (band-bottom (elem a))) (next a) a)]
             [new-b (if (= bottom (band-bottom (elem b))) (next b) b)]
             [walls (combine-bands (elem a) (elem b) band-op)])
        (loop new-a new-b (cons (list top bottom walls) out)))))
  (loop reg-a reg-b '()))

(define A (make-3d-region (list (make-region 11 13 (list (make-band 11 13 '(11 13)))))))
; (define A (make-region 11 13 (list (make-band 11 13 '(11 13)))))
; (define B (make-region 10 12 (list (make-band 10 12 '(10 12)))))
; (combine-region A B band-union)
