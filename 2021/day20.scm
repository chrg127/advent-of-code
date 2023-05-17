(define (to-matrix img)
  (define h (make-hash))
  (for ([y (in-range (length img))])
       (for ([x (in-range (string-length (car img)))])
            (hash-set! h (list y x) (string-ref (list-ref img y) x))))
  h)

            ; (if (char=? () #\#)
            ;   (hash-set! h (list y x) #\#)
            ;   (void))))

(define (parse name)
  (let* ([splitted (string-split (file->string name) "\n\n")]
         [pattern (car splitted)]
         [img (string-split (cadr splitted) "\n")])
    (values pattern (to-matrix img) (string-length (car img)) (length img))))

; (define (mat-ref m c) (hash-ref m c (lambda () #\.)))

(define (mat-print mat sx sy w h)
  (for ([y (in-range sx h)])
       (for ([x (in-range sy w)])
            (display (hash-ref mat (list y x) (lambda () #\.))))
       (newline)))

(define (get-coords x y) (cartesian-product (range (sub1 x) (+ 2 x)) (range (sub1 y) (+ 2 y))))

(define (to-bin m x y ref)
  (let* ([pixels (list->string (map (lambda (c) (ref m c)) (get-coords x y)))]
         [tmp1 (string-replace pixels "." "0")]
         [tmp2 (string-replace tmp1   "#" "1")]
         [num (string->number tmp2 2)])
    (string->number tmp2 2)))

; (define ((get-ref-fn pattern sx sy width height symbol) m c)
;   (if (or (< (first  c) sx) (>= (first  c) width)
;           (< (second c) sy) (>= (second c) height))
;     symbol
;     (mat-ref m c)))
  ; (define ref (get-ref-fn pattern sx sy w h (if (= odd-step? 1) #\. #\#)))
            ; (if (char=? (string-ref pattern (to-bin img x y ref)) #\#)
            ;   (hash-set! new-img (list x y) #\#)
            ;   (void))))

(define pad 3)

(define (enhance img pattern sx sy w h fill)
  (define new-img (make-hash))
  (define (mat-ref m c) (hash-ref m c (lambda () fill)))
  (for ([y (in-range sy h)])
       (for ([x (in-range sx w)])
            (hash-set! new-img (list y x) fill)))
  (for ([y (in-range (+ sy pad) (- h pad))])
       (for ([x (in-range (+ sx pad) (- w pad))])
            (hash-set! new-img (list y x) (string-ref pattern (to-bin img y x mat-ref)))))
  new-img)

(define (enhance-loop img pattern sx sy w h steps)
  (if (= steps 0)
    (values img sx sy w h)
    (let* ([new-sx (- sx pad)] [new-sy (- sy pad)]
           [new-w  (+  w pad)] [new-h  (+  h pad)]
           [symbol (if (and (char=? #\# (string-ref pattern 0)) (= (modulo steps 2) 0)) #\. #\#)]
           [new (enhance img pattern new-sx new-sy new-w new-h symbol)])
      (enhance-loop new pattern new-sx new-sy new-w new-h (sub1 steps)))))

(define name "input20-3.txt")
(define-values (p m w h) (parse name))
(define-values (next sx sy nw nh) (enhance-loop m p 0 0 w h 1))
(mat-print next sx sy nw nh)
(display (length (filter (lambda (x) (char=? x #\#)) (hash-values next))))
