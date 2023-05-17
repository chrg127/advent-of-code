(define (div x y correct) (correct (/ x y)))
(define (middle x y correct) (div (+ x y) 2 correct))
(define (print x) (begin (display x) (newline)))
(define (print-range ch x y) (begin (display x) (display "-") (display y) (display " ") (display ch) (newline)))

(define (read-line-by-line file)
  (let ((line (read-line file)))
    (if (eof-object? line)
      (begin
        (close-input-port file)
        '())
      (cons line (read-line-by-line file)))))

(define (getid seat-str)
  ;(print seat-str)
  (let ((len (string-length seat-str)))
    (define (compute-id row column) (+ (* row 8) column))
    (define (get-row start end index)
      (begin ;(print-range (string-ref seat-str index) start end)
      (cond ((= index (+ len 1)) (error "GET-ROW: string too short?"))
            ((eqv? #\F (string-ref seat-str index)) (get-row start (middle start end floor) (+ index 1)))
            ((eqv? #\B (string-ref seat-str index)) (get-row (middle start end ceiling) end (+ index 1)))
            ((eqv? #\L (string-ref seat-str index)) (get-column 0 3 start (+ index 1)))
            ((eqv? #\R (string-ref seat-str index)) (get-column 4 7 start (+ index 1)))
            (else (error "GET-ROW: unrecognized char")))))
    (define (get-column start end row index)
      (begin ;(print-range (string-ref seat-str (- index 1)) start end)
      (cond ((= index len) (compute-id row start))
            ((eqv? #\L (string-ref seat-str index)) (get-column start (middle start end floor) row (+ index 1)))
            ((eqv? #\R (string-ref seat-str index)) (get-column (middle start end ceiling) end row (+ index 1)))
            (else (error "GET-ROW: unrecognized char")))))
    (let ((id (get-row 0 127 0)))
      (begin ;(print id)
             id))))

(define (generate minn maxn)
  (if (= minn (+ maxn 1)) '() (cons minn (generate (+ minn 1) maxn))))

(define (find-missing maxn ls)
  (let ((nums (generate 0 maxn)))
    (filter (lambda (x)
              (eqv? (memq x ls) #f)) nums)))

; part 1
(for-each print (reduce max 0 (map getid (call-with-input-file "input5.txt" read-line-by-line))))

; part 2
(for-each print (find-missing 842 (map getid (call-with-input-file "input5.txt" read-line-by-line))))
