(define (shrink l) (drop (drop-right l 1) 1))

(define (push-button button machine)
  (foldl (lambda (pos machine)
           (list-set machine pos (if (list-ref machine pos) #f #t)))
         machine button))

(define (find-pressed machine diagram buttons)
  (define memo (make-hash))
  (define (loop machine button)
    (cond [(hash-has-key? memo (list machine button))
           (hash-ref memo (list machine button))]
          [(equal? machine diagram) 1]
          [else
           (begin
             (hash-set! memo (list machine button) #f)
             (let ([results (filter identity
                                 (map (lambda (b)
                                        (loop (push-button b machine) b))
                                      buttons))])
              (if (empty? results)
                  #f
                  (let ([res (apply min results)])
                    (begin
                      (hash-set! memo (list machine button) (+ 1 res))
                      (+ 1 res))))))]))
  (sub1 (loop machine '())))

(define (solve name)
  (apply +
    (map (lambda (line)
           (let* ([splitted (string-split line " ")]
                  [diagram (map (lambda (c) (if (char=? c #\#) #t #f))
                                (shrink (string->list (car splitted))))]
                  [buttons (map (lambda (b) (read (open-input-string (string-replace b "," " "))))
                                (shrink splitted))]
                  [joltages (last splitted)])
             (find-pressed (make-list (length diagram) #f) diagram buttons)))
         (file->lines name))))
