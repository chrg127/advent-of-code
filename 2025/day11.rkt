(define (count-paths-1 devices dev)
  (let loop ([dev dev])
    (let ([connections (hash-ref devices dev)])
      (if (equal? connections '("out"))
          1
          (apply + (map loop connections))))))

(define (count-paths-2 devices dev)
  (define results (make-hash))
  (let loop ([dev dev] [has-dac #f] [has-fft #f])
    (if (hash-has-key? results (list dev has-dac has-fft))
        (hash-ref results (list dev has-dac has-fft))
        (let ([connections (hash-ref devices dev)]
              [new-has-dac (if (equal? dev "dac") #t has-dac)]
              [new-has-fft (if (equal? dev "fft") #t has-fft)])
          (if (equal? connections '("out"))
              (if (and new-has-dac new-has-fft) 1 0)
              (let ([res (apply + (map (lambda (c) (loop c new-has-dac new-has-fft))
                                       connections))])
                (hash-set! results (list dev has-dac has-fft) res)
                res))))))

(define (solve name)
  (let* ([in (foldl (lambda (l h)
                      (let ([splitted (string-split l ": ")])
                        (hash-set h (car splitted) (string-split (cadr splitted)))))
                    (hash) (file->lines name))])
    (list (if (hash-has-key? in "you") (count-paths-1 in "you") 0)
          (if (hash-has-key? in "svr") (count-paths-2 in "svr") 0))))
