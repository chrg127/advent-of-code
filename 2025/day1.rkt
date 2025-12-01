(define (instr+ instr dial zeroes1 zeroes2)
  (let* ([res (+ dial instr)]
         [adj (modulo res 100)])
    (list adj
         (+ zeroes1 (if (= adj 0) 1 0))
         (+ zeroes2 (let ([c (abs (floor (/ res 100)))])
                      (if (>= instr 0) ; apply some adjustments
                          c
                          (+ c
                             (if (= dial 0) -1 0)
                             (if (= adj  0)  1 0))))))))

(define (solve in)
  (define repr string-replace)
  (let ([instrs (map (lambda (s) (string->number (repr (repr s "R" "+") "L" "-")))
                     (file->lines in))])
    (foldl (lambda (i r) (apply instr+ i r))
           '(50 0 0) instrs)))

(println (solve "input1-1.txt"))
(println (solve "input1-2.txt"))
