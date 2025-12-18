(define (drop-ends l) (drop (drop-right l 1) 1))

(define (count-presses diagram buttons)
  (let loop ([machines '(0)] [depth 0])
    (if (member diagram machines)
        depth
        (loop (remove-duplicates
               (append*
                 (map (lambda (m)
                        (map (lambda (b) (bitwise-xor b m))
                             buttons))
                      machines)))
              (add1 depth)))))

(define (solve name)
  (apply +
    (map (lambda (line)
           (let* ([splitted (string-split line " ")]
                  [diagram (string->number
                            (list->string
                              (reverse (map (lambda (c) (if (char=? c #\#) #\1 #\0))
                                            (drop-ends (string->list (car splitted))))))
                            2)]
                  [buttons (map (lambda (b) (read (open-input-string (string-replace b "," " "))))
                                (drop-ends splitted))]
                  [buttons-bins (map (lambda (b)
                                       (foldl (lambda (n r)
                                                (bitwise-ior r (arithmetic-shift 1 n)))
                                              0 b))
                                     buttons)]
                  [joltages (last splitted)])
             (count-presses diagram buttons-bins)))
         (file->lines name))))
