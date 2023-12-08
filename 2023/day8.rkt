(require srfi/1)

(define (build-path instrs paths [pos "AAA"])
  (stream-cons pos (build-path (cdr instrs) paths
                               ((car instrs) (hash-ref paths pos)))))

(define (build-path2 instrs paths [poss '("AAA")])
  (stream-cons poss (build-path2 (cdr instrs) paths
                                (map (lambda (x)
                                       ((car instrs) (hash-ref paths x)))
                                     poss))))

(define (find-zzz instrs paths [pos "AAA"])
  (for/or ([pos (build-path2 instrs paths (list pos))] [i (in-naturals)])
    (if (string=? (car pos) "ZZZ") i #f)))

(define (end=? c s) (char=? (string-ref s (- (string-length s) 1)) c))

(define (find-z-ends instrs paths)
  (let ([first-poss (filter (curry end=? #\A) (hash-keys paths))])
    (for/or ([poss (build-path2 instrs paths first-poss)]
             [i (in-naturals)])
      (if (andmap (curry end=? #\Z) poss) i #f))))

(define (print-until instrs paths n)
  (let ([first-poss (filter (curry end=? #\A) (hash-keys paths))])
    (for ([poss (build-path2 instrs paths first-poss)]
          [i (in-naturals)]
          #:break (end=? #\Z (car poss)))
      (printf "poss = ~a, i = ~a\n" poss i))))

(define (parse in)
  (let* ([split (string-split (file->string in) "\n\n")]
         [instrs (map (lambda (c) (if (char=? c #\L) car cadr))
                      (string->list (car split)))]
         [paths (map (lambda (line)
                       `(,(substring line 0 3)
                          (,(substring line 7 10) ,(substring line 12 15))))
                     (string-split (cadr split) "\n"))])
    (list (apply circular-list instrs) (apply hash (apply append paths)))))

(define (solve1 in) (apply find-zzz (parse in)))
(define (solve2 in) (apply find-z-ends (parse in)))

(println (solve1 "input8-1.txt"))
(println (solve1 "input8-3.txt"))
(println (solve1 "input8-2.txt"))
; (println (solve2 "input8-4.txt"))

; (let ([ins (parse "input8-2.txt")])
;   (print-until (car ins) (cadr ins) 1000))
; (println (solve2 "input8-2.txt"))
