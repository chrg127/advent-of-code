(require math)

(define (parse name)
  (map string->number (string-split (string-trim (file->string name)) ",")))

(define (get-fuel crabs pos fn)
  (for/sum ([c crabs]) (fn (abs (- c pos)))))

(define (min-fuel crabs fuel-fn)
  (apply min (map (lambda (pos) (get-fuel crabs pos fuel-fn))
                  (inclusive-range (apply min crabs) (apply max crabs)))))

(define (sol1 name)
  (define crabs (parse name))
  (displayln (get-fuel crabs (median < crabs) identity)))

(define (sol2 name)
  (displayln (min-fuel (parse name) (lambda (n) (quotient (* n (add1 n)) 2)))))

(sol1 "input7-1.txt")
(sol1 "input7-2.txt")
(sol2 "input7-1.txt")
(sol2 "input7-2.txt")
