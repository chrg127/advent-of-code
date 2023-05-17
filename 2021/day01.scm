(define (count-increases lst step)
  (count > (drop lst step) (drop-right lst step)))

(define (parse name)
  (map
    (lambda (line) (string->number line))
    (port->lines (open-input-file name))))

(define (sol1 input) (count-increases input 1))
(define (sol2 input) (count-increases input 3))

(displayln (sol1 (parse "input1-1.txt")))
(displayln (sol1 (parse "input1-2.txt")))
(displayln (sol2 (parse "input1-1.txt")))
(displayln (sol2 (parse "input1-2.txt")))
