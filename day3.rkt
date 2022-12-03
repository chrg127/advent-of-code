(define (priority-of item)
  (if (char-upper-case? item)
    (- (char->integer item) 38)
    (- (char->integer item) 96)))

(define 

(define (find-item rucksack)
  (let* ((half (/ (string-length rucksack) 2))
         (comp1 (list->set (string->list (substring rucksack 0 half))))
         (comp2 (string->list (substring rucksack half))))
    (car (filter (lambda (item) (set-member? comp1 item)) comp2))))

(define (part1 input)
  (println (apply + (map (lambda (r) (priority-of (find-item r))) input))))

(define (split-by lst n)
  (if (not (empty? lst))
    (cons (take lst n) (split-by (drop lst n) n))
    '()))

(define (find-badge groups)
  )

(define (part2 input)
  (println (split-by input 3)))

(define input1 (file->lines "input3-1.txt"))
(define input2 (file->lines "input3-2.txt"))
; (part1 input1)
; (part1 input2)
(part2 input1)
