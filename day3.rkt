(define (priority-of item)
  (if (char-upper-case? item)
    (- (char->integer item) 38)
    (- (char->integer item) 96)))

(define (find-item rucksack)
  (let* ((half (/ (string-length rucksack) 2))
         (comp1 (list->set (string->list (substring rucksack 0 half))))
         (comp2 (string->list (substring rucksack half))))
    (car (filter (lambda (item) (set-member? comp1 item)) comp2))))

(define (part1 input)
  (println (apply + (map (lambda (r) (priority-of (find-item r))) input))))

(define input1 (file->lines "input3-1.txt"))
(define input2 (file->lines "input3-2.txt"))
(part1 input1)
(part1 input2)
