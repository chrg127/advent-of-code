(define (parse name)
  (map (lambda (line)
         (map string-split (string-split line "|")))
       (file->lines name)))

(define (input-values entry) (first entry))
(define (output-values entry) (second entry))

(define (is-1478? digit-str)
  (let ((n (string-length digit-str)))
    (or (= n 2) (= n 3) (= n 4) (= n 7))))

(define (count-1478 entries)
  (map (lambda (entry) (count is-1478? (output-values entry))) entries))

(define (part1 name)
  (displayln (apply + (count-1478 (parse name)))))

(part1 "input8-1.txt")
(part1 "input8-3.txt")
(part1 "input8-2.txt")

(define (list-contained? needles haystack)
  (andmap (lambda (x) (not (eq? (memq x haystack) #f))) needles))
(define (str-contained? needles haystack) (list-contained? (string->list needles) (string->list haystack)))

(define (get-patterns digit-list)
  (define (strings-of-len len) (filter (lambda (x) (= (string-length x) len)) digit-list))
  (define seven (car (strings-of-len 3)))
  (define one   (car (strings-of-len 2)))
  (define four  (car (strings-of-len 4)))
  (define eight (car (strings-of-len 7)))
  (define six   (car (filter-not (lambda (x) (str-contained? seven x)) (strings-of-len 6))))
  (define nine  (car (filter     (lambda (x) (str-contained? four x))  (strings-of-len 6))))
  (define five  (car (filter     (lambda (x) (str-contained? x six))   (strings-of-len 5))))
  (define two   (car (filter-not (lambda (x) (str-contained? x nine))  (strings-of-len 5))))
  (define zero  (car (remove six (remove nine (strings-of-len 6)))))
  (define three (car (remove five (remove two (strings-of-len 5)))))
  (list zero one two three four five six seven eight nine))

(define (list-eq? l1 l2) (if (not (= (length l1) (length l2))) #f (andmap eq? l1 l2)))
(define (num->char n) (integer->char (+ n (char->integer #\0))))
(define (list->number l) (string->number (list->string l)))

(define (find-output-digits input)
  (let ((patterns (get-patterns (input-values input))))
    (map (lambda (v) (num->char (index-of patterns v
                               (lambda (x y) (list-eq? (sort (string->list x) char<?)
                                                       (sort (string->list y) char<?))))))
         (output-values input))))

(define (part2 name)
  (displayln (apply + (map (lambda (x) (list->number (find-output-digits x))) (parse name)))))

(part2 "input8-1.txt")
(part2 "input8-3.txt")
(part2 "input8-2.txt")
