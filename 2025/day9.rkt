(define (vec- . args) (apply map - args))

(define (area a b)
  (apply * (map (lambda (x) (+ 1 (abs x))) (vec- a b))))

(define (solve name)
  (let* ([in (map (lambda (l) (map string->number (string-split l ",")))
                  (file->lines name))]
         [pairs (combinations in 2)]
         [areas (map (lambda (p) (apply area p)) pairs)])
    pairs))

;; sort each point by x
;; group by that same x
;; sort group by the y
;;
;; begin "sweeping a line" through each group of points:
;; - the goal is to create rectangles that describe the big polygon
;; - for each point in a group, find if it connects to a point of
;;   the last group
;; - if such a connection has been found, try creating a rectangle
;;   by finding if there's a higher/lower connection
;; - put rectangle in a list
;;
;; afterwards, for each pair, we check if the missing corners are in
;; this list of rectangles. if both of them are, then the rectangle is valid,
;; and we return its area. (if it isn't, we return #f)
