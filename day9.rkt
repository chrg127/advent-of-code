(require math/array)

(define (parse input)
  (map (lambda (l) (list (string-ref l 0)
                         (string->number (substring l 2)))) input))

(define (make-step move)
  (cond ((char=? move #\R) (array #( 1  0)))
        ((char=? move #\L) (array #(-1  0)))
        ((char=? move #\U) (array #( 0 -1)))
        (else              (array #( 0  1)))))

(define (touching? head tail)
  (findf (curry equal? tail)
         (map (compose (lambda (p) (array+ head p)) list->array)
              (cartesian-product '(-1 0 1) '(-1 0 1)))))

(define positions (mutable-set))

(define (move-head head tail move steps)
  (if (= steps 0)
    (list head tail)
    (let* ([new-head (array+ head (make-step move))]
           [new-tail (if (touching? new-head tail) tail head)])
      (set-add! positions new-tail)
      (printf "~a ~a ~a\n" move steps new-tail)
      (move-head new-head new-tail move (sub1 steps)))))

(define (apply-instrs instrs)
  (foldl (lambda (x r) (apply move-head (append r x)))
         (list (array #(0 4)) (array #(0 4)))
         instrs))

(define (part1 input)
  (begin (apply-instrs input) (println positions)))

(define input1 (parse (file->lines "input9-1.txt")))
(define input2 (parse (file->lines "input9-2.txt")))
(part1 input1)
; (part1 input2)
