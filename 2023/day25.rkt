(require graph)

(define (parse in)
  (foldl (lambda (line h)
           (let ([split (string-split line ": ")])
             (hash-set h (car split) (string-split (cadr split) " "))))
         (hash) (file->lines in)))

(define (make-graph h)
  (unweighted-graph/undirected
    (append* (hash-map h (lambda (from to) (map (curry list from) to))))))

(define (remove-wire! g x)
  (remove-edge! g (car x) (cadr x)))

(define (count-reachable g source)
  (let-values ([(_ parents) (bfs g source)])
    (add1 (length (filter identity (hash-values parents))))))

(define (solve in to-remove)
  (let ([g (make-graph (parse in))])
    (for-each (lambda (wire) (remove-wire! g wire)) to-remove)
    (* (count-reachable g (car (car to-remove)))
       (count-reachable g (cadr (car to-remove))))))

(define (print-input g)
  (define-edge-property g tooltip)
  (for-each (lambda (e)
              (tooltip-set! (car e) (cadr e)
                            (format "~a to ~a" (car e) (cadr e))))
            (get-edges g))
  (display (graphviz g #:edge-attributes `([tooltip ,tooltip]))))

; use this to print the graph, then use:
;    dot -Tsvg graph.txt -o graph.svg
; or:
;    dot -Tsvg graph.txt -o graph.svg -Kneato
; (print-input (make-graph (parse "input25-1.txt")))
; (print-input (make-graph (parse "input25-2.txt")))

(println (solve "input25-1.txt" '(("hfx" "pzl") ("bvb" "cmg") ("nvd" "jqt"))))
(println (solve "input25-2.txt" '(("tjz" "vph") ("zkt" "jhq") ("pgt" "lnr"))))
