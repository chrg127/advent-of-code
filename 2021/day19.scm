(require math)

(define (parse name)
  (map (lambda (s)
         (map (lambda (x) (->col-matrix (map string->number (string-split x ","))))
              (cdr (string-split s "\n"))))
       (string-split (file->string name) "\n\n")))

(define (get-diff s t minimum)
  (define h (make-hash))
  (for ([v s])
       (for ([u t])
            (hash-update! h (matrix- v u) add1 (lambda () 0))))
  (define res (findf (lambda (p) (>= (cdr p) minimum))
                     (hash->list h)))
  (if res (car res) #f))

(define (test-overlap s t minimum)
  (let* ([diff (get-diff s t minimum)])
    (if (not diff)
      #f
      (let* ([new-t (map (lambda (p) (matrix+ p diff)) t)]
             [points (filter (lambda (p) (if (member p s) p #f)) new-t)])
        (list diff points new-t)))))

(define dir-mats (list (matrix [[ 1  0  0][ 0  1  0][ 0  0  1]])
                       (matrix [[ 1  0  0][ 0  0 -1][ 0  1  0]])
                       (matrix [[ 1  0  0][ 0  0  1][ 0 -1  0]])
                       (matrix [[ 0  0 -1][ 0  1  0][ 1  0  0]])
                       (matrix [[-1  0  0][ 0  1  0][ 0  0 -1]])
                       (matrix [[ 0  0  1][ 0  1  0][-1  0  0]])))

(define rot-mats (list (matrix [[ 1  0  0][ 0  1  0][ 0  0  1]])
                       (matrix [[ 0  1  0][-1  0  0][ 0  0  1]])
                       (matrix [[-1  0  0][ 0 -1  0][ 0  0  1]])
                       (matrix [[ 0 -1  0][ 1  0  0][ 0  0  1]])))

(define mats (map (lambda (x) (matrix* (car x) (cadr x)))
                  (cartesian-product dir-mats rot-mats)))

(define (scanner-rot scanner mat)
  (map (lambda (p) (matrix* mat p)) scanner))

(define (find-overlapping-with-mats s t)
  (ormap (lambda (m)
           (let ([res (test-overlap s (scanner-rot t m) 12)])
             (if res (append res (list t)) #f)))
         mats))

(define (scanner-loop scanners)
  (define beacons '())
  (define diffs '())
  (define (loop s)
    (if (eq? scanners '())
      (printf "done\n")
      (let ([res (filter-map (lambda (t) (find-overlapping-with-mats s t)) scanners)])
        (for-each (lambda (r)
                    (printf "diff: ~a\n" (car r))
                    (set! diffs (cons (car r) diffs))
                    (set! beacons (append beacons (third r)))
                    (set! scanners (remove (fourth r) scanners))
                    (loop (third r)))
                  res))))
  (define start (car scanners))
  (set! scanners (remove start scanners))
  (loop start)
  (list (remove-duplicates (list-union start beacons)) diffs))

(define (manhattan-dist a b)
  (let ((x (matrix- a b)))
    (+ (abs (matrix-ref x 0 0)) (abs (matrix-ref x 1 0)) (abs (matrix-ref x 2 0)))))

(define (sol name)
  (define res (scanner-loop (parse name)))
  (define perms (filter (lambda (x) (not (equal? (car x) (cadr x)))) (cartesian-product (cadr res) (cadr res))))
  (displayln (length (car res)))
  (displayln (apply max (map (lambda (x) (manhattan-dist (car x) (cadr x))) perms))))

(sol "input19-1.txt")
(sol "input19-2.txt")
