(define *rooms* '((B A) (C D) (B C) (D A)))
(define *hallway* '(() () () () ()))
(define *rh* (list *rooms* *hallway*))
(define *example-move* '(0 0 1 0))
(define *example-move2* '(1 1 0 1))

(define (mat-ref l i j) (list-ref (list-ref l i) j))
(define (mat-update l i j f) (list-update l i (lambda (x) (list-update x j f))))
(define (exec-move room-list move)
  (define-values (fish tmp)
    (values (car (mat-ref room-list (car move) (cadr move)))
            (mat-update room-list (car move) (cadr move) cdr)))
  (mat-update tmp (caddr move) (cadddr move) (lambda (x) (cons fish x))))

(define (all-filled? rooms)
  (and (equal? (car    rooms) '(A A))
       (equal? (cadr   rooms) '(B B))
       (equal? (caddr  rooms) '(C C))
       (equal? (cadddr rooms) '(D D))))

(define (fish->room f) (hash-ref (hash 'A 0 'B 1 'C 2 'D 3) f))
(define (hw-len i) (hash-ref (hash 0 2 1 1 2 1 3 1 4 2) i))

(define (filled? room) (= (length room) 2))

(define (blocked-hr? rooms ri hallway hi)
  #f)

(define (blocked-rh? rooms ri hallway hi)
  #f)

(define (blocked-rr? rooms hallway ri si)
  #f)

(define (hallway-move rooms hallway ri hi)
  (if (and (not (= (length (list-ref hallway hi)) (hw-len hi)))
           (not (blocked-rh? rooms ri hallway hi)))
    (list 0 ri 1 hi)
    #f))

(define (generate-moves-hr rooms hallway)
  (foldl (lambda (pos i r)
           (if (empty? pos)
             r
             (let* ([fish (car pos)] [ri (fish->room fish)])
               (if (and (not (filled? (list-ref rooms ri)))
                        (not (blocked-hr? rooms ri hallway i)))
                 (cons (list 1 i 0 ri) r)
                 r))))
         '() hallway '(0 1 2 3 4)))

(define (generate-moves-rh rooms hallway)
  (foldl (lambda (room i r)
           (if (empty? room)
             r
             (let* ([fish (car room)]
                    [moves (filter-map (lambda (hi) (hallway-move rooms hallway i hi))
                                       '(0 1 2 3 4))])
               (append moves r))))
         '() rooms '(0 1 2 3)))

(define (generate-moves-rr rooms hallway)
  (foldl (lambda (room i r)
           (if (empty? room)
             r
             (let* ([fish (car room)] [ri (fish->room fish)])
               (if (and (not (= i ri))
                        (not (filled? (list-ref rooms ri)))
                        (not (blocked-rr? rooms hallway i ri)))
                 (cons (list 0 i 0 ri) r)
                 r))))
         '() rooms '(0 1 2 3)))
