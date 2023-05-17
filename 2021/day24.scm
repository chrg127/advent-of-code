;; emulator
(define ((instr f) a b b-value regs inp-value)
  (list-update regs a (lambda (x) (f x (b-value regs b)))))

(define (inp a b f regs inp-value)
  (list-set regs a inp-value))

(define (int= x y) (if (= x y) 1 0))
(define (imm r b) b)

;; input parser
(define (string->instr s)
  (hash-ref (hash "add" (instr +)      "mul" (instr *)    "div" (instr quotient)
                  "mod" (instr modulo) "eql" (instr int=) "inp" inp)            s))

(define (string->reg s) (hash-ref (hash "x" 1 "y" 2 "z" 3 "w" 0) s))
(define (string->reg-or-imm s)
  (let ([num (string->number s)])
    (if num
      (list num imm)
      (list (string->reg s) list-ref))))

(define (parse name)
  (map (lambda (line)
         (let ([lst (string-split line)])
           (list (string->instr (car  lst))
                 (string->reg   (cadr lst))
                 (string->reg-or-imm (if (= (length lst) 3) (caddr lst) "0")))))
       (file->lines name)))

;; simple runner, inp instruction will always return the same value
(define (run program init inp-value)
  (foldl (lambda (instr regs)
           (let* ([f (car instr)] [a (cadr instr)]
                  [b (car (caddr instr))] [bv (cadr (caddr instr))]
                  [new-regs (f a b bv regs inp-value)])
             new-regs))
         init program))

; splits a program into a series of mini programs with always only one inp instruction
(define (split-program p)
  (define (loop program out proglist)
    (cond ((null? program) (reverse (cons (reverse out) proglist)))
          ((and (not (null? (cdr program))) (equal? (car (second program)) inp))
           (loop (cdr program) '() (cons (reverse (cons (car program) out)) proglist)))
          (else (loop (cdr program) (cons (car program) out) proglist))))
  (loop p '() '()))

; runs a program multiple times, with inp-value between 1 and 10
; it can be called with different functions for getting min or max
(define ((run-multiple-fn cmp1 cmp2 defval) program init)
  (define tab (make-hash))
  (for-each (lambda (v)
              (let ([state (run program init v)])
                (hash-update! tab state (lambda (x) (cmp1 x v)) (lambda () defval))))
            (range 1 10))
  (sort (hash-map tab list) (lambda (x y) (cmp2 (cadr x) (cadr y)))))

(define (sol name max-or-min)
  (define *memo* (make-hash))
  (define run-multiple (if (eq? max-or-min 'max)
                         (run-multiple-fn max > 0)
                         (run-multiple-fn min < (expt 2 64))))
  ; brute force for each digit, use memoization to cache entire branches of the call tree
  (define (step-run proglist init last-digit)
    (if (null? proglist)
      (if (= (list-ref init 3) 0)
        (list last-digit)
        #f)
      (let ([program (car proglist)])
        (if (hash-ref *memo* (list program init) (lambda () #f))
          #f
          (let* ([states (run-multiple program init)]
                 [digit (ormap (lambda (state)
                                (step-run (cdr proglist) (car state) (cadr state)))
                              states)])
            (if digit
              (cons last-digit digit)
              (begin (hash-set! *memo* (list program init) #t)
                     #f)))))))
  (displayln (step-run (split-program (parse name)) '(0 0 0 0) 0)))

(sol "input24-2.txt" 'max)
(sol "input24-2.txt" 'min)
