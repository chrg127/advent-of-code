; pulse structure: (type from to)
;   where: type = 'low | 'high
;          from, to = names
;
; module: name => (type value)
;   where: type = 'flip-flop 'conjuction 'broadcaster
;          value = #t or #f (if flip-flop)
;                | (listof pulse) (if conjunction)
;                | nothing (if broadcaster

(define mod-children cddr)
(define mod-type car)
(define pulse-type first)
(define pulse-from second)
(define pulse-to third)
(define mod-value second)

(define (init-conjunctions modules)
  (foldl (lambda (name r)
           (foldl (lambda (k r)
                    (update-modf r k (lambda (value) (hash-set value name 'low))))
                  r (filter (lambda (x)
                              (equal? (mod-type (hash-ref modules x)) 'conjuction))
                            (mod-children (hash-ref modules name)))))
         modules (hash-keys modules)))

(define (make-module name)
  (cond ([string-prefix? name "%"] (values (substring name 1) (list 'flip-flop #f)))
        ([string-prefix? name "&"] (values (substring name 1) (list 'conjuction (hash))))
        ([string-prefix? name "'"] (values (substring name 1) (list 'testing '())))
        ([string=? name "broadcaster"] (values name (list 'broadcaster '())))
        (else (error "unreachable"))))

(define (parse in)
  (init-conjunctions
    (apply hash (append*
                  (map (lambda (line)
                         (let ([split (string-split line " -> ")])
                           (let-values ([(name mod) (make-module (car split))])
                             (list name (append mod (string-split (cadr split) ", "))))))
                       (file->lines in))))))

(define (send-pulse name mod type)
  (map (lambda (x) (list type name x)) (mod-children mod)))

(define (update-mod modules name value)
  (hash-update modules name (lambda (mod) (list-set mod 1 value))))

(define (update-modf modules name fn)
  (hash-update modules name (lambda (mod) (list-update mod 1 fn))))

(define (all-high tab)
  ; (printf "all-high? tab = ~a\n" tab)
  (andmap (lambda (x) (equal? x 'high))
          (hash-values tab)))

(define (process-pulses modules2 pulses)
  (foldl (lambda (p r)
           (printf "~a ~a -> ~a\n" (pulse-from p) (pulse-type p) (pulse-to p))
           (let* ([modules (car r)] [pulses (cadr r)]
                  [name (pulse-to p)] [mod (hash-ref modules name)])
             (match (mod-type mod)
               ['broadcaster
                (list modules (append pulses (send-pulse name mod (pulse-type p))))]
               ['flip-flop
                (if (equal? (pulse-type p) 'high)
                  (list modules pulses)
                  (list (update-modf modules name (lambda (x) (xor x #t)))
                        (append pulses (send-pulse name mod (if (mod-value mod) 'low 'high)))))]
               ['conjuction
                (let* ([new-value (hash-set (mod-value mod) (pulse-from p) (pulse-type p))])
                  (list (update-mod modules name new-value)
                        (if (all-high new-value)
                          (send-pulse name mod 'low)
                          (send-pulse name mod 'high))))]
               ['testing (list modules pulses)])))
         (list modules2 '())
         pulses))

(define start-pulse '((low "button" "broadcaster")))

(define (pulse-loop modules pulses)
  (printf "pulses = ~a\n" pulses)
  ; (printf "modules = ~a\n" modules)
  (if (empty? pulses)
    modules
    (let ([res (process-pulses modules pulses)])
      (pulse-loop (car res) (cadr res)))))
