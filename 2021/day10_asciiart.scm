(define (hash-re h x) (hash-ref h x)) (define (ident x) (identity x)) (define (c=? x y) (char=? x y))

            (require math)        (define (parse
        name) (  map                    string->list
     (file->lines                          name  ) )  )
    (define (                                  open? a b
   x ) (or (                                    c=? x #\(
   )(c=? x                                       #\[)(c=?
   x #\{)(                                       c=? x #\<
   )) )  (                                       define q
   (hash #\)                                   3  #\] 57
    #\}  1197                                 #\> 25137
     ))(define                              p2(hash 'a
      1 'curve 1                          'square  2
       'curly 3 'ang                   4) ) (define
            typ ( hash #\(        'curve 1 1 #\)

   'curve #\[ 'square #\]         'square #\{ 'curly #\}
   'curly #\< 'ang    #\>         'ang  )) (define (synt
   parens    stk   suc-fn         err-fn )  ( if ( null?
   parens)                                       (suc-fn
   stk   )                                       (let ((
   p ( car                                       parens)
   ))(cond                                       ((open?
   'helper                                       'column
   p)(synt                                       (   cdr
   parens)                                       ( cons(
   hash-re                                       typ   p
   )   stk                                       )suc-fn
   err-fn)                                       ) ((not
   (eq?  (                                       hash-re
   typ p)(                                       car stk
   ) )   )                                       (err-fn
   p ) ) (                                       else  (
   synt (cdr parens) (cdr         stk) suc-fn err-fn))))
   )) (define (sol1 name )        (displayln (apply +  (
   map (lambda (x ) ( synt        x '() (lambda (s) 0) (

                 lambda (p        )(hash-re
               q p ) ) )            ) ( parse
             name)))))                (define (
           sol2 nam                     ) (define
         (partopnt                        lst)(foldl
       (lambda(p                            r) (+(* r
     5)(hash-re                               p2 p)) )
   0 lst)) (                                    displayln
     (  median                                < (sort (
       filter  (                            lambda(x)
         (not(= x 0                       )))(map (
           lambda(x)                    (partopnt
             (  synt x                '() ident
               (lambda(p            )'()))))(
                 parse nam        ))) <))))

(sol1 "input10-1.txt") (sol1 "input10-2.txt") (sol2 "input10-1.txt") (sol2 "input10-2.txt")
