(define (to-bin hex)
  (define lookup (hash #\0 "0000" #\1 "0001" #\2 "0010" #\3 "0011" #\4 "0100" #\5 "0101" #\6 "0110" #\7 "0111"
                       #\8 "1000" #\9 "1001" #\A "1010" #\B "1011" #\C "1100" #\D "1101" #\E "1110" #\F "1111"))
  (apply string-append (map (lambda (c) (hash-ref lookup c)) (string->list hex))))

(define (to-int bin) (string->number bin 2))

(define (parse-packet bits collect)
  (define i 0)
  (define (readb n)
    (if (= n 0) ""
      (let ((bits (substring bits i (+ i n))))
        (set! i (+ i n))
        bits)))

  (define (literal version [bits ""])
    (let* ([group (readb 5)]
           [new-bits (string-append bits (substring group 1 5))])
      (if (char=? (string-ref group 0) #\0)
        (collect version 4 (to-int new-bits))
        (literal version new-bits))))

  (define (operator version type data pred step [child-results '()])
    (if (pred data i)
      (collect version type child-results)
      (operator version type (step data) pred step (cons (parse) child-results))))

  (define (parse)
    (let* ([version (to-int (readb 3))]
           [type (to-int (readb 3))])
      (cond ((= type 4) (literal version))
            ((char=? (string-ref (readb 1) 0) #\0)
              (operator version type (+ (to-int (readb 15)) i) = identity))
            (else
              (operator version type (to-int (readb 11)) (lambda (x g) (= x 0)) sub1)))))
  (parse))

(define (version-sum version type data)
  (cond ((= type 4) version)
        (else (+ version (apply + data)))))

(define (expr-solver version type data)
  (cond ((= type 0) (apply + data))
        ((= type 1) (apply * data))
        ((= type 2) (apply min data))
        ((= type 3) (apply max data))
        ((= type 4) data)
        ((= type 5) (if (> (cadr data) (car data)) 1 0)) ; children are collected from last to first
        ((= type 6) (if (< (cadr data) (car data)) 1 0))
        ((= type 7) (if (= (cadr data) (car data)) 1 0))
        (else (error "unknown type"))))

(define (printer version type data)
  (cond ((= type 4) (printf "literal: ~a version: ~a\n" data version))
        (else       (printf "operator;   version: ~a\n" version))))

(define (test str) (parse-packet (to-bin str) printer))
(define (sol1 str) (displayln (parse-packet (to-bin str) version-sum)))
(define (sol2 str) (displayln (parse-packet (to-bin str) expr-solver)))

;; tests
(test "D2FE28")
(test "38006F45291200")
(test "EE00D40C823060")
(sol1 "8A004A801A8002F478")
(sol1 "620080001611562C8802118E34")
(sol1 "C0015000016115A2E0802F182340")
(sol1 "A0016C880162017C3686B18A3D4780")
(sol2 "C200B40A82")
(sol2 "04005AC33890")
(sol2 "880086C3E88112")
(sol2 "CE00C43D881120")
(sol2 "D8005AC2A8F0")
(sol2 "F600BC2D8F")
(sol2 "9C005AC2F8F0")
(sol2 "9C0141080250320F1802104A08")

;; real
(define input (string-trim (file->string "input16.txt") "\n"))
(sol1 input)
(sol2 input)
