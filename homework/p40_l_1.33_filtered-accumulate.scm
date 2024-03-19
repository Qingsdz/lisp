
;;判断一个数是否为素数 1.循环 2-sqrt(n) 2.x | n 若除尽 则非素数 若除不尽 则继续循环
(define (pow-ngreater? x n)
    (if (> x (/ n 2))
        #f
        #t))

(define (find-max-factor n)
    (define (iter-f x max-factor)
        (if (pow-ngreater? x n)
            (if (= 0 (remainder n x))
                (iter-f (1+ x) x)
                (iter-f (1+ x) max-factor))
            max-factor))
    (iter-f 1 1))


(define (prime? n)
    (= 1 (find-max-factor n)))

(define (filtered-accumulate filter combiner null-value term a next b)
    (cond
        ((> a b) null-value)
        ((filter a) (combiner (term a) (filtered-accumulate filter combiner null-value term (next a) next b)))
        (else (combiner null-value (filtered-accumulate filter combiner null-value term (next a) next b)))))

(define (gcd a b)
    (if (= b 0)
        a 
        (gcd b (remainder a b))))

(define (identity x) x)
(define (sum-prime a b)
    (filtered-accumulate prime? + 0.0 identity a 1+ b))
(define (pro-n-prime n)
    (define (filter x)
        (= (gcd x n) 1))
    (filtered-accumulate filter * 1.0 identity 1 1+ n))


