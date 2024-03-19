(load "./common/math.scm")

(define (gcd a b)
    (if (= b 0)
        a 
        (gcd b (remainder a b))))

;;;寻找因子
(define (find-divisor-iter n test-divisor)
    (cond 
        ((> (square test-divisor) n) n)
        ((devides? test-divisor n)
            test-divisor)
        (else (find-divisor-iter n (1+ test-divisor)))))
(define (devides? a b)
    (= (remainder b a) 0))
(define (find-divisor n)
    (find-divisor-iter n 2))
(define (prime? n)
    (= (find-divisor n) n))
