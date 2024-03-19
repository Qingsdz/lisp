(define (sum term a next b)
    (define (iter a result)
        (if (> a b)
            result
            (iter (next a) (+ result (term a)))))
    (iter a 0))

(define (cube x) (* x x x))

(define (sum-integers a b)
    (define (identity x) x)
    (sum identity a 1+ b))

(define (sum-cube-integers a b)
    (sum cube a 1+ b))