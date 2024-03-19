(define (identity x) x)
(define (product term a next b)
    (if (> a b)
        1.0
        (* (term a) (product term (next a) next b))))

(define (product-iter term a next b)
    (define (iter a result)
        (if (> a b)
            result
            (iter (next a) (* result (term a)))))
    (iter a 1.0))

(define (pro-integers a b)
    (product identity a 1+ b))
(define (pro-integers-iter a b)
    (product identity a 1+ b))

(define (pi-product pro n)
    (define (process-moleculatr x)
        (if (odd? x)
            (-1+ x)
            x))
    (define (process-denominator x)
        (if (odd? x)
            x
            (-1+ x)))
    (define (term x)
        (/ (process-moleculatr x) (process-denominator x)))
    (pro term 3 1+ (+ n 3)))