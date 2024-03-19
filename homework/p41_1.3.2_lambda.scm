(define (sum-integers a b)
    (if (> a b)
        0
        (+ a (sum-integers (+ a 1) b))))
(define (sum-cube a b)
    (if (> a b)
        0
        (+ (* a a a) (sum-cube (+ a 1) b))))
(define (pi-sum a b)
    (if (> a b)
        0
        (+ (/ 1.0 (* a (+ a 2))) (pi-sum (+ a 4) b))))

(define (name a b)
    (if (> a b)
        0
        (+ (trem a)
            (name (next a) b))))

(define (cube x)
    (* x x x))

(define (sum term a next b)
    (if (> a b)
    0
    (+ (term a)
        (sum term (next a) next b))))

(define (pi-sum a b)
    (sum (lambda (x) (/ 1.0 (* x (+ x 2))))
    a
    (lambda (x) (+ x 4))
    b))

(define (inc n) (+ n 1))
(define (sum-cubes a b)
    (sum cube a inc b))
(define (identity x) x)
(define (sum-integers a b)
    (sum identity a inc b))

(define (integral f a b dx)
    (* (sum f (+ a (/ dx 2.0)) (lambda (x) (+ x dx)) b)
    dx))
