(load "../homework/p41_1.3.2_lambda.scm")
(load "../common/math.scm")
(lambda (x) (+ x 4))
(lambda (x) (/ 1.0 (* x (+ x 2))))

(define (pi-sum a b)
    (sum (lambda (x) (/ 1.0 (* x (+ x 2)))))
        a 
        (lambda (x) (+ x 4))
        b)

(define (plus4 x) (+ x 4))
(define plus4 (lambda (x) (+ x 4)))
((lambda (x y z) (+ x y (square z))) 1 2 3)

(define (f x y)
    (define (f-helper a b)
        (+ (* x (square a))
            (* y b)
            (* a b)))
    (f-helper (+ 1 (* x y))
        (- 1 y)))

(define (f x y)
    (lambda (a b)
        (+ (* x (square a))
            (* y b)
            (* a b))
    (+ 1 (* x y))
    (- 1 y)))

(define (f x y)
    (let ((a (+ 1 (* x y)))
            (b (- 1 y)))
         (+ (* x (square a))
            (* y b)
            (* a b))))

(+ (let ((x 3)) (+ x (* x 10))) x)

(define (f g)
    (g 2))

(f square)
(f (lambda (z) (* z (+ z 1))))