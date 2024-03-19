(load "common/math.scm")
(define (cubic-iter guess x)
  (define (guess-enough? new-guess guess)
    (< (abs (/ (- new-guess guess) guess)) .001))
  (define (improve guess x)
    (/ (+ (/ x (square guess)) (* 2 guess)) 3))

  (define new-guess (improve guess x))
  (if (guess-enough? new-guess guess)
    new-guess
    (cubic-iter new-guess x)))

(define (cubit x) (cubic-iter 1.0 x))