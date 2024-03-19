(define (sqrt-iter guess x)
  (define (improve guess x) 
    (average guess x))
  (define (average guess x) 
    (/ (+ guess (/ x guess)) 2))
  (define (good-enough? new-guess guess x) 
    (< (/ (abs (- new-guess guess)) x) .001))

  (define new-guess (improve guess x))
  (if (good-enough? new-guess guess x) 
    new-guess
    (sqrt-iter new-guess x)))
  
(define (sqrt x)  
  (sqrt-iter 1.0 x))
