(load "1/1.6.lsp")

(define (sqrt-iter guess x)
  (define (improve guess x) 
    (average guess x))
  (define (average guess x) 
    (/ (+ guess (/ x guess)) 2))
  (define (good-enough? guess x) 
    (< (abs (- (* guess guess) x)) .001))

  (new-if (good-enough? guess x) 
    guess 
    (sqrt-iter (improve guess x) x)))
  
(define (sqrt x)  
  (sqrt-iter 1.0 x))




