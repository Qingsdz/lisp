;;(cont-frac  (lambda (i) 1.0) (lambda (i) 1.0) k)

(define (cont-frac-iter Ni Di k)
  (define (cont-iter k result)
    (if (= k 0)
      result
      (cont-iter (-1+ k) (/ (Ni k) (+ (Di k) result))))
  )
  (cont-iter k 0)
)

(define (cont-frac-rec Ni Di k)
  (define (cont-frac-rec-inter i)
    (if (= i k)
        (/ (Ni k) (Di k))
        (/ (Ni k) (+ (Di k) (cont-frac-rec-inter (1+ i))))
    )
  )
  (cont-frac-rec-inter 1)
)

(define (e-cf k)
  (+ 2
    (cont-frac-iter
      (lambda (n) 1.0)
      (lambda (n) 
        (if (= (remainder n 3) 2)
            (* (/ (1+ n) 3) 2)
            1.0))
      k)
  )
)

(define (tan-cf x k)
  (cont-frac-iter 
    (lambda (n) (if (= n 1) x (- (* x x))))
    (lambda (n) (-1+ (* 2 n))  )
    k)
)