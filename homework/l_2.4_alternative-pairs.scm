(define (cons x y)
  (lambda (m) (m x y)))

(define (car z)
  (z (lambda (p q) p)))

;;;z是一个过程 可以接受一个过程 用于代替m 对x y及进行操作
(define (cdr z)
  (z (lambda (p q) q)))

(define a (cons 1 2))
;a -> (lambda (m) (m x y))
;car a -> (a (lambda (p q) p))
;        ((cons 1 2) (lambda (p q) p))
;        ((lambda (m) (m x y)) (lambda (p q) p))

;;Ex 2.5
(define (cons x y)
  (*  (expt 2 x)
      (expt 3 y)))

(define (car z)
  (cond 
    ((= (remainder z 2) 0) (1+ (car (/ z 2))))
    (else 0)
  )
)
(define (cdr z)
  (cond
    ((= (remainder z 3) 0) (1+ (cdr (/ z 3))))
    (else 0)
  )
)
;;Ex 2.6 Mind-boggling pairs
(define zero (lambda (f) (lambda (x) x)))
(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(add-1 zero)
  (lambda (f) (lambda (x) (f ((zero f) x))))
  (lambda (f) (lambda (x) (f (((lambda (f) (lambda (x) x)) f) x))))
  