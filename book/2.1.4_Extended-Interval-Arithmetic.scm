;;;实现make-interval，upper-bound lower-bound 在构造的时候并不保证序关系
(define (make-interval a b) 
      (cons a b))
(define (upper-bound x) (max (car x) (cdr x)))
(define (lower-bound x) (min (car x) (cdr x)))

(define (display interval) 
  (display "[")
  (display (lower-bound interval))
  (display ",")
  (display (upper-bound interval))
  (display "]")
  (newline))

;;;定义加法
(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                (+ (upper-bound x) (upper-bound y))))
;;;定义减法 构造的时候要考虑两个参数的低值和高值
(define (negate-interval x)
  (make-interval (- (upper-bound x)) (- (lower-bound x))))

(define (sub-interval x y)
  ((add-interval x (minus-interval y))))

;;;定义乘法
(define (mul-interval x y)
  (let (
    (p1 (* (lower-bound x) (lower-bound y)))
    (p2 (* (lower-bound x) (upper-bound y)))
    (p3 (* (upper-bound x) (lower-bound y)))
    (p4 (* (upper-bound x) (upper-bound y)))
    )
    (make-interval (min p1 p2 p3 p4)
                    (max p1 p2 p3 p4))
  )
)
;;;定义除法
(define (div-interval x y)
  (mul-interval x
      (make-interval (/ 1.0 (upper-bound y))
                    (/ 1.0 (lower-bound y)))))

;;;lanverb


  