(load "./common/math.scm")

(define (fixed-point f first-guess)
  (define tolerance 0.001)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let (
        (next (f guess))
      )
      (display next)
      (newline)
      (if (close-enough? guess next)
        next
        (try next)
      )
    ))
  (try first-guess)  
)  

;;Procedures as Returned Value
(define square
  (lambda (x) (* x x)))

;;此处是函数式编程 即对f进行操作 x作为操作后的变量
(define (average-dump f)
  (lambda (x) (average x (f x))))

;;将average-damp作为新的迭代函数
(define (sqrt x)
  (fixed-point
    (average-damp
      (lambda (y) (/ x y)))
    1.0))

(define (cube-root x)
  (fixed-point
    (average-damp
      (lambda (y) (/ x (square y))))
    1.0))

;;Newton's method
(define dx 0.00001)
(define (deriv g)
  (lambda (x) 
    (/  (- (g (+ x dx)) (g x)) 
        dx)))

(define (cube x) (* x x x))

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x)
            ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point  (newton-transform g)
                guess))

(define (sqrt x)
  (newtons-method
    (lambda (y)
      (- (square y) x))
    1.0))

;; Abstractions and first-class procedures
(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

(define (sqrt x)
  (fixed-point-of-transform (lambda (y) (/ x y)) average-damp 1.0))

(define (sqrt x)
  (fixed-point-of-transform (lambda (y) (- (square y) x)) newton-transform 1.0))

