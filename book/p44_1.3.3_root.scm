;;Finding roots of equations by the half-interval method
(define (close-enough? x y)
  (< (abs (- x y)) 0.01))

(define (average a b)
  (/ (+ a b) 2))

(define (search f neg-point pos-point)
  (let  (
    (midpoint (average neg-point pos-point))
    )
    (if (close-enough? neg-point pos-point)
      midpoint
      (let  (
        (test-value (f midpoint))
        )
        (cond
          ((positive? test-value)
            (search f neg-point midpoint))
          ((negative? test-value)
            (search f midpoint pos-point))
          (else
            midpoint)
        )
      )
    )
  )
)

(define (half-interval-method f a b)
  (let  (
    (a-value (f a))
    (b-value (f b))
    )
    (cond 
      ((and (negative? a-value) (positive? b-value))
        (search f a b))
      ((and (positive? a-value) (negative? b-value))
        (search f b a))
      (else (error "Value are not of opposite sign" a b))
    )
  )
)

;;Finding fixed points of functions
(define tolerance 0.0001)

(define (fixed-point f first-guess)
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

;;l_1.36_modify fixed-point
(define (sqrt x)
  (fixed-point (lambda (y) (average y (/ x y))) x))

(define (golden x)
  (fixed-point (lambda (y) (+ 1 (/ 1 y))) x))

(define (xx x)
  (fixed-point (lambda (x) (/ (log 1000) (log x))) x))

(define (xx-average x)
  (fixed-point (lambda (x) (/ (+ (/ (log 1000) (log x)) x) 2)) x))