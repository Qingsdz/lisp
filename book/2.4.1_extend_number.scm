(define (make-interval a b) (cons a b))
(define (lower-bound x) 
  (min (car x) (cdr x)))
(define (upper-bound x)
  (max (car x) (cdr x)))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (upper-bound y))
                  (+ (upper-bound x) (upper-bound y))))

(define (inverse-interval x)
  (make-interval (- (upper-bound x)) (- (lower-bound x))))

(define (mul-interval x y)
  (let (
    (p1 (* (lower-bound x) (lower-bound y)))
    (p2 (* (lower-bound x) (upper-bound y)))
    (p3 (* (upper-bound x) (lower-bound y)))
    (p4 (* (upper-bound x) (upper-bound y)))
    )
    (make-interval (min p1 p2 p3 p4)
                    (max p1 p2 p3 p4))
))

(define (mul-interval-nine x y)

)

(define (interval-cross-zero? x)
  (if 
    (and (< (lower-bound x) 0) (> (upper-bound x) 0))
      #t
    #f
  ))

(define (div-interval x y)
  (if (interval-cross-zero? y)
    (error "div a cross zero number" y)
    (mul-interval x
              (make-interval (/ 1.0 (upper-bound y))
                              (/ 1.0 (lower-bound y))))
  ))

(define (sub-interval x y)
  (add-interval x (inverse-interval y)))


(define (display-interval x)
  (display "(")
  (display (lower-bound x))
  (display ",")
  (display (upper-bound x))
  (display ")")
  (newline))

(define a (make-interval 2 8))
(define b (make-interval 4 6))
(define cross-zero-number (make-interval (- 2) 3))
