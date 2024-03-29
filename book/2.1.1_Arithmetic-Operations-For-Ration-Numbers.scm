;;(make-rat n d)
;;(numer x) return numerator
;;(denom x) return denominator
(load "./book/p32_1.2.5_GCD.scm")

(define (add-rat x y)
  (make-rat (+  (* (numer x) (denom y))
                (* (numer y) (denom x)))
            (* (denom x) (denom y)))
)
(define (sub-rat x y)
  (make-rat (-  (* (numer x) (denom y))
                (* (numer y) (denom x)))
            (* (denom x) (denom y)))
)
(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y)))
)
(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y)))
)
(define (equal-rat? x y)
  (=  (* (numer x) (denom y))
      (* (numer y) (denom x)))
)

(define (make-rat n d)
  (let (
    (g (gcd n d))
  )
  (cons (/ n g)
        (/ d g))
  ))

(define (make-rat n d)
  (let ((g (abs (gcd n d))))
  (if (< d 0)
    (cons (/ (- n) g) (/ (- d) g))
    (cons (/ n g) (/ d g)))))

(define (numer x) (car x))
(define (denom x) (cdr x))
(define (print-rat x)
  (display (numer x))
  (display "/")
  (display (denom x))
  (newline)
)

(define one-half (make-rat 1 2))
(define one-third (make-rat 1 3))



