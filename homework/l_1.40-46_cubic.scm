;;(load "./book/1.3.4_Procedures-as-Returned-Value.scm")
(load "./common/math.scm")

;;Ex 1.40
(define (cubic a b c)
  (lambda (x) (+ (cube x) (* a (square x)) (* b x) c)))

;;;(newtons-method (cubic a b c) 1.0)

;;Ex 1.41
(define (double f)
  (lambda (x) (f (f x))))

;; (double double) -> (lambda (x) (double (double x)))

;;Ex 1.42
(define (compose f g)
  (lambda (x) (f (g x))))
 
;;Ex1.43 ;;(lambda (x) (f x)) == f
(define (repeated f n)
  (if (= n 1)
      f
      (lambda (x) ((repeated f (-1+ n)) (f x)))
  )
)

(define (repeated f n)
  (if (= n 1)
      f
      (compose f (repeated f (-1+ n)))))

;;;使用新的抽象 compose 可以略去lambda表达式 更容易表达
(define (repeated-iter f n)
  (define (iter i result)
    (if (= n i)
        result
        (iter (1+ i) (compose result f)))
  )
  (iter 1 f)
)

(define (repeated-iter f n)
  (define (iter i result)
    (if (= n i)
        result
        (iter (1+ i) (lambda (x) (f (result x))))))
  (iter 1 f)
)

;;1.44
(define dx 0.0001)
(define (smooth f)
  (lambda (x) (/  (+ (f (- x dx))
                    (f x)
                    (f (+ x dx))
                  ) 3)
  )
)

(define (smooth-n f n)
  (lambda (x) (((repeated-iter smooth n) f) x)))


;;O(logn)复杂度的指数乘法
(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n)  (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (-1+ n))))))

(define (fast-expt-iter b n)
  (define (iter n result)
    (cond ((= n 0) result)
          ((even? n) (iter (/ n 2) (square result)))
          (else (iter (-1+ n) (* b result)))))
  (iter n 1)
)

;;;实现log(n)时间复杂度的（repeated-n f)
(define (repeated-n-ologn f n)
  (cond ((= n 1) f)
        ((even? n) (compose (repeated-n-ologn f (/ n 2)) (repeated-n-ologn f (/ n 2))))
        (else (compose f (repeated-n-ologn f (-1+ n))))      
  )  
)

;;不使用compose的版本 更加复杂 容易搞乱
(define (repeated-n-ologn f n)
  (cond ((= n 1) f)
        ((even? n) (lambda (x) ((repeated-n-ologn f (/ n 2)) ((repeated-n-ologn f (/ n 2)) x))))
        (else (lambda (x) ((repeated-n-ologn f (-1+ n)) (f x))))      
  )
)

(define (average-dump-n-times f n)
  ((repeated average-dump n) f))

(define (average-dump-n-times f n)
  (lambda (x) (((repeated average-dump n) f) x)))
;;;烂尾啦

