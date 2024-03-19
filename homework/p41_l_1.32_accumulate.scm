;;递归的时候需要保存状态 需要传入累计的参数
(define (accumulate combiner null-value term a next b)
    (if (> a b)
        null-value
        (combiner (term a) 
            (accumulate combiner null-value term (next a) next b))))

;;由于是迭代 所以不需要保存所有状态 迭代的时候只需要传入变动的参数
(define (accumulate-iter combiner null-value term a next b)
    (define (iter a result)
        (if (> a b)
            result
            (iter (next a) (combiner (term a) result))))
    (iter a null-value))

(define (sum term a next b)
    (accumulate-iter + 0.0 term a next b))

(define (product term a next b)
    (accumulate-iter * 1.0 term a next b))