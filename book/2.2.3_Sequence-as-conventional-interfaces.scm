;;;the use of conventional interfaces
(load "./common/math.scm")
(define (sum-odd-squares tree)
  (cond 
    [(null? tree) 0]
    [(not (pair? tree))
      (if (odd? tree) (square tree) 0)]
    [else
      (+ (sum-odd-squares (car tree))
        (sum-odd-squares (cdr tree)))]
  )
)

(define tree1 (list 1 2 3 (list 4 5) (list 6 7)))

(define (fib k)
  (define (iter result pre i)
    (if (= i k)
        pre
        (iter (+ pre result) result (1+ i))
    )
  )
  (iter 1 0 0)
)

(define (even-fibs n)
  (define next (trace-lambda next (k)
    (if (> k n)
        '()
        (let 
          (
            (f (fib k))
          )
          (if (even? f)
              (cons f (next (1+ k)))
              (next (1+ k))
          )
        )
    )
  ))
  (next 0)
)
;;;Sequenec Opreations
(define (filter predicate sequence)
  (cond 
    [(null? sequence) sequence]
    [(predicate (car sequence))
      (cons (car sequence) 
            (filter predicate (cdr sequence)))]
    [else (filter predicate (cdr sequence))]
  )
)
;;;Accumulations
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence))
      )
  )
)
;;;enumerate-interfaces 
(define (enumerate-interval low high)
  (if (> low high) ;;;此处不包含等号 则 程序运行完high终止 所以会 [low high]
      '()
      (cons low 
            (enumerate-interval (1+ low) high))
  )
)
;;;enumerate the leaves of a tree 需要保持list结构
(define (enumerate-tree tree)
  (cond 
    [(null? tree) tree]
    [(not (pair? tree)) (list tree)]
    [else (append 
            (enumerate-tree (car tree))
            (enumerate-tree (cdr tree)))]
  )
)

(define (sum-odd-squares tree)
  (accumulate
    +
    0
    (map square (filter odd? (enumerate-tree tree)))
  )
)

(define (even-fibs n)
  (accumulate
    cons
    '()
    (filter even? (map fib (enumerate-interval 0 n)))
  )
)

(define (list-fib-squares n)
  (accumulate
    cons
    '()
    (map square (map fib (enumerate-interval 0 n)))
  )
)

(define (product-of-squares-of-odd-elements sequence)
  (accumulate
    *
    1
    (map square (filter odd? sequence))
  )
)

(define (salary-of-highext-paid-programmer recodes)
  (accumulate
    max
    0
    (map salary 
          (filter programmer? records))
  )
)