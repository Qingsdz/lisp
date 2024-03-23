;;

(define (square x)
  (* x x))
(cons (cons 1 2)
      (cons 2 3))

(cons (cons 1 
            (cons 2 3))
      4)

(cons 1
      (cons 2
            (cons 3
                  (cons 4 0))))

(list 1 2 3 4)
(define odds (list 1 3 5 7))
(define evens (list 2 4 6 8))

(define one-through-four (list 1 2 3 4))

(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (-1+ n))))

(define (length items)
  (if (null? items)
      0
      (1+ (length (cdr items)))))

(define (length items)
  (define (length-iter a count)
    (if (null? a)
        count
        (length-iter (cdr a)
                      (1+ count))))
  (length-iter items 0)                      
)

;;;(递归) 为了不去找尾巴 所以要在开头添加元素
(define (append list1 list2)
  (if (null? list1)
    list2
    (cons (car list1)
          (append (cdr list1) list2))))

;;;Ex 2.17 last-pair
(define (last-pair list)
  (if (null? (cdr list))
      (car list)
      (last-pair (cdr list))
  )
)

;;;Ex 2.18 reverse
(define (reverse list)
  (if (null? list)
      list
      (append (reverse (cdr list)) (cons (car list) '()))))

;;;Ex 2.19 us-coins
(define us-coins
  (list 50 25 10 5 1))
(define uk-coins
  (list 100 50 20 10 5 2 1 0.5))

(define (cc amount coin-values)
  (cond ((= amount 0)
          1)
        ((or (< amount 0) (no-more? coin-values))
          0)
        (else
          (+ (cc
              amount
              (except-first-denomination coin-values))
             (cc
              (- amount
                (first-denomination
                coin-values))
             coin-values)))))

(define (first-denomination coin-values)
  (car coin-values))
(define (no-more? coin-values)
  (null? coin-values))
(define (except-first-denomination coin-values)
  (cdr coin-values))

;;Ex 2.20
(define (f x y . z)
  (display x)
  (display y)
  (display z))


(define (same-pairty x . z)
  (define (process x)
    (if (even? x)
      1
      0)) ;;;此处可以更正为过程 
  (define od 
    (process x))
  (define pick-up (trace-lambda pick-up (items)
    (cond 
      ((null? items) 
        items)
      ((= od (process (car items)))
        (cons (car items) (pick-up (cdr items))))
      (else 
        (pick-up (cdr items))))))
  (cons x (pick-up z))
)

;;;Mapping over lists
(define scale-list 
  (trace-lambda scale-list (items factor)
  (if (null? items)
      '()
      [cons (* (car items) factor)
            (scale-list (cdr items) factor)]))
)

(define map1 (trace-lambda map1 (proc items)
  (if (null? items)
      items
      [cons (proc (car items))
            (map1 proc (cdr items))]
)))

(define scale-list (trace-lambda scale-list (items factor)
  (map (lambda (x) (* x factor)) items)
))


;;;Ex 2.21 Square-list
(define (square-list items)
  (if (null? items)
      items
      (cons (let (
                (p (car items))
              )
              (* p p)  
            ) 
            (square-list (cdr items)))
  )  
) 

(define (square-list items)
  (map (lambda (x) (* x x)) items))

;;;Ex 2.22
;;; 包括前面的append 都是在列表前增加元素或者使用递归来保证list的顺对
;;; 若希望在构造时候保持序对，可以使用append来拼接表
(define (square-list items)
  (define (make-square-list num)
    (cons (square num) '()))
  (define iter (trace-lambda iter (now result)
    (if (null? now) 
      result
      (iter (cdr now) (append result 
                              (make-square-list (car now))))
    )  
  ))
  (iter (cdr items) (cons (square (car items)) '()))
)
;;;;书中的错误答案 正是迭代版本的reserve
(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        [iter (cdr things)
              (cons (square (car things))
                    answer)]
    )
  )
  (iter items nil)
)

;;;2.23 for-each
(define (for-each proc items)
  (if (null? items)
      #t
      (begin  (proc (car items))
              (for-each proc (cdr items)))
  )
)

;;2.2.2 Hierarchical Structures
(cons (list 1 2) (list 3 4))
(define x (cons (list 1 2) (list 3 4)))

(define count-leaves (trace-lambda count-leaves (x)
  (cond ;;;此处考虑三种情况 空 单个元素 pair
    ((null? x) 0)
    ((not (pair? x)) 1)
    (else (+  (count-leaves (car x))
              (count-leaves (cdr x)))) 
  ))
)

;;;Ex 2.26 
(define x (list 1 2 3))
(define y (list 4 5 6))

;;Ex 2.28
(define deep-reverse (trace-lambda deep-reverse (items)
  (cond 
    ((null? items) items)
    ((not (pair? items)) 
      (cons items '()))
    (else (cons (deep-reverse (cdr items))
                  (deep-reverse (car items))))
  ))
)


