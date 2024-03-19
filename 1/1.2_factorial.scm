(define (factorial n)  
  (if (= n 1) 
  1
  (* n (factorial (-1+ n)))))

(define (factorial-iter product couner n)
  (if (> counter n) 
      product 
      (factorial-iter (* product counter) (1+ counter) n)))
(define (factorial n)
  (factorial-iter 1 1 n))