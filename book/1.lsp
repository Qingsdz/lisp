1.2
(/  (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5)))))
    (* 3 (- 6 2) (- 2 7)))
1.5
(define (p) (p)) 
(define (test x y) 
    (if (= x 0) 
        0
        y))

(test 0 (p))