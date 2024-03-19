(define (get_bigger a b) 
        (if (> a b) a b))
(define (get_smaller a b)
        (- (get_bigger (- a) (- b) )))
(define (sum_bigger a b c)  
        (define d (get_bigger a b))
        (define f (get_smaller a b))
        (if (> c d) (+ c d) (if (< c f) (+ f b) (+ c d))))

(define (a-plus-abs-b a b) 
        ((if (> b 0) + -) a b))

        