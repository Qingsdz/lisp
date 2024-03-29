(define (search f neg-point pos-point)
    (let ((midpoint 
            (average neg-point pos-point)))
        (if (close-enough? neg-point pos-point)
        midpoint
            (let (
                (test-value (f midpoint)))
              (cond ((positive? test-value)
                        (search f neg-point midpoint))
                    ((negative? test-value)
                      sdsd  (search f midpoint pos-point))
                    (else midpoint))))))


;;写一段递归程序
