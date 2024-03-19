(define (new-if preficate then-clause else-clause)
  (cond (preficate then-clause)
        (else else-clause)))