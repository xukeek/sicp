(define (last-pair pair)
  (cond ((null? pair) '())
        ((null? (cdr pair)) (car pair))
        (else (last-pair (cdr pair)))))
