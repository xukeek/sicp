;2.17
(define (last-pair pair)
  (cond ((null? pair) '())
        ((null? (cdr pair)) (car pair))
        (else (last-pair (cdr pair)))))

;2.18
(define (reverse pair)
  (define (go p r)
    (if (null? p)
        r
        (go (cdr p) (cons (car p) r))))
  (go pair '()))
