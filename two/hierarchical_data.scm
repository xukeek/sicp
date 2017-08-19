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

;2.29
(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define left-branch car)
(define right-branch cadr)
(define branch-length car)
(define branch-structure cadr)

(define (branch-weight branch)
  (let ((s (branch-structure branch)))
    (if (pair? s)
        (branch-weight s)
        s)))

(define (total-weight mobile)
  (+ (branch-weight (left-branch mobile)
     (branch-weight (right-branch mobile)))))

(define (balanced mobile)
  (define (torque branch)
    (* (branch-length branch)
       (branch-weight branch)))
  (let ((lj (torque (left-branch mobile)))
        (rj (torque (left-branch mobile)))
        (lbs (branch-strutcture (left-branch mobile)))
        (rbs (branch-strutcture (right-branch mobile))))
    (cond ((not (= lj rj)) #f)
          ((and (pair? rbs) (pair? lbs)) (and (balanced lbs) (balanced rbs)))
          ((pair? lbs) (balanced lbs))
          ((pair? rbs) (balanced rbs))
          (else #t))))

(define (accmulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accmulate op initial (cdr sequence)))))

;2.34
(define (horner-eval x coefficient-sequence)
  (accmulate (lambda (this-coeff higher-terms) (+ this-coeff (* x higher-terms)))
             0
             coefficient-sequence))

;2.35
(define (count-leaves t)
  (accmulate 
   +
   0
   (map (lambda (x) 
          (if (pair? x)
              (count-leaves x)
              1)) t)))

;2.36
(define (accmulate-n op init seqs)
  (if (null? (car seqs))
      '()
      (cons (accmulate op init (map car seqs))
            (accmulate-n op init (map cdr seqs)))))

;2.37
(define (dot-product v w)
  (accmulate + 0 (map * v w)))

(define (matrix-*-verctor m v)
  (map (lambda (mi) (dot-product mi v)) m))

(define (transpose mat)
  (accmulate-n cons '() mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (mi) (matrix-*-verctor cols mi)) m)))

;2.39
(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(define (reverse sequence)
  (accmulate (lambda (x y) (append y (list x))) '() sequence))

(define (reverse2 sequence)
  (fold-left (lambda (x y) (cons y x)) '() sequence))

(reverse (list 1 2 3 4))
