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

;2.40
(define (flat-map proc seq)
  (accmulate append '() (map proc seq)))

(define (enumerate-interval a b)
  (if (> a b)
      '()
      (cons a (enumerate-interval (+ a 1) b))))

(define (unique-pairs n)
  (flat-map (lambda (i) 
              (map 
               (lambda (j) (cons i j)) 
               (enumerate-interval 1 (- i 1))))
            (enumerate-interval 2 n)))

;2.41
(define (triple-unique-pairs n)
  (flat-map 
           (lambda (i) (flat-map (lambda (j) 
                                   (map (lambda (z) 
                                          (list i j z))
                                        (enumerate-interval 1 (- j 1))))
                                 (enumerate-interval 2 (- i 1))))
           (enumerate-interval 3 n)))

(define (ordered-triples-sum n s)
  (filter (lambda (x) (= (+ (car x) (+ (cadr x) (caddr x))) s))
          (triple-unique-pairs n)))

;2.42
(define (is-safe? a l i)
  (cond ((null? l) #t)
        ((= (car l) a) #f)
        ((let ((k (/ (- a (car l)) i))) 
           (or (= k 1) (= k -1))) #f)
        (else (is-safe? a (cdr l) (+ i 1)))))

(define (queens board-size) 
  (define empty-board '())

  (define (safe? k positions)
    (is-safe? (car positions) (cdr positions) 1))
  
  (define (adjoin-position new-row k rest-of-queens)
    (cons new-row rest-of-queens))
  
  (define (queen-cols k) 
    (if (= k 0) 
        (list empty-board) 
        (filter 
         (lambda (positions) (safe? k positions)) 
         (flat-map 
          (lambda (rest-of-queens) 
            (map (lambda (new-row) 
                   (adjoin-position new-row k rest-of-queens)) 
                 (enumerate-interval 1 board-size))) 
          (queen-cols (- k 1)))))) 
  (queen-cols board-size)) 
