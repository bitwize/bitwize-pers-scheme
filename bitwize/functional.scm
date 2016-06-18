(define (partial-apply1 f a) (lambda l (apply f (cons a l))))
(define (partial-apply2 f a b) (lambda l (apply f (cons a (cons b l)))))
(define (partial-apply3 f a b c) (lambda l (apply f (cons a (cons b (cons c l))))))
(define (partial-apply f . rest) (lambda l (apply f (append rest l))))
(define (compose a b) (lambda xs (call-with-values (lambda () (apply b xs)) a)))
(define pipe (partial-apply fold compose))
(define (compose-all . fs) (apply pipe (reverse! fs)))
(define (conjunction . fs)
  (lambda (x)
    (cond
     ((null? fs) #f)
     ((null? (cdr fs))
      ((car fs) x))
     (else
      (and ((car fs) x) ((apply conjunction (cdr fs)) x))))))

(define (disjunction . fs)
  (lambda (x)
    (cond
     ((null? fs) #t)
     ((null? (cdr fs))
      ((car fs) x))
     (else
      (or ((car fs) x) ((apply disjunction (cdr fs)) x))))))
