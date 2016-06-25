(define unquoted-string
  (parse-multi (match-char/pred
		(lambda (c) (not (member c '(#\return #\newline #\" #\,)))))))

(define quoted-string
  (transform-result
   (parse-all-of/list
    (match-char #\")
    (parse-multi
     (parse-one-of
      (match-char/pred
       (lambda (c) (not (member c '(#\")))))
      (transform-result (match-string "\"\"") (lambda (x) "\""))))
    (match-char #\"))
   cadr))

(define separator (transform-result (match-char #\,) (lambda (x) 'separator)))

(define field
  (parse-one-of quoted-string unquoted-string))
(define end-row
  (transform-result (parse-one-of (match-string "\n") (match-string "\r\n")) (lambda (x) '())))

(define row
  (parse-one-of
   (transform-result
    end-row
    (lambda (x) '()))
   (transform-result
    (parse-all-of/list
     field
     (transform-result
      (parse-multi/list
       (parse-all-of/list
	separator
	field))
       (lambda (x) (apply append x)))
     end-row)
    (lambda (x) (filter (lambda (y) (not (eq? y 'separator)))
			(cons (car x) (cadr x)))))))

(define (read-csv/field-header port)
  (let* ((br (make-buffered-reader port))
	 (header-row (row br)))
    (let loop ((r (row br))
	       (l '()))
      (cond
       ((not r) (reverse! l))
       ((null? r) (loop (row br) l))
       (else
	(let* ((ht (make-hash-table)))
	  (for-each (lambda (a b) (hash-table-set! ht a b))
		    header-row
		    r)
	  (loop (row br) (cons ht l))))))))
