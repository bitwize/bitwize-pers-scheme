(define-record-type buffered-reader
  (really-make-buffered-reader buf port)
  buffered-reader?
  (buf buffered-reader-buffer buffered-reader-set-buffer!)
  (port buffered-reader-port))

(define (make-buffered-reader port)
  (really-make-buffered-reader '() port))

(define (buffered-reader-push-char! br c)
  (buffered-reader-set-buffer!
   br
   (cons c
	 (buffered-reader-buffer br))))

(define (buffered-reader-push-string! br s)
  (for-each (cut buffered-reader-push-char! br <>) (reverse! (string->list s))))

(define (buffered-reader-push-item! br i)
  (cond
   ((char? i) (buffered-reader-push-char! br i))
   ((string? i) (buffered-reader-push-string! br i))
   ((list? i) (for-each (lambda (j) (buffered-reader-push-item! br j)) (reverse i)))))

(define (buffered-reader-next-char! br)
  (let* ((buf (buffered-reader-buffer br)))
    (cond
     ((null? buf)
      (let* ((c (read-char
		 (buffered-reader-port br))))
	(if (not (char? c)) #f c)))
     (else
      (buffered-reader-set-buffer! br (cdr buf))
      (car buf)))))

(define (match-char c)
  (lambda (br)
    (let* ((c2 (buffered-reader-next-char! br)))
      (cond
       ((not c2) #f)
       ((not (char=? c2 c)) (buffered-reader-push-char! br c2) #f)
       (else (string c2))))))

(define (match-string str)
  (lambda (br)
    (let* ((l (string-length str)))
      (let loop ((i 0)
		 (acc '()))
	(if
	 (>= i l)
	 str
	 (let* ((c (string-ref str i))
		(c2 (buffered-reader-next-char! br)))
	  (cond
	   ((or (not c2) (not (char=? c2 c)))
	    (begin
	      (if c2 (buffered-reader-push-char! br c2))
	      (for-each (cut buffered-reader-push-char! br <>) acc)
	      #f))
	   (else (loop (+ i 1) (cons c acc))))))))))

(define (match-char/pred p)
  (lambda (br)
      (let* ((c (buffered-reader-next-char! br)))
	(cond
	 ((not c) #f)
	 ((not (p c)) (buffered-reader-push-char! br c) #f)
	 (else (string c))))))

(define (match-end br)
  (if (buffered-reader-next-char! br) #f ""))

(define (parse-all-of/list . parsers)
  (lambda (br)
    (let loop ((fs parsers)
	       (results '()))
      (if
       (null? fs)
       (reverse! results)
       (let* ((result ((car fs) br)))
	 (cond
	  ((not result) (for-each (cut buffered-reader-push-item! br <>) results) #f)
	  (else (loop (cdr fs) (cons result results)))))))))

(define (parse-multi/list parser)
  (lambda (br)
    (let loop ((results '()))
      (let* ((result (parser br)))
	(cond
	 ((not result) (reverse! results))
	 (else (loop (cons result results))))))))

(define (parse-one-of . parsers)
  (lambda (br)
    (let loop ((fs parsers))
      (if
       (null? fs)
       #f
       (let ((result ((car fs) br)))
	 (cond
	  ((not result) (loop (cdr fs)))
	  (else result)))))))

(define (transform-result parser transformer)
  (lambda (br)
    (let* ((result (parser br)))
      (if (not result) #f
	  (transformer result)))))

(define (parse-all-of . parsers)
  (transform-result (apply parse-all-of/list parsers) (cut apply string-append <>)))

(define (parse-multi parser)
  (transform-result (parse-multi/list parser) (cut apply string-append <>)))
