(define-library
  (bitwize parser)
  (import (scheme base) (srfi 1) (srfi 9) (srfi 26))
  (export make-buffered-reader buffered-reader? buffered-reader-next-char! buffered-reader-push-char! buffered-reader-push-string!
  match-char match-string match-char/pred match-end parse-one-of parse-all-of parse-all-of/list parse-multi/list parse-multi)
  (include "parser.scm"))
