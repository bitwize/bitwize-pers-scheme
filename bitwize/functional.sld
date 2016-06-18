(define-library
  (bitwize functional)
  (import (scheme base) (srfi 1))
  (export partial-apply partial-apply1 partial-apply2 partial-apply3 compose pipe compose-all conjunction disjunction)
  (include "functional.scm"))
