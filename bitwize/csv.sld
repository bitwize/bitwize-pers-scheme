(define-library
  (bitwize csv)
  (import (scheme base) (srfi 1) (srfi 69) (bitwize parser))
  (export read-csv/field-header)
  (include "csv.scm"))
