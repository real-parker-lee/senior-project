(defpackage senior-project/calculation
  (:use :cl :alexandria :senior-project/utils :senior-project/enumeration)
  (:export #:ferrers-conj
  )
)
(in-package :senior-project/calculation)

(defun ferrers-conj (vec-list size)
  "computes the conjugate of a non-negative integral vector (list of nonnegative whole numbers) as defined on page 60 of Nelson's work."
  (mapcar (lambda (l) (apply '+ l))
    (loop for i in (range 1 (+ 1 size)) collect
      (mapcar (lambda (x) (if (<= i x) 1 0)) vec-list)
    )
  )
)
