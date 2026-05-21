(defpackage senior-project/calculation
  (:use :cl :alexandria :senior-project/utils :senior-project/enumeration)
  (:export #:ferrers-conj
            :solution
           #:make-solution
           #:make-solution-list
           #:solution-to-csv-part
           #:csv-from-params
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

(defstruct solution
  column-sums
  row-sums
  (padding 1 :type integer)
  columns
)

(defun make-solution-list (verts degree-list &key (padding 1))
  (let  (
          (columns-list (gen-tuples verts degree-list :padding padding))
          (row-list (gen-row-vectors verts degree-list :padding padding))
        )
    (mapcar (lambda (cs r)
                    (make-solution :column-sums degree-list
                                   :row-sums r
                                   :padding padding
                                   :columns cs
                    )
            )
            columns-list
            row-list
    )
  )
)

(defun solution-to-csv-part (solution)
  (let  (
          (colsum (solution-column-sums solution))
          (rowsum (solution-row-sums    solution))
          (paddng (solution-padding     solution))
          (colmns (solution-columns     solution))
        )
    (format nil "\"~s\",\"~s\",~s,\"~s\"~%" colsum rowsum paddng colmns)
  )
);; TODO: combine solutions with the same colsum, rowsum, and padding

(defun csv-from-params (verts degree-list &key (padding 1) &key (stream t))
  (format stream (apply 'concatenate 'string
                     (mapcar
                       (lambda (s) (solution-to-csv-part s))
                       (make-solution-list verts degree-list :padding padding)
                     )
              )
  )
)
