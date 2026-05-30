(defpackage senior-project/calculation
  (:use :cl :alexandria :senior-project/utils :senior-project/enumeration)
  (:export #:ferrers-conj
            :solution
           #:make-solution
           #:make-solution-list
           #:solution-to-csv-part
           #:csv-from-params
           #:group-solutions
           #:majorizes
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
    (format nil "\"~s\",\"~s\",~s,\"~s\",\"~s\"~%" colsum rowsum paddng colmns (list-length colmns)) 
  )
)

(defun csv-from-params (verts degree-list &key (padding 1) (stream t))
  (format stream (apply 'concatenate 'string
                     (mapcar
                       (lambda (s) (solution-to-csv-part s))
                       (group-solutions (make-solution-list verts degree-list :padding padding))
                     )
              )
  )
)

(defun group-solutions (solutions)
  "group solutions by whether their row and column sums are the same, collecting the column vectors into a tree."
  (let* (
          (group-data (find-duplicates
                        (mapcar (lambda (x) (list (solution-column-sums x) (solution-row-sums x) (solution-padding x)))
                                solutions
                        )
                      )
          )
          (idx-groups (remove-duplicates (mapcar (lambda (d) (car d)) group-data) :test 'equal))
          (indices (mapcar (lambda (i) (car i)) idx-groups))
        )
    (mapcar (lambda (idxs) (make-solution
                             :column-sums (solution-column-sums (nth (car idxs) solutions))
                             :row-sums    (solution-row-sums (nth (car idxs) solutions))
                             :padding     (solution-padding (nth (car idxs) solutions))
                             :columns (mapcar (lambda (i) (solution-columns (nth i solutions))) idxs)
                           )
            )
            idx-groups
    )
  )
)

(defun majorizes (vec1 vec2 &key (test '<=))
  "Returns T if vec1 majorizes vec2, and NIL otherwise."
  (let* (
          (sizediff (abs (- (list-length vec1) (list-length vec2))))
          (first-vector (reverse (if (<= (list-length vec1) (list-length vec2))
                            (append vec1 (make-list sizediff :initial-element 0))
                            vec1
                        ))
          )
          (second-vector (reverse (if (<= (list-length vec2) (list-length vec1))
                             (append vec2 (make-list sizediff :initial-element 0))
                             vec2
                         ))
          )
        )
    (eval `(and ,@(loop for run on (range 0 (list-length first-vector)) collect
      (funcall test (apply '+ (mapcar (lambda (x) (nth x first-vector)) run))
          (apply '+ (mapcar (lambda (x) (nth x second-vector)) run))
      )
    )))
  )
)
