(defpackage senior-project/enumeration
  (:use :cl :alexandria :senior-project/utils)
  (:export #:gen-part
           #:count-columns
           #:gen-columns
           #:gen-tuples
           #:gen-row-vectors
           #:count-row-vectors
  )
)
(in-package :senior-project/enumeration)

(defun gen-part (length position)
  "Create a list with given length filled with 0, with the exception of the entry at the given position, which will be 1."
  (let (
        (output (make-list length :initial-element 0))
       )
    (progn
      (setf (nth position output) 1)
      output
    )
  )
)

(defun count-columns (verts degree &key (padding 1))
  "Returns the number of possible columns of length VERTS that have a column sum of DEGREE, and where each 1 is followed by PADDING zeros."
  (binomial-coefficient (- (+ degree (- verts (* (+ 1 padding) (- degree 1)))) 1)
                        degree
  )
)

(defun gen-columns (verts degree &key (padding 1))
  "Generates all columns of length VERTS and sum DEGREE where each 1 is followed by PADDING zeros."
  (if (= degree 1)
      (mapcar (lambda (x) (gen-part verts x))
              (range 0 verts)
      )
      (smoosh (loop for i in (range 1 (+ 1 (- verts (* (+ 1 padding) (- degree 1))))) collect
                    (mapcar (lambda (x) (append (gen-part (+ padding i) (- i 1)) x))
                            (gen-columns (- verts (+ padding i)) (- degree 1) :padding padding)
                    )
              )
      )
  )
)

(defmacro make-tuples (verts degree-list &key (padding 1))
  (let  (
          (groups (mapcar (lambda (d) (gen-columns verts d :padding padding)) degree-list))
        )
    `(map-product 'list ,@(mapcar (lambda (l) (cons 'list (mapcar (lambda (m) (cons 'list m)) l))) groups))
  )
)

(defun gen-tuples (verts degree-list &key (padding 1))
  "Given a natural number VERTS and a list of the column sums, return a list containing tuples with every combination of a possible column for each degree. This function wraps a macro to fix a minor syntax gripe, so see SENIOR-PROJECT/COUNTING:MAKE-TUPLES"
  (eval `(make-tuples ,verts ,degree-list :padding ,padding))
)

(defun gen-row-vectors (verts degree-list &key (padding 1))
  "For given column sums in DEGREE-LIST, constructs all possible row sum vectors of length VERTS, where each 1 in a column is followed by PADDING zeros. This function WILL GENERATE DUPLICATES for the purpose of tracking which columns gave which sums."
  (let  (
          (tuples (gen-tuples verts degree-list :padding padding))
        )
    (mapcar (lambda (p) (eval `(zip '+ ,@(mapcar (lambda (c) (cons 'list c)) p)))) tuples)
  )
)


