(defpackage senior-project/utils
  (:use :cl)
  (:export #:range
           #:smoosh
           #:zip
           #:count-repeats
           #:find-duplicates
           #:search-and-delete
  )
)
(in-package :senior-project/utils)

(defun range (start end)
  "Construct a list of numbers from START through END, where consecutive terms have a difference of 1"
  (let (
        (counter (let ((idx -1)) (lambda () (incf idx))))
        (guidelist (make-list (- end start)))
       )
    (loop for i in guidelist collect
      (+ start (funcall counter))
    )
  )
)

(defun smoosh (list)
  "Collapse only the first layer of branches of a tree. EG: (((1 2)) ((3 4) (5 6 7) (8 9))) becomes ((1 2) (3 4) (5 6 7) (8 9))"
  (apply 'append list)
)

(defun zip (func list1 list2 &rest lists)
  "applies FUNC to all elements in each list, grouping them by their index. If one list is shorter than the others, longer lists will have their extra elements ignored."
  (let* (
          (args (append (list list1 list2) lists))
          (len (apply 'min (mapcar (lambda (x) (list-length x)) args)))
        )
    (loop for i in (range 0 len) collect
      (apply func (mapcar (lambda (x) (nth i x)) args))
    )
  )
)

(defun count-repeats (list)
  "returns an alist where each pair contains an element of the input list in the CAR, and the number of times it occurs in the CDR."
  (let  (
          (kinds (remove-duplicates list))
        )
    (zip 'cons
      kinds
      (mapcar (lambda (x) (apply '+ x))
        (mapcar (lambda (k)
                        (mapcar (lambda (e) (if (equal e k) 1 0)) list)
                )
                kinds
        )
      )
    )
  )
)

(defun find-duplicates (list &key (test 'equal))
  (loop for key in (remove-duplicates list) collect
    (cons
      (search-and-delete nil
        (loop for idx in (range 0 (list-length list)) collect
          (if (funcall test key (nth idx list))
              idx
          )
        )
      )
      key
    )
  )
)

(defun search-and-delete (value list)
  "Remove all occurrences of a value from a list."
  (remove-if (lambda (c) (eq c value)) list)
)
