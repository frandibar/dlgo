(defpackage dlgo.util
  (:use #:common-lisp)
  (:export #:partition))

(in-package :dlgo.util)

;; TODO: replace with lib serapeum
;; https://github.com/ruricolist/serapeum/blob/master/REFERENCE.md#partition-pred-seq-key-start-end-key
(defun partition (predicate list)
  "Partition LIST into two lists based on PREDICATE.
   Returns two lists: (matching-elements non-matching-elements)."
  (loop for item in list
        if (funcall predicate item)
          collect item into true-list
        else
          collect item into false-list
        finally (return (values true-list false-list))))
