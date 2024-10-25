(in-package #:dlgo/tests)

(test test-partition ()
  (multiple-value-bind (even odd)
      (partition #'evenp (list 1 2 3 4 6 5))
    (is (equal even '(2 4 6)))
    (is (equal odd '(1 3 5)))))
