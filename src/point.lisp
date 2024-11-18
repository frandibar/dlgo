(defpackage #:dlgo.point
  (:use #:common-lisp)
  (:import-from #:alexandria
		#:rcurry
		#:define-constant)
  (:export #:make-point
	   #:point-col
	   #:point-row
	   #:point-p
	   #:point-equal-p
	   #:point-neighbors
	   #:point-neighbors-on-grid
	   #:point-on-grid
	   #:point-to-coords
	   #:point-index
	   #:coords-to-point
	   #:+column-labels+))

(in-package #:dlgo.point)

(defstruct point
  "A point on the Go board."
  col row)

(defun point-equal-p (point-a point-b)
  "Return T if POINT-A is equal to POINT-B."
  (and
   (= (point-col point-a)
      (point-col point-b))
   (= (point-row point-a)
      (point-row point-b))))

(defun point-neighbors (point)
  "Return list of points adjacent to POINT."
  (let ((col (point-col point))
	(row (point-row point)))
    (list
     (make-point :col col :row (1- row))
     (make-point :col col :row (1+ row))
     (make-point :col (1- col) :row row)
     (make-point :col (1+ col) :row row))))

(defun point-neighbors-on-grid (point size)
  (remove-if-not (rcurry #'point-on-grid-p size)
		 (point-neighbors point)))

(define-constant +column-labels+ "ABCDEFGHJKLMNOPQRST"
  :test #'equal)

(defun point-to-coords (point)
  "Return a string with the board coordinates respresenting POINT.
i.e. C4 corresponds to (make-point :col 3 :row 4)."
  (format nil "~c~d"
	  (char +column-labels+ (1- (point-col point)))
	  (point-row point)))

(defun point-index (point board-size)
  "Return the corresponding vector index for POINT in the board grid."
  ;; The order is A1..A19,...,T1..T19.
  (+ (* (1- (point-col point)) board-size)
     (1- (point-row point))))

(defun point-on-grid-p (point size)
  "Return POINT if it's within the bounds of the board of SIZE,
otherwise NIL."
  (let ((col (point-col point))
	(row (point-row point)))
    (when (and (<= 1 col size)
	       (<= 1 row size))
      point)))


(defun column-to-int (column-label)
  "Return the number corresponding to COLUMN-LABEL."
  (1+ (position (char-upcase column-label)
		+column-labels+)))

(defun coords-to-point (coords)
  "Return a point corresponding to COORDS. COORDS is a string such as A1."
  (let ((col (column-to-int (char coords 0)))
	(row (parse-integer (subseq coords 1))))
    (make-point :col col :row row)))
