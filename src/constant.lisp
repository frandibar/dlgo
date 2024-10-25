(defpackage #:dlgo.constant
  (:use #:common-lisp)
  (:import-from #:alexandria
		#:define-constant)
  (:export #:black
	   #:white
	   #:pass
	   #:resign
	   #:user-error
	   #:text
	   #:+small-board+
	   #:+medium-board+
	   #:+big-board+))

(in-package #:dlgo.constant)

(define-constant +small-board+ 9)
(define-constant +medium-board+ 13)
(define-constant +big-board+ 19)

;; Be able to identify user errors such as invalid moves from bugs.
(define-condition user-error (error)
  ((text :initarg :text
	 :reader text)))
