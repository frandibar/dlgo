(defpackage #:dlgo.constant
  (:use #:common-lisp)

  (:import-from #:alexandria
		#:define-constant)

  (:export #:+small-board+
	   #:+medium-board+
	   #:+big-board+
	   #:+komi+
	   #:black
	   #:white
	   #:pass
	   #:resign
	   #:user-error
	   #:text))

(in-package #:dlgo.constant)

(define-constant +small-board+ 9)
(define-constant +medium-board+ 13)
(define-constant +big-board+ 19)

(define-constant +komi+ 7.5)

;; Be able to identify user errors such as invalid moves from bugs.
(define-condition user-error (error)
  ((text :initarg :text
	 :reader text)))
