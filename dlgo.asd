(defsystem dlgo
  :version "0.1"
  :author "Francisco Dibar"
  :license "MIT"
  :depends-on (#:alexandria
	       #:binding-arrows
	       #:split-sequence)
  :components ((:module "src"
		:serial t
		:components ((:file "util")
			     (:file "constant")
			     (:file "point")
			     (:file "zobrist")
			     (:file "main")
			     (:file "agent")
			     (:file "sgf")
			     (:file "tui"))))
  :description ""
  :in-order-to ((test-op (test-op "dlgo/tests")))
  :build-operation "program-op"
  :build-pathname "dlgo-cli"
  :entry-point "dlgo.tui:start-game")

(defsystem dlgo/tests
  :version "0.1"
  :author "Francisco Dibar"
  :license "MIT"
  :depends-on (#:dlgo
	       #:alexandria
	       #:binding-arrows
	       #:1am)
  :components ((:file "package.test")
	       (:module "tests"
		:components
		((:file "main")
		 (:file "tui")
		 (:file "sgf")
		 (:file "util"))))
  :description "Test system for dlgo"
  :perform (test-op (op c)
		    (funcall (read-from-string "dlgo/tests:run-tests"))))
