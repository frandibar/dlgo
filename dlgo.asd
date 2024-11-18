(defsystem dlgo
  :version "0.1"
  :author "Francisco Dibar"
  :license "MIT"
  :depends-on (#:alexandria
	       #:binding-arrows
	       #:split-sequence
	       #:uiop
	       #:str)
  :components ((:module "src"
		:serial t
		:components ((:file "util")
			     (:file "constant")
			     (:file "point")
			     (:file "zobrist")
			     (:file "board")
			     (:file "main")
			     (:file "agent")
			     (:file "gtp")
			     (:file "sgf")
			     (:file "tui"))))
  :description "Go board engine with textual interface for two players and a dummy bot to play against."
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

(defsystem dlgo/tictactoe
  :version "0.1"
  :author "Francisco Dibar"
  :license "MIT"
  :depends-on (#:alexandria
	       #:binding-arrows)
  :components ((:module "src"
		:serial t
		:components ((:module "tictactoe"
			      :serial t
			      :components ((:file "constant")
					   (:file "point")
					   (:file "board")
					   (:file "main")
					   (:file "agent")
					   (:file "tui"))))))
  :description "Tic-tac-toe game for two players or against bot using minimax search.")
