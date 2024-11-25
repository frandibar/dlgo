(defsystem dlgo
  :version "0.1"
  :author "Francisco Dibar"
  :license "MIT"
  :depends-on (#:alexandria
	       #:binding-arrows
	       #:split-sequence
	       #:str
	       #:uiop)
  :components ((:module "src"
		:serial t
		:components ((:file "util")
			     (:file "constant")
			     (:file "point")
			     (:file "zobrist")
			     (:file "board")
			     (:file "game")
			     (:file "score")
			     (:file "main")
			     (:file "agent")
			     (:file "sgf")
			     (:file "mcts")
			     (:file "gtp")
			     (:file "tui"))))
  :description "Go board engine with textual interface for two players and a bot to play against."
  :in-order-to ((test-op (test-op "dlgo/tests")))
  :build-operation "program-op"
  :build-pathname "dlgo-cli"
  :entry-point "dlgo.tui:start-game")

(defsystem dlgo/tests
  :version "0.1"
  :author "Francisco Dibar"
  :license "MIT"
  :depends-on (#:1am
	       #:alexandria
	       #:binding-arrows
	       #:dlgo)
  :components ((:file "package.test")
	       (:module "tests"
		:serial t
		:components ((:file "main")
			     (:file "tui")
			     (:file "agent")
			     (:file "sgf")
			     (:file "score")
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
