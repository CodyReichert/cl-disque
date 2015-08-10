# Makefile
#
# Cody Reichert <codyreichert@gmail.com>
#
# $ make test (to run the test suite)
# $ make sbcl-repl (to load cl-disque in a SBCL repl.)

test:
	sbcl --noinform --non-interactive 		    \
		--load cl-disque.asd          		    \
		--load cl-disque-test.asd     		    \
		--eval '(ql:quickload :cl-disque-test)'     \
		--eval '(asdf:test-system :cl-disque-test)'

sbcl-repl:
	sbcl                                                \
		--load cl-disque.asd                        \
		--eval '(ql:quickload :cl-disque)'
