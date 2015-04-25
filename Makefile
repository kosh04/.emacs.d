EMACS ?= emacs

test:
	${EMACS} -q -nw --batch -l test-startup.el
