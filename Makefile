EMACS ?= emacs
EMACS_BATCH = $(EMACS) -batch -no-site-file

COMPILE.el = $(EMACS_BATCH) -f batch-byte-compile

CASK ?= cask
CASK_PACKAGE_DIR := $(shell $(CASK) package-directory)


default: compile test

compile: $(wildcard site-lisp/*.elc)

%.elc: %.el
	$(COMPILE.el) $<

test: $(CASK_PACKAGE_DIR)
	$(CASK) exec $(EMACS_BATCH) -l test-startup.el

$(CASK_PACKAGE_DIR): Cask
	$(CASK) install

.PHONY: compile test
