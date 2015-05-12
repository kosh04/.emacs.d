EMACS ?= emacs
EMACS_BATCH = $(EMACS) -batch -no-site-file

CASK ?= cask
CASK_PACKAGE_DIR := $(shell $(CASK) package-directory)

COMPILE.el = $(EMACS_BATCH) -f batch-byte-compile

SRC.el := $(filter-out %-test.el, $(wildcard site-lisp/*.el))

default: compile test

compile: $(SRC.el:.el=.elc)

%.elc: $(CASK_PACKAGE_DIR)
%.elc: %.el
	$(CASK) exec $(COMPILE.el) $<

test: $(CASK_PACKAGE_DIR)
	$(CASK) exec $(EMACS_BATCH) -l test-startup.el

$(CASK_PACKAGE_DIR): Cask
	$(CASK) install

clean:
	$(RM) $(SRC.el:.el=.elc)

.PHONY: compile test clean
