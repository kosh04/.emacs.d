EMACS ?= emacs
EMACS_BATCH := $(EMACS) -batch -no-site-file

CASK ?= cask
CASK_PACKAGE_DIR := $(shell $(CASK) package-directory)

.PHONY: test

default: test

test: $(CASK_PACKAGE_DIR)
	$(CASK) exec $(EMACS_BATCH) -l test-startup.el

$(CASK_PACKAGE_DIR): Cask
	$(CASK) install
