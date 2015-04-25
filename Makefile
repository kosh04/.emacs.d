EMACS ?= emacs
EMACS_BATCH := $(EMACS) -batch -no-site-file

CASK ?= cask
CASK := EMACS=$(EMACS) $(CASK)
CASK_PACKAGE_DIR := $(shell $(CASK) package-directory)

default: cask_install test

cask_install $(CASK_PACKAGE_DIR): Cask
	$(CASK) install

test: 
	$(CASK) exec $(EMACS_BATCH) -l test-startup.el

clean:
	$(RM) -rf $(CASK_PACKAGE_DIR)

.PHONY: cask_install test clean
