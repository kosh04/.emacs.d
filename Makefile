# todo: use NTEmacs+Cask

USAGE := $(MAKE) [compile|test|update|clean|help]

EMACS ?= emacs
EMACSFLAGS = -L site-lisp
EMACS_BATCH = $(EMACS) -batch -no-site-file

CASK ?= cask
CASK_PACKAGE_DIR := $(shell $(CASK) package-directory)

COMPILE.el = $(EMACS_BATCH) $(EMACSFLAGS) -f batch-byte-compile

SRC.el := $(filter-out %-test.el, $(wildcard site-lisp/*.el))


default: compile test

compile: $(SRC.el:.el=.elc)

%.elc: $(CASK_PACKAGE_DIR)
%.elc: %.el
	$(CASK) exec $(COMPILE.el) $<


test-startup: $(CASK_PACKAGE_DIR)
	$(CASK) exec $(EMACS_BATCH) $(EMACSFLAGS) -l test-startup.el

test: test-lisp

test-lisp: $(basename $(notdir $(wildcard site-lisp/*-test.el)))

%-test: site-lisp/%-test.el
	$(CASK) exec $(EMACS_BATCH) $(EMACSFLAGS) -l $^ -f ert-run-tests-batch-and-exit

$(CASK_PACKAGE_DIR): Cask
	$(CASK) install

update: update-package

update-package:
	$(EMACS) --script script/update-package.el

UnicodeData.txt:
	curl -Os  "http://www.unicode.org/Public/UNIDATA/UnicodeData.txt"

clean:
	$(RM) $(SRC.el:.el=.elc)

help usage:
	$(info $(USAGE))

.PHONY: compile clean
.PHONY: help usage
.PHONY: test test-startup test-lisp
.PHONY: update update-package
