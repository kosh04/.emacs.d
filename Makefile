USAGE = Usage: $(notdir $(MAKE)) [compile|test|update|clean|help]

EMACS ?= emacs
EMACSFLAGS = -L site-lisp
EMACS_BATCH = $(EMACS) -batch -no-site-file

COMPILE.el = $(EMACS_BATCH) $(EMACSFLAGS) -f batch-byte-compile

SRCS := $(filter-out %-test.el, $(wildcard site-lisp/*.el))

default: compile test

compile: EMACSFLAGS += -f package-initialize
compile: $(SRCS:.el=.elc)

%.elc: %.el
	$(COMPILE.el) $<

test-startup:
	$(EMACS_BATCH) $(EMACSFLAGS) -l test-startup.el

test: test-lisp

test-lisp: $(basename $(notdir $(wildcard site-lisp/*-test.el)))

%-test: site-lisp/%-test.el
	$(EMACS_BATCH) $(EMACSFLAGS) -l $^ -f ert-run-tests-batch-and-exit

update: update-package

update-package:
	$(EMACS) --script script/package-update.el

UnicodeData.txt:
	curl -Os  "http://www.unicode.org/Public/UNIDATA/UnicodeData.txt"

clean:
	$(RM) $(SRCS:.el=.elc)

help usage:
	$(info $(USAGE))

.PHONY: compile clean
.PHONY: help usage
.PHONY: test test-startup test-lisp
.PHONY: update update-package
