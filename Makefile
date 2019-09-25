USAGE = Usage: $(notdir $(MAKE)) [compile|test|update|clean|help]

EMACS ?= emacs
EMACSFLAGS = -l .user-dir.el

COMPILE.el = $(EMACS) -batch $(EMACSFLAGS) -f batch-byte-compile

SRCS := $(filter-out %-test.el, $(wildcard site-lisp/*.el))

default: compile test

compile: EMACSFLAGS += -f package-initialize
compile: $(SRCS:.el=.elc)

%.elc: %.el
	$(COMPILE.el) $<

test-startup:
	$(EMACS) -batch $(EMACSFLAGS) -l test-startup.el

test: test-lisp
test-all: test-lisp test-startup

test-lisp: $(basename $(notdir $(wildcard site-lisp/*-test.el)))

%-test: site-lisp/%-test.el
	$(EMACS) -batch -L site-lisp $(EMACSFLAGS) -l $^ -f ert-run-tests-batch-and-exit

package-install:
	$(EMACS) -batch $(EMACSFLAGS) -l script/package-install.el

update: update-package

update-package:
	$(EMACS) $(EMACSFLAGS) --script script/package-update.el

etc/UnicodeData.txt:
	curl "http://www.unicode.org/Public/UNIDATA/UnicodeData.txt" -o $@

clean:
	$(RM) $(SRCS:.el=.elc)

help usage:
	$(info $(USAGE))

.PHONY: compile clean
.PHONY: help usage
.PHONY: test test-all test-startup test-lisp
.PHONY: update update-package
