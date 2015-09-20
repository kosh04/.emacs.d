EMACS ?= emacs
EMACSFLAGS = -L site-lisp
EMACS_BATCH = $(EMACS) -batch -no-site-file $(EMACSFLAGS)

CASK ?= cask
CASK_PACKAGE_DIR := $(shell $(CASK) package-directory)

COMPILE.el = $(EMACS_BATCH) -f batch-byte-compile

SRC.el := $(filter-out %-test.el, $(wildcard site-lisp/*.el))

TESTS := $(basename $(notdir $(wildcard site-lisp/*-test.el)))


default: compile test-startup test-lisp

compile: $(SRC.el:.el=.elc)

%.elc: $(CASK_PACKAGE_DIR)
%.elc: %.el
	$(CASK) exec $(COMPILE.el) $<


test-startup: $(CASK_PACKAGE_DIR)
	$(CASK) exec $(EMACS_BATCH) -l test-startup.el

test-lisp: $(TESTS)

# usage: $(call define-test,$(TEST_NAME))
define define-test
.PHONY: $1
$1: site-lisp/$1.el
	$$(CASK) exec $$(EMACS_BATCH) -l $$^ -f ert-run-tests-batch-and-exit
endef

$(foreach test, $(TESTS), $(eval $(call define-test,$(test))))


$(CASK_PACKAGE_DIR): Cask
	$(CASK) install

UnicodeData.txt:
	curl -Os  "http://www.unicode.org/Public/UNIDATA/UnicodeData.txt"

clean:
	$(RM) $(SRC.el:.el=.elc)

.PHONY: compile clean
.PHONY: test test-startup test-lisp
