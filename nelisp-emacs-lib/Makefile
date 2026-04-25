## nelisp-emacs Makefile

EMACS = emacs --batch
SRC_FILES = $(wildcard src/*.el)
TEST_FILES = $(wildcard test/*.el)

.PHONY: compile test clean

compile:
	$(EMACS) -L src -L $(HOME)/Notes/dev/nelisp/src \
		-f batch-byte-compile $(SRC_FILES)

test:
	$(EMACS) -L src -L test -L $(HOME)/Notes/dev/nelisp/src \
		$(foreach t,$(TEST_FILES),-l $(t)) \
		-f ert-run-tests-batch-and-exit

clean:
	find . -name "*.elc" -delete
