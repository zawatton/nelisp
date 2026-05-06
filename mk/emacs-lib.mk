## nelisp-emacs Makefile

EMACS = emacs --batch
NELISP_ROOT = $(HOME)/Notes/dev/nelisp
NELISP_LOAD_PATH = -L $(NELISP_ROOT)/src \
	$(foreach d,$(wildcard $(NELISP_ROOT)/packages/*/src),-L $(d))
SRC_FILES = $(wildcard src/*.el)
TEST_FILES = $(wildcard test/*.el)

.PHONY: compile test bench clean

compile:
	$(EMACS) -L src $(NELISP_LOAD_PATH) \
		-f batch-byte-compile $(SRC_FILES)

test:
	$(EMACS) -L src -L test $(NELISP_LOAD_PATH) \
		$(foreach t,$(TEST_FILES),-l $(t)) \
		-f ert-run-tests-batch-and-exit

bench:
	$(EMACS) -L src -L bench $(NELISP_LOAD_PATH) \
		-l bench-redisplay.el \
		-f bench-redisplay-run-all

clean:
	find . -name "*.elc" -delete
