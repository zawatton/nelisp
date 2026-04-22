.PHONY: test compile clean all

EMACS ?= emacs

# make on MSYS strips TEMP/TMP from the environment, which makes
# `make-temp-file' in the subprocess fall back to `c:/' (unwritable)
# and fail every test that touches a temp file.  Force sane defaults.
export TMPDIR ?= /tmp
export TEMP   ?= /tmp
export TMP    ?= /tmp

all: compile test

test:
	$(EMACS) --batch -Q -L src -L test \
	  --eval '(setq load-prefer-newer t)' \
	  -l ert \
	  -l test/nelisp-test.el \
	  -f ert-run-tests-batch-and-exit

compile:
	$(EMACS) --batch -Q -L src \
	  --eval '(setq byte-compile-error-on-warn t)' \
	  -f batch-byte-compile src/nelisp.el

clean:
	find . -name '*.elc' -type f -delete
