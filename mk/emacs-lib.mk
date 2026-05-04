## nelisp-emacs Makefile

EMACS = emacs --batch
SRC_FILES = $(wildcard src/*.el)
TEST_FILES = $(wildcard test/*.el)

VENDOR_NELISP = vendor/nelisp
NELISP_BIN    = $(VENDOR_NELISP)/target/release/nelisp

.PHONY: compile test clean nelisp nelisp-rebuild nelisp-clean help

help:
	@echo "Targets:"
	@echo "  make compile         byte-compile src/*.el"
	@echo "  make test            run ERT under host emacs"
	@echo "  make nelisp          fetch + build the nelisp runtime into"
	@echo "                       vendor/nelisp/ (idempotent)"
	@echo "  make nelisp-rebuild  cargo clean + rebuild"
	@echo "  make nelisp-clean    remove vendor/nelisp/ entirely"
	@echo "  make clean           remove .elc files"

compile:
	$(EMACS) -L src \
		-f batch-byte-compile $(SRC_FILES)

test:
	$(EMACS) -L src -L test \
		$(foreach t,$(TEST_FILES),-l $(t)) \
		-f ert-run-tests-batch-and-exit

# Layer-2 self-containment (Doc 51, 2026-05-04): the nelisp runtime
# is fetched + built into vendor/nelisp/ by `bin/build-nelisp'.
# `make nelisp' is the canonical entry point — it is idempotent
# (= clones once, then pulls + cargo build on subsequent runs).
nelisp:
	bin/build-nelisp

nelisp-rebuild:
	bin/build-nelisp --rebuild

nelisp-clean:
	rm -rf $(VENDOR_NELISP)

clean:
	find . -name "*.elc" -delete
