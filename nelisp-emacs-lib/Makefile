## nelisp-emacs Makefile

EMACS = emacs --batch
# Layer-2 self-containment (Doc 51, 2026-05-04): the nelisp runtime
# normally lives at vendor/nelisp/ (populated by `bin/build-nelisp').
# Fall back to ~/Notes/dev/nelisp if the vendored copy is absent so
# legacy worktrees keep building.
VENDOR_NELISP = vendor/nelisp
NELISP_BIN    = $(VENDOR_NELISP)/target/release/nelisp
NELISP_ROOT  ?= $(if $(wildcard $(VENDOR_NELISP)/src),$(VENDOR_NELISP),$(HOME)/Notes/dev/nelisp)
NELISP_LOAD_PATH = -L $(NELISP_ROOT)/src \
	$(foreach d,$(wildcard $(NELISP_ROOT)/packages/*/src),-L $(d))
SRC_FILES = $(wildcard src/*.el)
TEST_FILES = $(wildcard test/*.el)

.PHONY: compile test bench demo demo-phase2 clean nelisp nelisp-rebuild nelisp-clean help

help:
	@echo "Targets:"
	@echo "  make compile         byte-compile src/*.el"
	@echo "  make test            run ERT under host emacs"
	@echo "  make bench           run redisplay benchmark"
	@echo "  make demo            run Phase 1 close demo"
	@echo "  make demo-phase2     run Phase 2 close demo"
	@echo "  make nelisp          fetch + build the nelisp runtime into"
	@echo "                       vendor/nelisp/ (idempotent)"
	@echo "  make nelisp-rebuild  cargo clean + rebuild"
	@echo "  make nelisp-clean    remove vendor/nelisp/ entirely"
	@echo "  make clean           remove .elc files"

compile:
	$(EMACS) -L src $(NELISP_LOAD_PATH) \
		-f batch-byte-compile $(SRC_FILES)

test:
	$(EMACS) -L src -L test -L demo $(NELISP_LOAD_PATH) \
		$(foreach t,$(TEST_FILES),-l $(t)) \
		-f ert-run-tests-batch-and-exit

bench:
	$(EMACS) -L src -L bench $(NELISP_LOAD_PATH) \
		-l bench-redisplay.el \
		-f bench-redisplay-run-all

demo:
	$(EMACS) -L src -L demo $(NELISP_LOAD_PATH) \
		-l phase1-close-demo \
		--eval "(prin1 (phase1-close-demo-run))" \
		--eval "(terpri)"

demo-phase2:
	$(EMACS) -L src -L demo $(NELISP_LOAD_PATH) \
		-l phase2-close-demo \
		--eval "(prin1 (phase2-close-demo-run))" \
		--eval "(terpri)"

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
