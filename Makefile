.PHONY: test compile clean all bench gc-bench actor-bench soak smoke stage-d-tarball \
        runtime runtime-test runtime-clean test-runtime \
        runtime-staticlib stage-d-v2-bin \
        release-artifact release-checksum soak-blocker soak-post-ship

EMACS ?= emacs

# make on MSYS strips TEMP/TMP from the environment, which makes
# `make-temp-file' in the subprocess fall back to `c:/' (unwritable)
# and fail every test that touches a temp file.  Force sane defaults.
export TMPDIR ?= /tmp
export TEMP   ?= /tmp
export TMP    ?= /tmp

# Sorted so `nelisp-read.el' is compiled before `nelisp.el' (the latter
# requires the former at byte-compile time).  Glob pattern matches both
# `nelisp.el' and `nelisp-FOO.el'.
SRCS  := $(sort $(wildcard src/nelisp*.el))
# Soak test (Phase 5-D.6) is advisory only and deliberately excluded
# from the gated TESTS glob — it runs long-lived `sleep-for' jobs and
# is invoked explicitly via `make soak'.
TESTS := $(sort $(filter-out test/nelisp-worker-soak-test.el, \
                  $(wildcard test/nelisp*-test.el)))
TEST_LOADS := $(addprefix -l ,$(TESTS))

# `all' deliberately runs only the test target — the self-host
# probes (`test/nelisp-self-host-test.el') evaluate `nelisp-eval.el'
# *through NeLisp itself*, and the extra host stack frames introduced
# by byte-compiled `nelisp-eval.el' trip `max-lisp-eval-depth' inside
# `nelisp--install-core-macros' (see the inline comment in
# `src/nelisp-eval.el:542').  Keep `compile' as a separate byte-
# compile-error-on-warn lint check that never contaminates the test
# environment with stale or depth-sensitive .elc files.
all: test

test: clean
	$(EMACS) --batch -Q -L src -L test -L bench \
	  --eval '(setq load-prefer-newer t)' \
	  -l ert \
	  $(TEST_LOADS) \
	  -f ert-run-tests-batch-and-exit

compile:
	$(EMACS) --batch -Q -L src \
	  --eval '(setq byte-compile-error-on-warn t)' \
	  -f batch-byte-compile $(SRCS)

clean:
	find . -name '*.elc' -type f -delete

# Phase 7+ replan-gate audit scanner (T14 nelisp-dev-audit).
# Optional NELISP_AUDIT_WEEK env to inject current development week (e.g., 4 / 8 / 12).
# Exit code 0 = all pass / pending、1 = any gate fires.
audit:
	$(EMACS) --batch -Q -L src \
	  --eval '(setq load-prefer-newer t)' \
	  -l nelisp-dev-audit \
	  -f nelisp-dev-audit-batch

# Phase 3b.7 perf bench.  Always runs against compiled .elc — the
# uncompiled VM is ~17x slower than the byte-compiled one, so
# anything else would lie about the steady-state numbers.
bench: compile
	$(EMACS) --batch -Q -L src -L bench \
	  --eval '(setq load-prefer-newer t)' \
	  -l nelisp-bench \
	  -f nelisp-bench-batch

# Phase 3c.6 GC mark-pass bench.  Advisory only — not gated.
gc-bench: compile
	$(EMACS) --batch -Q -L src -L bench \
	  --eval '(setq load-prefer-newer t)' \
	  -l nelisp-gc-bench \
	  -f nelisp-gc-bench-batch

# Phase 4.7 actor runtime bench.  Advisory only — not gated.
actor-bench: compile
	$(EMACS) --batch -Q -L src -L bench \
	  --eval '(setq load-prefer-newer t)' \
	  -l nelisp-actor-bench \
	  -f nelisp-actor-bench-batch

# Phase 5-D.6 worker soak.  Advisory only — not gated.  Exercises the
# 3-lane worker pool under sustained mixed load (20 read + 5 write +
# 1 long-running batch) and proves no cross-lane starvation.
soak:
	$(EMACS) --batch -Q -L src -L test \
	  --eval '(setq load-prefer-newer t)' \
	  -l ert \
	  -l test/nelisp-worker-soak-test.el \
	  -f ert-run-tests-batch-and-exit

# Phase 5-E.4 MCP stdio smoke.  Runs the `nelisp-anvil' MCP server
# in a subprocess and pipes a scripted dialog through it, asserting
# on the JSON-RPC reply shape.  Proves the end-to-end stdio path
# (emacs --batch + -l nelisp-server + -l nelisp-tools + -f
# nelisp-server-run-stdio) is wired correctly for Claude Code.
smoke:
	./test/nelisp-server-smoke.sh

# Phase 7.0 (Doc 27 §3 7.0) Rust syscall stub.  `nelisp-runtime/` is a
# self-contained Cargo crate (cdylib + bin) that ships ~10 OS syscall
# thin wrappers under the `nelisp_syscall_*` C ABI prefix.  Phase 7.5
# wires NeLisp's FFI to those symbols; Phase 7.0 only proves the
# binary builds, links, and runs `--syscall-smoke' green.
NELISP_RUNTIME_DIR := nelisp-runtime
NELISP_RUNTIME_BIN := $(NELISP_RUNTIME_DIR)/target/release/nelisp-runtime
# Pick the first cargo on $PATH, fall back to a rustup default install
# path so a non-login shell (e.g. `make' invoked from a daemon) still
# finds the toolchain without forcing the user to source `~/.cargo/env'.
CARGO ?= $(shell command -v cargo 2>/dev/null || echo $(HOME)/.cargo/bin/cargo)

runtime:
	cd $(NELISP_RUNTIME_DIR) && $(CARGO) build --release

runtime-test:
	cd $(NELISP_RUNTIME_DIR) && $(CARGO) test --release

runtime-clean:
	cd $(NELISP_RUNTIME_DIR) && $(CARGO) clean

# `test-runtime' depends on `runtime' so a fresh checkout that runs
# only this target still proves the ERT + cargo + binary chain.  The
# ERT layer (`test/nelisp-runtime-test.el') skips cleanly when the
# binary is missing, so plain `make test' stays green for hosts
# without a Rust toolchain.
test-runtime: runtime
	$(EMACS) --batch -Q -L src -L test \
	  --eval '(setq load-prefer-newer t)' \
	  -l ert \
	  -l test/nelisp-runtime-test.el \
	  -f ert-run-tests-batch-and-exit

# Phase 7.5.1 (Doc 32 v2 LOCKED §3.1) — staticlib build.  Cargo.toml
# declares cdylib + rlib + staticlib in parallel, so a single
# `cargo build --release' produces all three artifacts; this target
# exists primarily so the Makefile names the staticlib path and the
# Phase 7.5.2 `stage-d-v2-bin' rule has a clean dependency edge.
NELISP_RUNTIME_STATICLIB := $(NELISP_RUNTIME_DIR)/target/release/libnelisp_runtime.a

runtime-staticlib:
	cd $(NELISP_RUNTIME_DIR) && $(CARGO) build --release
	@if [ -f "$(NELISP_RUNTIME_STATICLIB)" ]; then \
	    printf "  \033[1;32m✓\033[0m staticlib built: %s ($$(du -h "$(NELISP_RUNTIME_STATICLIB)" | cut -f1))\n" "$(NELISP_RUNTIME_STATICLIB)"; \
	else \
	    printf "  \033[1;33m!\033[0m staticlib NOT produced — check Cargo.toml crate-type contains \"staticlib\"\n"; \
	    exit 1; \
	fi

# Phase 7.5.1 (Doc 32 v2 LOCKED §3.1) — stage-d-v2.0 candidate binary
# scaffold.  This target reserves the build edge for Phase 7.5.2 where
# the real cold-init embed lands; for now it just proves the staticlib
# is reachable from the bin/anvil launcher path.  The real link step
# (bin/anvil rewrite + libnelisp_runtime.a static link) is intentional
# scope-out of Phase 7.5.1 partial — see Doc 32 v2 §3.2.
stage-d-v2-bin: runtime-staticlib
	@echo "stage-d-v2-bin (Phase 7.5.1 partial scaffold)"
	@echo "  staticlib    : $(NELISP_RUNTIME_STATICLIB)"
	@echo "  bin/anvil    : $$(pwd)/bin/anvil (--strict-no-emacs scaffold only)"
	@echo "  next phase   : 7.5.2 — real cold-init + 4-stage bootstrap embed"
	@echo "  Doc 32 v2 §3.2 で real bin/anvil + libnelisp_runtime.a static link 完成予定"

# Phase 6.3 (Stage D, Doc 18) distribution tarball.  Bundles only what
# `bin/anvil mcp serve' needs at runtime — bin/, src/*.el, README,
# LICENSE, install.sh — under a versioned prefix so `tar -xzf
# --strip-components=1' lands cleanly on the install target.
#
#   make stage-d-tarball                 → dist/anvil-stage-d-vDEV.tar.gz
#   make stage-d-tarball ANVIL_VERSION=stage-d-v0.1
#                                        → dist/anvil-stage-d-v0.1.tar.gz
ANVIL_VERSION ?= stage-d-vDEV
STAGE_D_NAME  := anvil-$(ANVIL_VERSION)
STAGE_D_DIR   := dist/$(STAGE_D_NAME)
STAGE_D_TAR   := dist/$(STAGE_D_NAME).tar.gz

# Phase 6.1 architecture α: bundle anvil.el for the architecture α
# delegate chain (anvil-XXX → nelisp-XXX via fboundp guard + fallback).
# ANVIL_EL_SOURCE points at an anvil.el checkout.  Missing / empty =>
# tarball ships without anvil-lib/ and bin/anvil falls back to
# nelisp-server (Phase 6.0 baseline).
ANVIL_EL_SOURCE ?= $(HOME)/Notes/dev/anvil.el

stage-d-tarball:
	@rm -rf "$(STAGE_D_DIR)"
	@mkdir -p "$(STAGE_D_DIR)/bin" "$(STAGE_D_DIR)/src"
	cp bin/anvil       "$(STAGE_D_DIR)/bin/"
	cp $(SRCS)         "$(STAGE_D_DIR)/src/"
	cp LICENSE         "$(STAGE_D_DIR)/" 2>/dev/null || true
	cp README-stage-d.org "$(STAGE_D_DIR)/README.org"
	cp install.sh      "$(STAGE_D_DIR)/" 2>/dev/null || true
	@printf "%s\n" "$(ANVIL_VERSION)" > "$(STAGE_D_DIR)/VERSION"
	@if [ -f "$(ANVIL_EL_SOURCE)/anvil.el" ] && \
	    [ -f "$(ANVIL_EL_SOURCE)/anvil-server-commands.el" ]; then \
	    mkdir -p "$(STAGE_D_DIR)/anvil-lib"; \
	    cp "$(ANVIL_EL_SOURCE)"/anvil*.el "$(STAGE_D_DIR)/anvil-lib/"; \
	    [ -f "$(ANVIL_EL_SOURCE)/LICENSE" ] && \
	        cp "$(ANVIL_EL_SOURCE)/LICENSE" \
	           "$(STAGE_D_DIR)/anvil-lib/LICENSE-anvil" || true; \
	    printf "  architecture α active — anvil.el bundled from %s (%d files)\n" \
	        "$(ANVIL_EL_SOURCE)" "$$(ls $(STAGE_D_DIR)/anvil-lib/anvil*.el | wc -l)"; \
	else \
	    printf "  architecture α INACTIVE — set ANVIL_EL_SOURCE=<path> to bundle anvil.el\n"; \
	fi
	tar -czf "$(STAGE_D_TAR)" -C dist "$(STAGE_D_NAME)"
	@rm -rf "$(STAGE_D_DIR)"
	@printf "  \033[1;32m✓\033[0m built %s ($$(du -h "$(STAGE_D_TAR)" | cut -f1))\n" "$(STAGE_D_TAR)"

# Phase 7.5.3 (Doc 32 v2 LOCKED §3.3) — stage-d-v2.0 release artifact.
# Wraps `tools/build-release-artifact.sh` so callers can drive the
# release pipeline through the same `make' surface as the rest of the
# build.  PLATFORM defaults to linux-x86_64 (the §11 blocker tier);
# RELEASE_VERSION defaults to stage-d-v2.0.
PLATFORM        ?= linux-x86_64
RELEASE_VERSION ?= stage-d-v2.0

release-artifact:
	@./tools/build-release-artifact.sh $(PLATFORM) $(RELEASE_VERSION)

release-checksum:
	@cd dist && \
	  if command -v sha256sum >/dev/null 2>&1; then \
	    sha256sum --check $(RELEASE_VERSION)-$(PLATFORM).tar.gz.sha256; \
	  else \
	    shasum -a 256 --check $(RELEASE_VERSION)-$(PLATFORM).tar.gz.sha256; \
	  fi

# Phase 7.5.3 soak gates (Doc 32 v2 §2.7 + §7).  blocker = CI 1h、
# post-ship = release-audit 24h.  Both wrap `tools/soak-test.sh` with
# the right SOAK_DURATION_HOURS env so the threshold logic stays
# co-located with the harness.
soak-blocker:
	@SOAK_DURATION_HOURS=1 ./tools/soak-test.sh

soak-post-ship:
	@SOAK_DURATION_HOURS=24 ./tools/soak-test.sh
