.PHONY: test compile clean all bench gc-bench actor-bench soak soak-1h soak-full soak-worker \
        sqlite-module sqlite-module-clean \
        release-artifact release-checksum soak-blocker soak-post-ship \
        bench-actual bench-allocator bench-allocator-heavy \
        stage-d-tarball standalone-tarball standalone-tarball-verify \
        verify-elisp-fixtures \
        standalone-eval standalone-eval-clean standalone-eval-test standalone-eval-j \
        standalone-reader standalone-reader-test standalone-reader-prelude-test standalone-selfhost-test standalone-selfhost-mt-test standalone-parallel-compile-test

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
PACKAGE_SRC_DIRS := $(sort $(wildcard packages/*/src))
PACKAGE_TEST_DIRS := $(sort $(wildcard packages/*/test))
PACKAGE_SRCS := $(sort $(wildcard packages/*/src/nelisp*.el))
# Soak test (Phase 5-D.6) is advisory only and deliberately excluded
# from the gated TESTS glob — it runs long-lived `sleep-for' jobs and
# is invoked explicitly via `make soak'.
TESTS := $(sort $(filter-out test/nelisp-worker-soak-test.el, \
                  $(wildcard test/nelisp*-test.el) \
                  $(wildcard packages/*/test/nelisp*-test.el)))
TEST_LOADS := $(addprefix -l ,$(TESTS))
PACKAGE_SRC_LOADS := $(addprefix -L ,$(PACKAGE_SRC_DIRS))
PACKAGE_TEST_LOADS := $(addprefix -L ,$(PACKAGE_TEST_DIRS))

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
	$(EMACS) --batch -Q -L lisp -L src -L test -L bench \
	  $(PACKAGE_SRC_LOADS) \
	  $(PACKAGE_TEST_LOADS) \
	  --eval '(setq load-prefer-newer t)' \
	  -l ert \
	  $(TEST_LOADS) \
	  -f ert-run-tests-batch-and-exit

compile:
	$(EMACS) --batch -Q -L src \
	  $(PACKAGE_SRC_LOADS) \
	  --eval '(setq byte-compile-error-on-warn t)' \
	  -f batch-byte-compile $(SRCS) $(PACKAGE_SRCS)

clean:
	find . -name '*.elc' -type f -delete

# ===================================================================
# Standalone NeLisp eval binary (pure-elisp AOT, ZERO Rust).
# The REAL evaluator (nl_eval_inner + combiner cons/apply + bootstrap
# mirror) is compiled by the Phase 47 elisp compiler into relocatable
# units and linked by the pure-elisp static linker into a freestanding
# static ELF.  No cargo / rustc / target binary involved.
#
#   make standalone-eval         # whole, INCREMENTAL build -> target/nelisp-standalone-eval
#   make standalone-eval-test    # build, run, assert (+ 1 2) -> exit 3
#   make standalone-eval-clean   # drop the per-unit object cache
#
# Individual .el rebuild: editing one lisp/nelisp-cc-XXX.el invalidates
# ONLY that unit's cache (target/standalone-units/NAME.unit); the next
# `make standalone-eval' recompiles just that unit + relinks.
# Force one unit:  emacs ... --eval '(nelisp-standalone-rebuild-one "eq-symbol.o")'
# Parametrize the embedded form: NELISP_FORM_OP={+,-,*} NELISP_FORM_A=N NELISP_FORM_B=M.
standalone-eval:
	$(EMACS) --batch -Q -L lisp -L src -L scripts \
	  --eval '(setq load-prefer-newer t)' \
	  -l nelisp-standalone-build -f nelisp-standalone-build

standalone-eval-test:
	$(EMACS) --batch -Q -L lisp -L src -L scripts \
	  --eval '(setq load-prefer-newer t)' \
	  -l nelisp-standalone-build -f nelisp-standalone-test

standalone-eval-clean:
	$(EMACS) --batch -Q -L lisp -L src -L scripts \
	  --eval '(setq load-prefer-newer t)' \
	  -l nelisp-standalone-build -f nelisp-standalone-clean

# Reader path (Doc 137 M1): text -> AOT reader -> eval, ZERO Rust.
#   make standalone-reader        # build -> target/nelisp
#   make standalone-reader-test   # build, run, assert exit == eval(NELISP_SRC)
# Embedded source via NELISP_SRC (default "(+ 40 2)" -> 42; + - * only for now).
standalone-reader:
	$(EMACS) --batch -Q -L lisp -L src -L scripts \
	  --eval '(setq load-prefer-newer t)' \
	  -l nelisp-standalone-build -f nelisp-standalone-build-reader

standalone-reader-test:
	$(EMACS) --batch -Q -L lisp -L src -L scripts \
	  --eval '(setq load-prefer-newer t)' \
	  -l nelisp-standalone-build -f nelisp-standalone-reader-test

# Prelude-load breadth test (Wave-1 (A)+(B)).  Builds the reader binary, then
# runs it on  scripts/nelisp-stdlib-prelude.el  followed by a breadth test that
# exercises cond / dolist / nth / plist-get / backquote (all backed by the
# Wave-1 (B) breadth primitives), asserting exit == 42.  The prelude is just a
# loadable .el: the binary loads it then user code.  To use it by hand:
#   cat scripts/nelisp-stdlib-prelude.el yourfile.el > /tmp/prog.el
#   target/nelisp /tmp/prog.el   # exit = last form's value
standalone-reader-prelude-test:
	$(EMACS) --batch -Q -L lisp -L src -L scripts \
	  --eval '(setq load-prefer-newer t)' \
	  -l nelisp-standalone-build -f nelisp-standalone-reader-prelude-test

# Zero-Rust standalone reader distribution.  Builds a short `bin/nelisp`
# (`bin/nelisp.exe` for windows-x86_64) tarball for the requested platform.
#   make standalone-tarball PLATFORM=linux-x86_64
#   make standalone-tarball PLATFORM=macos-aarch64
#   make standalone-tarball-verify PLATFORM=linux-x86_64
STANDALONE_VERSION ?= stage-d-v3.0
standalone-tarball:
	@./tools/build-standalone-tarball.sh $(STANDALONE_VERSION) $(PLATFORM) --emacs "$(EMACS)"

standalone-tarball-verify:
	@./tools/verify-standalone-tarball.sh $(STANDALONE_VERSION) $(PLATFORM)
# Stage 3 SELF-HOST test: the standalone interpreter loads its OWN compiler
# toolchain as source and compiles a recursive program (fact) to a native
# x86_64 ELF with ZERO emacs, then we exec it and assert exit 120 (= 5!).
standalone-selfhost-test:
	./tools/selfhost-test.sh

# Stage 4 SELF-HOST MULTI-THREADED test: the standalone interpreter compiles a
# clone(2)+atomics multi-threaded program to native code with ZERO emacs; the
# binary spawns 3 OS threads that produce partial results in parallel, joined
# via a shared SeqCst atomic counter -> exit 42.  Proves NeLisp's multi-threaded
# parallel build capability.
standalone-selfhost-mt-test:
	./tools/selfhost-mt-test.sh

# Stage 4 PRODUCTION PARALLEL BUILD: the standalone interpreter compiles N units
# CONCURRENTLY (fork(2) workers, each running the full Phase-47 compiler, COW-
# isolated so no shared-state race), joined via a MAP_SHARED atomic counter.
standalone-parallel-compile-test:
	./tools/parallel-compile-test.sh


# Multi-process parallel compile (startup-bound for the current unit set:
# usually SLOWER than serial `standalone-eval' -- see the script header).
# JOBS defaults to nproc.
standalone-eval-j:
	@JOBS=$(JOBS) ./tools/build-standalone-parallel.sh $(JOBS)

# Doc 126 (2026-05-18): the `bake-images'/`bake-check' `lisp/*.image'
# boot path was retired -- the interpreter loads `.el' sources directly
# via read + eval, so no on-disk `.image' artifacts exist.

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

# Phase 7.2 §5.1 v2 LOCK-close — 3-tier ratio bench (Doc 29).
# Tier-A gates on the low end of the §5.1 v2 bands (3-5x / 4-6x /
# 8-12x).  Until Phase 7.5 wires the alloc fast path the harness
# reports `simulator-only' for every tier — gate-pass evaluates to
# :skipped (= exit code 0, never blocks) so the harness ships green
# from day one and flips to "gate verification" with no code change
# the moment Phase 7.5 lands.  See bench/nelisp-allocator-bench.el
# commentary for the const-unfoldable construction notes.
#
# Deliberately does NOT depend on `compile' — the bench reports its
# numbers off the source `.el' (matching `bench' / `gc-bench' /
# `actor-bench' all-source intent: the bench itself is a hot path,
# not the SUT, so compile-once-per-target is the right tradeoff).
bench-allocator:
	$(EMACS) --batch -Q -L src -L bench \
	  --eval '(setq load-prefer-newer t)' \
	  -l nelisp-allocator-bench \
	  -f nelisp-allocator-bench-batch

# Heavy variant: cons-stress 1M / per-pool 100k / bulk-alloc 1000
# (= the full Doc 29 §5.1 v2 input sizes).  Gated under
# NELISP_HEAVY_TESTS=1 per the project's existing convention so the
# default `bench-allocator' target stays CI-friendly (= ~30 s wall
# under simulator-only mode).  Once Phase 7.5 wires the native fast
# path, this is the gate verification target.
bench-allocator-heavy:
	$(EMACS) --batch -Q -L src -L bench \
	  --eval '(setq load-prefer-newer t)' \
	  --eval '(setq nelisp-allocator-bench-cons-stress-n 1000000)' \
	  --eval '(setq nelisp-allocator-bench-per-pool-n 100000)' \
	  --eval '(setq nelisp-allocator-bench-bulk-alloc-n 1000)' \
	  -l nelisp-allocator-bench \
	  -f nelisp-allocator-bench-batch

# Phase 7.5.3 (Doc 32 v2 §2.7 + §7).  blocker = CI 1h、
# post-ship = release-audit 24h.  Both wrap `tools/soak-test.sh` with
# the right SOAK_DURATION_HOURS env so the threshold logic stays
# co-located with the harness.
soak:
	@./test/nelisp-soak-test.sh

soak-1h:
	@./test/nelisp-soak-test.sh --1h-soak

soak-full:
	@./test/nelisp-soak-test.sh --full-24h

# Phase 5-D.6 worker soak.  Advisory only — not gated.  Exercises the
# 3-lane worker pool under sustained mixed load (20 read + 5 write +
# 1 long-running batch) and proves no cross-lane starvation.
soak-worker:
	$(EMACS) --batch -Q -L src -L test \
	  --eval '(setq load-prefer-newer t)' \
	  -l ert \
	  -l test/nelisp-worker-soak-test.el \
	  -f ert-run-tests-batch-and-exit

soak-blocker:
	@SOAK_DURATION_HOURS=1 ./tools/soak-test.sh

soak-post-ship:
	@SOAK_DURATION_HOURS=24 ./tools/soak-test.sh

# Phase 7.1 完遂 gate 3-axis bench actual measurement (Doc 28 v2 §5.2).
# Runs `bench/nelisp-cc-bench-actual.el' end-to-end and exits with
# code 0 when all three §5.2 gates PASS (fib(30) 30x / fact-iter 20x
# / alloc-heavy 5x speedup vs bytecode VM), 1 otherwise.
bench-actual:
	$(EMACS) --batch -Q -L src -L bench \
	  --eval '(setq load-prefer-newer t)' \
	  -l nelisp-cc-bench-actual \
	  -f nelisp-cc-bench-actual-run-3-axis

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
# tarball ships without anvil-lib/ and bin/anvil exits early at install
# time with a clear "anvil.el required" error (legacy nelisp-server
# fallback was removed once architecture α stabilised).
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
