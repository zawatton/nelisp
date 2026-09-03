.PHONY: version-consistency actor-bench all aot-differential bench bench-aot-checked-arith bench-aot-tco clean compile gc-bench jit-unverified neln-loader-test nl-check-gate nl-dev-loop nl-safe-bench nl-safe-native-bench nl-violation-corpus ns-gate ns-inventory parens-check soak soak-1h soak-full soak-worker standalone-reader-recursion-guard-smoke test test-fast test-jit test-nojit test-one test-parallel unsafe-inventory wasm-dtw-compile wasm-dtw-site wasm-dtw-site-smoke wasm-dtw-skeleton-smoke wasm-dtw-smoke wasm-dtw-transpile wasm-runtime-image-smoke wasm-smoke \
        sqlite-module sqlite-module-clean \
        release-artifact release-checksum soak-blocker soak-post-ship \
        bench-actual bench-allocator bench-allocator-heavy \
        stage-d-tarball stage-d-v2-tarball stage-d-v2-tarball-verify \
        standalone-tarball standalone-tarball-verify \
        verify-elisp-fixtures \
        standalone-eval standalone-eval-clean standalone-eval-test standalone-eval-j \
        standalone-reader standalone-reader-test standalone-reader-load-smoke standalone-reader-checked standalone-reader-fmt-smoke standalone-reader-prelude-equal-reload-smoke standalone-reader-declare-strip-smoke standalone-reader-nested-backquote-macro-smoke standalone-reader-derived-mode-shape-smoke standalone-reader-pcase-quote-literal-smoke standalone-reader-catch-throw-tag-smoke standalone-reader-cond-let-shape-smoke standalone-reader-ffi-smoke standalone-reader-tls-smoke standalone-reader-tls-smoke-linux standalone-reader-tls-smoke-windows standalone-reader-process-smoke standalone-reader-realrt-smoke standalone-reader-repl-smoke standalone-reader-prelude-test standalone-reader-intern-soft-smoke standalone-reader-intern-soft-loop-smoke standalone-reader-number-token-smoke standalone-reader-getenv-smoke standalone-selfhost-test standalone-selfhost-mt-test standalone-parallel-compile-test standalone-chunk-growth-test \
        standalone-reader-mod-float-smoke standalone-reader-match-data-smoke standalone-reader-current-time-smoke standalone-reader-require-provide-smoke \
        alloc-check-collect standalone-reader-checked-soak standalone-reader-shadow-smoke standalone-reader-elt-smoke \
        nelisp-performance-gate nelisp-nelix-command-gate nelisp-native-artifact-gate nelisp-nelix-native-hot-gate \
        nelisp-nelix-operational-gate \
        nelisp-runtime-image-cache-gate nelisp-source-command-substrate-gate \
        nl-condition-standalone-smoke nl-safe-standalone-smoke nl-resource-standalone-smoke \
        nl-ns-reader-standalone-smoke \
        standalone-reader-buffer-smoke \
        nl-actor-standalone-smoke nelisp-actor-cps-baseline nelisp-actor-cps-parity \
        nl-clj-standalone-smoke nl-clj-async-standalone-smoke nl-clj-async-cps-baseline \
        nl-clj-future-standalone-smoke \
        nl-num-standalone-smoke nelisp-thread-standalone-smoke \
        nelisp-thread-allocating-standalone-smoke \
        nelisp-thread-mirror-guard-standalone-smoke \
        nelisp-thread-percpu-roots-smoke

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
PACKAGE_SRCS := $(sort $(wildcard packages/*/src/nelisp*.el) \
                       $(wildcard packages/*/src/nl-*.el))
# Soak test (Phase 5-D.6) is advisory only and deliberately excluded
# from the gated TESTS glob — it runs long-lived `sleep-for' jobs and
# is invoked explicitly via `make soak'.
TESTS := $(sort $(filter-out test/nelisp-worker-soak-test.el, \
                  $(wildcard test/nelisp*-test.el) \
                  $(wildcard packages/*/test/nelisp*-test.el) \
                  $(wildcard packages/*/test/nl-*-test.el)))
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

# Fast TDD loop: same test load graph as `test' but skips `clean';
# use `test' as the clean verification gate before trusting results.
# EMACS_PRELOAD carries the extra `-l FILE' arguments the JIT variants
# below need.  They used to pass them by overriding EMACS itself, but
# make exports command-line variables into every recipe subprocess, so
# a multi-word EMACS leaked into the environment of the tests' OWN
# child processes -- packages/nelisp-sys/bin/nelisp-sys reads
# "${EMACS}" and invokes it as a single word, which cannot execute.
# That is what turned the macOS JIT lane red while the plain suite
# passed.  Keep EMACS a program name; put flags here.
test-fast:
	$(EMACS) $(EMACS_PRELOAD) --batch -Q -L lisp -L src -L test -L bench \
	  $(PACKAGE_SRC_LOADS) \
	  $(PACKAGE_TEST_LOADS) \
	  --eval '(setq load-prefer-newer t)' \
	  -l ert \
	  $(TEST_LOADS) \
	  -f ert-run-tests-batch-and-exit

# Super-parallel host ERT runner.  Same file set + `-L' load paths as
# `test', but the ~160 test files are sharded (balanced by size) across
# JOBS worker `emacs --batch' processes and run concurrently, then the
# per-shard ert summaries are aggregated into one verdict.  The total
# `Ran N tests' count is invariant vs serial `test' — that is the
# runner's correctness gate.  On a 32-core box this turns a multi-minute
# serial suite into a sub-30s loop.
#   make test-parallel              # JOBS = nproc
#   make test-parallel JOBS=8
test-parallel:
	@JOBS=$(JOBS) EMACS="$(EMACS)" ./tools/run-tests-parallel.sh

# Focused single-file loop: run just one (or a few) test file(s) with the
# full `-L' load paths, no `clean', for tight TDD iteration.
#   make test-one FILE=test/nelisp-artifact-test.el
#   make test-one FILE="test/nelisp-artifact-test.el test/nelisp-core-fileio-test.el"
test-one:
	@test -n "$(FILE)" || { echo 'usage: make test-one FILE=test/nelisp-FOO-test.el'; exit 2; }
	$(EMACS) --batch -Q -L lisp -L src -L test -L bench \
	  $(PACKAGE_SRC_LOADS) \
	  $(PACKAGE_TEST_LOADS) \
	  --eval '(setq load-prefer-newer t)' \
	  -l ert \
	  $(addprefix -l ,$(FILE)) \
	  -f ert-run-tests-batch-and-exit

# Automated development loop.  Detects touched AOT/loader files (or accepts
# FILES explicitly), selects the focused gates, records output, and writes a
# diagnostic handoff on either success or failure.
#   make nl-dev-loop FILES="lisp/nelisp-aot-compiler.el lisp/nelisp-native-load.el"
nl-dev-loop:
	@./tools/nl-dev-loop.sh $(if $(FILES),--files "$(FILES)")

wasm-smoke:
	mkdir -p target/wasm-smoke
	HOME="$(CURDIR)" XDG_CONFIG_HOME="$(CURDIR)" $(EMACS) --batch -Q -L lisp -L src \
	  --eval '(setq load-prefer-newer t)' \
	  --eval "(progn \
	    (require 'nelisp-aot-compiler) \
	    (nelisp-aot-compile-to-object \
	     '(defun f () (+ (* 6 7) 0)) \
	     \"target/wasm-smoke/f.wasm\" \
	     :arch 'wasm32 :format 'wasm) \
	    (nelisp-aot-compile-to-object \
	     '(seq \
	       (defun g (y) (+ y 1)) \
	       (defun f () (let ((x (+ (g 3) 4))) (g x)))) \
	     \"target/wasm-smoke/f-locals.wasm\" \
	     :arch 'wasm32 :format 'wasm) \
	    (nelisp-aot-compile-to-object \
	     '(defun arg-budget-sum8 (a b c d e f g h) \
	        (extern-call nelisp_aot_wasm_arg_budget_sum8 \
	                     a b c d e f g h)) \
	     \"target/wasm-smoke/arg-budget-sum8.wasm\" \
	     :arch 'wasm32 :format 'wasm) \
	    (nelisp-aot-compile-to-object \
	     '(defun symbol-lit-probe (dest) \
	        (seq \
	         (sexp-write-symbol-lit dest \"eq\") \
	         (+ (* 40000000 (ptr-read-u8 dest 0)) \
	            (+ (* 100000 (- (ptr-read-u64 dest 16) dest)) \
	               (+ (* 10000 (ptr-read-u64 dest 24)) \
	                  (+ (* 100 (ptr-read-u8 dest 32)) \
	                     (ptr-read-u8 dest 33))))))) \
	     \"target/wasm-smoke/symbol-lit-probe.wasm\" \
	     :arch 'wasm32 :format 'wasm))"
	node tools/wasm-driver.mjs target/wasm-smoke/f.wasm f 42
	node tools/wasm-driver.mjs target/wasm-smoke/f-locals.wasm f 9
	# Doc 192 §3 Phase A/B: an 8-GP-argument `extern-call' -- one more
	# argument than `nelisp_aot_builtin_calln''s own six-argument fixed
	# ABI prefix takes before a single user argument, Doc 192 §1.2's
	# measured wall -- routed through a wasm `env' import and run under
	# Node.  Before the Phase A fix to `--current-arg-regs'
	# (`lisp/nelisp-aot-compiler.el'), the compile step above signals
	# `:extern-call-too-many-gp-args' for this defun specifically; see
	# `tools/wasm-arg-budget-env.mjs' for the full defect-class citation.
	node tools/wasm-driver.mjs --env-module tools/wasm-arg-budget-env.mjs \
	  target/wasm-smoke/arg-budget-sum8.wasm arg-budget-sum8 36 \
	  1 2 3 4 5 6 7 8
	# Doc 192 §3 Phase B: `sexp-write-symbol-lit' materializes a literal
	# symbol into DEST's frame slot.  Before this phase's wasm boundary-
	# slot fix, the compile step above signals `(:wasm-unsupported-ir
	# sexp-write-symbol-lit)' verbatim (red-first-verified by reverting
	# just the `--wasm-emit-value' tag-90/91 arms and the matching
	# `nelisp-aot-compiler--wasm-emit-sexp-write-lit' helper in
	# `lisp/nelisp-aot-compiler.el', then re-running `make wasm-smoke',
	# which fails at the compile step, `Error 255', before Node ever
	# runs -- the same discipline the arg-budget-sum8 pair above uses).
	# After the fix: DEST self-allocates a 32-byte header + inline
	# "eq" payload in linear memory (no `env' import -- see the tag-90/91
	# comment at that helper for why none is needed) and the checksum
	# below packs tag(4)*4e7 + (char-buf-ptr - dest)(32)*1e5 +
	# len(2)*1e4 + byte0('e'=101)*100 + byte1('q'=113) = 163230213,
	# verifying the header, the inline-payload offset convention, and
	# both literal bytes in one return value.
	node tools/wasm-driver.mjs \
	  target/wasm-smoke/symbol-lit-probe.wasm symbol-lit-probe 163230213 0
	@echo "GATE-COUNT checked=4 findings=0"

wasm-runtime-image-smoke:
	mkdir -p target/wasm-runtime-image
	HOME="$(CURDIR)" XDG_CONFIG_HOME="$(CURDIR)" $(EMACS) --batch -Q -L lisp -L src \
	  --eval '(setq load-prefer-newer t)' \
	  --eval "(progn \
	    (require 'nelisp-artifact) \
	    (compile-runtime-image \
	     '(\"compile-runtime-image\" \"--kind\" \"wasm\" \
	       \"--target\" \"wasm32-wasi\" \
	       \"--input\" \"tools/wasm-runtime-image-p3c.nlri\" \
	       \"--output\" \"target/wasm-runtime-image/runtime-image.wasm\")))"
	@test -s target/wasm-runtime-image/runtime-image.wasm || { \
	  echo "wasm-runtime-image-smoke: the compile step wrote no .wasm."; \
	  echo "  Its message is above; it exits 0 either way, so without this"; \
	  echo "  check the failure arrives as an ENOENT from node opening a"; \
	  echo "  file nobody wrote."; \
	  echo "GATE-COUNT checked=2 findings=1"; \
	  exit 1; }
	@if node tools/wasm-driver.mjs target/wasm-runtime-image/runtime-image.wasm _start 3; then \
	  echo "GATE-COUNT checked=2 findings=0"; \
	else \
	  echo "GATE-COUNT checked=2 findings=1"; exit 1; \
	fi

wasm-dtw-skeleton-smoke:
	@if node tools/wasm-proofs/p4-run-all.mjs; then \
	  echo "GATE-COUNT checked=1 findings=0"; \
	else \
	  echo "GATE-COUNT checked=1 findings=1"; exit 1; \
	fi

# The DTW slice transpiles a game-state file that lives in a SEPARATE checkout
# (newDTW-nelisp), whose default path in transpile-slice.mjs is a Windows one.
# Absent, this used to die inside node with an ENOENT -- a gate that cannot run
# reported as a gate that failed.  DTW_GAME_ROOT points it at a checkout;
# without one the chain says so and skips, which `verify' accepts for a
# required gate.
DTW_GAME_ROOT ?= $(CURDIR)/../newDTW-nelisp

wasm-dtw-transpile:
	@if [ ! -f "$(DTW_GAME_ROOT)/nelisp_runtime/gamedata-state-dungeon.el" ]; then \
	  echo "GATE-SKIP newDTW-nelisp checkout absent (looked for $(DTW_GAME_ROOT))"; \
	  echo "[wasm-dtw] SKIP: no game data to transpile"; \
	  exit 0; \
	fi; \
	mkdir -p target/wasm-dtw && \
	node tools/wasm-dtw-p4b/transpile-slice.mjs "$(DTW_GAME_ROOT)"

wasm-dtw-compile: wasm-dtw-transpile
	HOME="$(CURDIR)" XDG_CONFIG_HOME="$(CURDIR)" $(EMACS) --batch -Q -L lisp -L src \
	  --eval '(setq load-prefer-newer t)' \
	  --eval "(progn \
	    (require 'nelisp-artifact) \
	    (compile-runtime-image \
	     '(\"compile-runtime-image\" \"--kind\" \"wasm\" \
	       \"--target\" \"wasm32-wasi\" \
	       \"--input\" \"target/wasm-dtw/dtw-p4b.nlri\" \
	       \"--output\" \"target/wasm-dtw/dtw.wasm\")))"

wasm-dtw-smoke: wasm-dtw-compile
	@if [ ! -s target/wasm-dtw/dtw.wasm ]; then \
	  echo "GATE-SKIP no dtw.wasm (the transpile step skipped: see above)"; \
	  exit 0; \
	fi; \
	if node tools/wasm-dtw-p4b/smoke.mjs target/wasm-dtw/dtw.wasm; then \
	  echo "GATE-COUNT checked=1 findings=0"; \
	else \
	  echo "GATE-COUNT checked=1 findings=1"; exit 1; \
	fi

wasm-dtw-site: wasm-dtw-compile
	@if [ ! -s target/wasm-dtw/dtw.wasm ]; then \
	  echo "[wasm-dtw] SKIP: no dtw.wasm to build a site from"; \
	  exit 0; \
	fi; \
	node tools/wasm-dtw-p4b/build-site.mjs

wasm-dtw-site-smoke: wasm-dtw-site
	@if [ ! -d site/dtw ]; then \
	  echo "GATE-SKIP no site/dtw (the transpile step skipped: see above)"; \
	  exit 0; \
	fi; \
	if node tools/wasm-dtw-p4b/site-smoke.mjs site/dtw; then \
	  echo "GATE-COUNT checked=1 findings=0"; \
	else \
	  echo "GATE-COUNT checked=1 findings=1"; exit 1; \
	fi

# nl-check owns the expansion-time checks (`nl-must-use', resource
# tracking).  Until now nothing ran them as a gate: `unsafe-inventory'
# counted one of the five kinds and the other four were reported by no
# target at all.  They belong here, where a compile-time error belongs.
#
# It runs as a separate reading pass rather than inside the byte-compiler
# on purpose.  Doc 170 section 9 requires that with checking disabled the
# expansion stay byte-identical to the plain version; a pass that only
# reads cannot affect the emitted code, so that property holds by
# construction instead of by test.
nl-check-gate:
	$(EMACS) --batch -Q -L packages/nl-prelude/src -L packages/nl-safe/src \
	  -L packages/nl-check/src -l scripts/nl-check-gate.el

# Doc 170 section 9 sets budgets -- 15% for a borrow, 20% for a fat
# pointer access -- and bench/nl-safe-bench.el measures them.  Nothing
# ran it: no make target, no CI.  So the budgets were written, the
# measurement was written, and the result went unread.  It is 11.66x,
# 15.93x and 3.63x against those budgets, which is why nl-safe has no
# users outside its own tests, which is why there is no violation data,
# which is why the Doc 170 section 8 gate cannot be answered.
#
# This reports; it does not gate.  Gating on a budget nothing meets
# would just be a red build nobody can act on until the overhead comes
# down, and the number is the thing worth watching meanwhile.
nl-safe-bench:
	$(EMACS) --batch -Q --eval '(setq load-prefer-newer t)' \
	  -L lisp -L src -L bench \
	  -L packages/nl-prelude/src -L packages/nl-safe/src \
	  -l bench/nl-safe-bench.el -f nl-safe-bench-run

# Doc 170 section 9 on the path the budget belongs to.  `nl-safe-bench'
# measures on host Emacs, where the borrow SHAPE alone costs 2.69x before
# anything is checked, so it can only ever report "over budget".  This
# compiles both sides to .neln with the dynamic-user-call lowering (which
# is what closes their extern sets) and runs them through the in-process
# loader inside the reader.
nl-safe-native-bench: standalone-reader
	@mkdir -p target/nl-safe-native-bench
	NELISP_ARTIFACT_DIR=$(CURDIR)/target/nl-safe-native-bench \
	  $(EMACS) --batch -Q -L lisp -L src -L scripts \
	  --eval '(setq load-prefer-newer t)' \
	  -l nl-safe-native-bench-fixtures \
	  -f nl-safe-native-bench-fixtures-main
	@prelude=target/nl-safe-native-bench/prelude.el; \
	{ echo '(load "$(CURDIR)/lisp/nelisp-native-load.el")'; \
	  echo '(defvar nl-safe-native-bench-dir "$(CURDIR)/target/nl-safe-native-bench")'; \
	  echo '(load "$(CURDIR)/bench/nl-safe-native-bench.el")'; \
	  echo '(nl-safe-native-bench-run)'; \
	} > "$$prelude"; \
	./target/nelisp --load "$$prelude"

# Reports how many bodies reached the JIT, and how many of those carried
# a finding.  The JIT is the one place code arrives that no build step
# saw, so this is the measurement behind "nothing executes unverified".
# It does not gate; it prints.  Turning `count' on costs a walk of every
# body the JIT sees, which is why it is not the default policy.
jit-unverified:
	$(MAKE) test-fast \
	  EMACS_PRELOAD="-l scripts/nelisp-jit-unverified.el"

# Runs the suite with the JIT on.  `nelisp-jit-enabled' is nil by
# default and the only bindings of it are two of the JIT's own tests, so
# a whole translation path is exercised by the tests written for it and
# by nothing that uses it.  This puts every closure the suite builds
# through it.  Unlike `jit-unverified' it adds no per-body work -- that
# one is the measurement, this one is the coverage.
test-jit:
	$(MAKE) test-fast \
	  EMACS_PRELOAD="-l scripts/nelisp-jit-enable.el"

# The mirror.  With `nelisp-jit-enabled' defaulting to t, the ordinary
# suite exercises the JIT and the bcl / interpreter paths stop being
# covered unless something turns it back off.  Whichever way the default
# points, the other path needs a run of its own or it rots -- which is
# how the JIT reached a default-off state with three semantic
# differences from the interpreter still in it.
test-nojit:
	$(MAKE) test-fast \
	  EMACS_PRELOAD="-l scripts/nelisp-jit-disable.el"

compile: nl-check-gate
	$(EMACS) --batch -Q -L src \
	  $(PACKAGE_SRC_LOADS) \
	  --eval '(setq byte-compile-error-on-warn t)' \
	  -f batch-byte-compile $(SRCS) $(PACKAGE_SRCS)

clean:
	find . -name '*.elc' -type f -delete

# ===================================================================
# Standalone NeLisp eval binary (pure-elisp AOT, ZERO Rust).
# The REAL evaluator (nl_eval_inner + combiner cons/apply + bootstrap
# mirror) is compiled by the AOT elisp compiler into relocatable
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
# Standalone gates run a binary for the native host by default.  Override this
# with NELISP_STANDALONE_TARGET to build a different target; its run gate will
# report SKIP when that target is not executable on the host.
STANDALONE_GATE_TARGET ?= $(or $(NELISP_STANDALONE_TARGET),$(if $(filter Windows_NT,$(OS)),windows-x86_64,linux-x86_64))

# The standalone binary for the target the gates build and run.  ONE name,
# because there used to be three ways of spelling it and they disagreed:
# 10 of the 46 `STANDALONE_READER_SMOKES' picked `.exe' from a `case' on
# `NELISP_STANDALONE_TARGET', a handful of other gates picked "whichever
# file exists" (wrong when both do, which is normal on a machine that
# cross-builds), and the remaining 36 hardcoded `./target/nelisp' with no
# `.exe' case at all -- so the aggregate had never once run against a
# windows-native build, and said nothing about that.  Doc 201 §6.7.
STANDALONE_BIN = ./target/nelisp$(if $(filter windows%,$(STANDALONE_GATE_TARGET)),.exe,)

# `ulimit -v' bounds the ADDRESS SPACE so a runaway allocation fails loudly
# instead of taking the machine with it -- see the intern-soft-loop comment
# below for the regression that put it there.  It cannot be applied to the
# Win32 binary: that one RESERVES far more address space than it commits
# (VirtualAlloc), so a 4 GiB cap kills it before it has done anything, and
# `standalone-reader-bignum-smoke' read as a failing gate on windows-x86_64
# for that reason alone -- measured: the same probe passes with
# `BIGNUM-SMOKE cases=54 mismatches=0' when the cap is lifted, and is killed
# by `timeout' with empty output when it is not.  `timeout' still bounds
# every one of these either way, so a runaway is still caught on Windows;
# only the address-space cap is dropped.
STANDALONE_ULIMIT = $(if $(filter windows%,$(STANDALONE_GATE_TARGET)),:,ulimit -v 4194304)

# The `timeout' that bounds those same probes.  30s was chosen against the
# Linux binary; the Windows one is slower at the same work by enough that
# the bound became the thing being measured -- `scripts/standalone-bignum-
# smoke.el' takes 31.8s SOLO on windows-x86_64 (measured 2026-08-30), so it
# was killed with empty output and `standalone-reader-bignum-smoke' read as
# a failing gate on a probe that is entirely correct.  Under the 4-way
# `standalone-reader-smokes' fan-out it is slower still.  180s keeps the
# bound doing its actual job -- catching a run that will never finish --
# without deciding the verdict for a run that merely takes a while.
STANDALONE_SMOKE_TIMEOUT = $(if $(filter windows%,$(STANDALONE_GATE_TARGET)),180,30)
standalone-eval:
	NELISP_STANDALONE_TARGET=$(STANDALONE_GATE_TARGET) $(EMACS) --batch -Q -L lisp -L src -L scripts \
	  --eval '(setq load-prefer-newer t)' \
	  -l nelisp-standalone-build -f nelisp-standalone-build

standalone-eval-test:
	NELISP_STANDALONE_TARGET=$(STANDALONE_GATE_TARGET) $(EMACS) --batch -Q -L lisp -L src -L scripts \
	  --eval '(setq load-prefer-newer t)' \
	  -l nelisp-standalone-build -f nelisp-standalone-test

# macOS Mach-O acceptance (CI macos runner): emit arm64 MH_EXECUTE x2 +
# MH_OBJECT via the pure-elisp writers, then codesign/run/clang-link
# them on the Darwin side.  Emission is host-agnostic; the check
# script requires macOS (arm64) and fails the job on any regression.
macho-acceptance-emit:
	$(EMACS) --batch -Q -L lisp -L src -L scripts \
	  --eval '(setq load-prefer-newer t)' \
	  -l nelisp-macho-acceptance -f nelisp-macho-acceptance-emit

macho-acceptance-test: macho-acceptance-emit
	sh scripts/macho-acceptance-check.sh dist/macho-acceptance

standalone-eval-clean:
	$(EMACS) --batch -Q -L lisp -L src -L scripts \
	  --eval '(setq load-prefer-newer t)' \
	  -l nelisp-standalone-build -f nelisp-standalone-clean

# Reader path (Doc 137 M1): text -> AOT reader -> eval, ZERO Rust.
#   make standalone-reader        # build -> target/nelisp
#   make standalone-reader-test   # build, run, assert exit == eval(NELISP_SRC)
# Embedded source via NELISP_SRC (default "(+ 40 2)" -> 42; + - * only for now).
standalone-reader:
	NELISP_STANDALONE_TARGET=$(STANDALONE_GATE_TARGET) $(EMACS) --batch -Q -L lisp -L src -L scripts \
	  --eval '(setq load-prefer-newer t)' \
	  -l nelisp-standalone-build -f nelisp-standalone-build-reader

# Doc 169/170 language-extension standalone reality.  `nl-condition' and
# `nl-safe' both claim (README.org "Testing") to run unchanged on
# target/nelisp; `make test'/`ert-full' only proves the host-Emacs half
# of that.  These three run the exact ERT bodies (`packages/*/test/*
# -standalone-smoke.el', a mini `ert-deftest'/`should' shim over the
# same test files -- see nl-condition-standalone-smoke.el's Commentary)
# on the binary itself, each package's own examples/ demo included.
# Conditional prerequisite matches `binary-size-ratchet' above: build
# only if neither target/nelisp nor target/nelisp.exe exists yet.
# `[ -f "$$bin" ]' below only proves the build produced a file -- on a host
# that cannot run the binary's target (e.g. a linux-x86_64 target/nelisp
# under Windows), the file exists and this check alone would try to exec it
# anyway (2026-08-23 Windows inventory: `Exec format error').  The runnable-
# host predicate is asked FIRST, same convention as `standalone-reader-test'
# (scripts/nelisp-standalone-build.el).
nl-condition-standalone-smoke: $(if $(wildcard target/nelisp target/nelisp.exe),,standalone-reader)
	@NELISP_STANDALONE_TARGET=$(STANDALONE_GATE_TARGET) $(EMACS) --batch -Q -L lisp -L src -L scripts -l nelisp-standalone-build \
	  --eval '(kill-emacs (if (nelisp-standalone--target-runnable-on-host-p) 0 3))' \
	  >/dev/null 2>&1; \
	host_rc=$$?; \
	if [ "$$host_rc" = 3 ]; then \
	  echo "GATE-SKIP target $(STANDALONE_GATE_TARGET) cannot run on this host"; \
	  exit 0; \
	fi; \
	bin=$(STANDALONE_BIN); \
	if [ ! -f "$$bin" ]; then \
	  echo "GATE-SKIP no nelisp binary in target/ after build attempt"; \
	  exit 0; \
	fi; \
	"$$bin" --load packages/nl-condition/test/nl-condition-standalone-smoke.el

nl-safe-standalone-smoke: $(if $(wildcard target/nelisp target/nelisp.exe),,standalone-reader)
	@NELISP_STANDALONE_TARGET=$(STANDALONE_GATE_TARGET) $(EMACS) --batch -Q -L lisp -L src -L scripts -l nelisp-standalone-build \
	  --eval '(kill-emacs (if (nelisp-standalone--target-runnable-on-host-p) 0 3))' \
	  >/dev/null 2>&1; \
	host_rc=$$?; \
	if [ "$$host_rc" = 3 ]; then \
	  echo "GATE-SKIP target $(STANDALONE_GATE_TARGET) cannot run on this host"; \
	  exit 0; \
	fi; \
	bin=$(STANDALONE_BIN); \
	if [ ! -f "$$bin" ]; then \
	  echo "GATE-SKIP no nelisp binary in target/ after build attempt"; \
	  exit 0; \
	fi; \
	"$$bin" --load packages/nl-safe/test/nl-safe-standalone-smoke.el

# Doc 189 Phase 0 (reader-time namespace resolution): runs the exact
# ERT bodies of `packages/nl-ns/test/nl-ns-reader-test.el' on
# `target/nelisp' itself -- proving the `nelisp-read-namespace-resolve'
# hook `src/nelisp-read.el' gained is real on the compiled substrate,
# not only under the development host's own Emacs.  Same conditional-
# build / shim pattern as `nl-condition-standalone-smoke' above.
nl-ns-reader-standalone-smoke: $(if $(wildcard target/nelisp target/nelisp.exe),,standalone-reader)
	@bin=$(STANDALONE_BIN); \
	if [ ! -f "$$bin" ]; then \
	  echo "GATE-SKIP no nelisp binary in target/ after build attempt"; \
	  exit 0; \
	fi; \
	"$$bin" --load packages/nl-ns/test/nl-ns-reader-standalone-smoke.el

# Doc 198 Phases 2-3: the real standalone substrate runs the hygiene
# against-the-bug ERT bodies and a one-million-hop mutual trampoline.
# Declared beside the target to avoid expanding the conflict-prone global
# .PHONY list (the package-target convention documented near pkg-graph).
.PHONY: nl-hygiene-standalone-smoke
nl-hygiene-standalone-smoke: $(if $(wildcard target/nelisp target/nelisp.exe),,standalone-reader)
	@bin=$(STANDALONE_BIN); \
	if [ ! -f "$$bin" ]; then \
	  echo "GATE-SKIP no nelisp binary in target/ after build attempt"; \
	  exit 0; \
	fi; \
	"$$bin" --load packages/nl-hygiene/test/nl-hygiene-standalone-smoke.el

# Runnable-host guard: see `nl-condition-standalone-smoke' above.
nl-resource-standalone-smoke: $(if $(wildcard target/nelisp target/nelisp.exe),,standalone-reader)
	@NELISP_STANDALONE_TARGET=$(STANDALONE_GATE_TARGET) $(EMACS) --batch -Q -L lisp -L src -L scripts -l nelisp-standalone-build \
	  --eval '(kill-emacs (if (nelisp-standalone--target-runnable-on-host-p) 0 3))' \
	  >/dev/null 2>&1; \
	host_rc=$$?; \
	if [ "$$host_rc" = 3 ]; then \
	  echo "GATE-SKIP target $(STANDALONE_GATE_TARGET) cannot run on this host"; \
	  exit 0; \
	fi; \
	bin=$(STANDALONE_BIN); \
	if [ ! -f "$$bin" ]; then \
	  echo "GATE-SKIP no nelisp binary in target/ after build attempt"; \
	  exit 0; \
	fi; \
	"$$bin" --load packages/nl-safe/test/nl-resource-standalone-smoke.el

# Doc 188 P1 (buffer unification): runs
# test/nelisp-buffer-unification-standalone-smoke.el -- the same
# assertions as test/nelisp-buffer-unification-test.el, but against
# target/nelisp itself via scripts/nelisp-ert-shim.el, since a plain
# host-Emacs ERT run of those forms cannot distinguish this tree's own
# `insert'/`buffer-string' wiring from Emacs's (Doc 188 §1.8/§4.1).
# Same conditional-build pattern as the nl-condition/nl-safe smokes
# above.
# Runnable-host guard: see `nl-condition-standalone-smoke' above.
.PHONY: precise-root-coverage
# Precise root coverage for the mid-form collector.  Runs with the
# conservative native-stack scan off, which is the only configuration where a
# missing precise root arm is observable at all -- see the script's header and
# the `precise-root-coverage' rows in tools/gate-mutations.txt.
#
# The prerequisite is UNCONDITIONAL, matching standalone-midform-gc-bounded and
# unlike the `$(wildcard target/nelisp ...)' smokes: this gate's mutation row
# injects into scripts/nelisp-standalone-build.el, i.e. into the source the
# binary is generated from, so a target that reuses an existing target/nelisp
# tests the pre-injection build and reports GREEN with the defect in front of
# it.  That is exactly the UNREACHABLE failure mode tools/gate-mutations.txt
# warns about, and this target hit it on its first run.
precise-root-coverage: standalone-reader
	@NELISP_STANDALONE_TARGET=$(STANDALONE_GATE_TARGET) $(EMACS) --batch -Q -L lisp -L src -L scripts -l nelisp-standalone-build \
	  --eval '(kill-emacs (if (nelisp-standalone--target-runnable-on-host-p) 0 3))' \
	  >/dev/null 2>&1; \
	host_rc=$$?; \
	if [ "$$host_rc" = 3 ]; then \
	  echo "GATE-SKIP target $(STANDALONE_GATE_TARGET) cannot run on this host"; \
	  echo "GATE-COUNT checked=0 findings=0"; \
	  exit 0; \
	fi; \
	if [ "$$host_rc" != 0 ]; then \
	  echo "precise-root-coverage: target runnable predicate failed"; \
	  exit "$$host_rc"; \
	fi; \
	bash tools/nelisp-precise-root-gate.sh

standalone-reader-buffer-smoke: $(if $(wildcard target/nelisp target/nelisp.exe),,standalone-reader)
	@NELISP_STANDALONE_TARGET=$(STANDALONE_GATE_TARGET) $(EMACS) --batch -Q -L lisp -L src -L scripts -l nelisp-standalone-build \
	  --eval '(kill-emacs (if (nelisp-standalone--target-runnable-on-host-p) 0 3))' \
	  >/dev/null 2>&1; \
	host_rc=$$?; \
	if [ "$$host_rc" = 3 ]; then \
	  echo "GATE-SKIP target $(STANDALONE_GATE_TARGET) cannot run on this host"; \
	  exit 0; \
	fi; \
	bin=$(STANDALONE_BIN); \
	if [ ! -f "$$bin" ]; then \
	  echo "GATE-SKIP no nelisp binary in target/ after build attempt"; \
	  exit 0; \
	fi; \
	"$$bin" --load test/nelisp-buffer-unification-standalone-smoke.el

# Doc 193 §4.4 Phase 1: nelisp-actor's own standalone reality --
# `generator.el' is not vendored for the substrate, so nothing built
# from `(nelisp-actor-lambda ...)' directly can run here (see
# packages/nelisp-actor/README.org).  Unlike the three smokes above,
# this does NOT replay nelisp-actor-test.el's ERT bodies (those spawn
# actors via the macro and stay host-Emacs-only by design); it runs
# packages/nelisp-actor/generated/two-actor-exchange-cps.el -- the
# checked-in build-time CPS transform of the ping-pong demo
# (regenerate with `make nelisp-actor-cps-baseline') -- proving spawn/
# mailbox/send/receive/yield/run-until-idle all work on the binary
# itself.  Same conditional-build shape as the three above.
# Runnable-host guard: see `nl-condition-standalone-smoke' above.
nl-actor-standalone-smoke: $(if $(wildcard target/nelisp target/nelisp.exe),,standalone-reader)
	@NELISP_STANDALONE_TARGET=$(STANDALONE_GATE_TARGET) $(EMACS) --batch -Q -L lisp -L src -L scripts -l nelisp-standalone-build \
	  --eval '(kill-emacs (if (nelisp-standalone--target-runnable-on-host-p) 0 3))' \
	  >/dev/null 2>&1; \
	host_rc=$$?; \
	if [ "$$host_rc" = 3 ]; then \
	  echo "GATE-SKIP target $(STANDALONE_GATE_TARGET) cannot run on this host"; \
	  exit 0; \
	fi; \
	bin=$(STANDALONE_BIN); \
	if [ ! -f "$$bin" ]; then \
	  echo "GATE-SKIP no nelisp binary in target/ after build attempt"; \
	  exit 0; \
	fi; \
	"$$bin" --load packages/nelisp-actor/test/nelisp-actor-standalone-smoke.el

# Doc 195 (docs/design/195-clojure-compat-library.org) build-first Tier
# 1: runs the exact ERT bodies of every nl-clj-*-test.el on
# target/nelisp itself, plus a print/read round-trip check on a tagged
# persistent vector -- the specific substrate risk this package's own
# representation choice (nl-clj-core.el's Commentary) exists to avoid
# (Doc 195 §2.1: cl-defstruct/record print but do not round-trip on
# this substrate).  Same conditional-build shape as the smokes above.
nl-clj-standalone-smoke: $(if $(wildcard target/nelisp target/nelisp.exe),,standalone-reader)
	@bin=$(STANDALONE_BIN); \
	if [ ! -f "$$bin" ]; then \
	  echo "GATE-SKIP no nelisp binary in target/ after build attempt"; \
	  exit 0; \
	fi; \
	"$$bin" --load packages/nl-clj/test/nl-clj-standalone-smoke.el

# Doc 199 Tier 1: cooperative pmap/future/pcalls on target/nelisp.  Needs no
# generator (a future worker never parks), so it runs standalone directly.
.PHONY: nl-clj-future-standalone-smoke
nl-clj-future-standalone-smoke: $(if $(wildcard target/nelisp target/nelisp.exe),,standalone-reader)
	@bin=$(STANDALONE_BIN); \
	if [ ! -f "$$bin" ]; then \
	  echo "GATE-SKIP no nelisp binary in target/ after build attempt"; \
	  exit 0; \
	fi; \
	"$$bin" --load packages/nl-clj/test/nl-clj-future-standalone-smoke.el

# Doc 196 Phases 0-4: exact rational/complex reference-contract tests on the
# standalone, plus tagged-vector print/read round trips for a bignum-backed
# rational and a rational-component complex value.  Same explicit-load shim
# pattern as nl-clj-standalone-smoke above.
nl-num-standalone-smoke: $(if $(wildcard target/nelisp target/nelisp.exe),,standalone-reader)
	@NELISP_STANDALONE_TARGET=$(STANDALONE_GATE_TARGET) $(EMACS) --batch -Q -L lisp -L src -L scripts -l nelisp-standalone-build \
	  --eval '(kill-emacs (if (nelisp-standalone--target-runnable-on-host-p) 0 3))' \
	  >/dev/null 2>&1; \
	host_rc=$$?; \
	if [ "$$host_rc" = 3 ]; then \
	  echo "GATE-SKIP target $(STANDALONE_GATE_TARGET) cannot run on this host"; \
	  exit 0; \
	fi; \
	bin=$(STANDALONE_BIN); \
	if [ ! -f "$$bin" ]; then \
	  echo "GATE-SKIP no nelisp binary in target/ after build attempt"; \
	  exit 0; \
	fi; \
	"$$bin" --load packages/nl-num/test/nl-num-standalone-smoke.el

# Doc 199 Tier 2: interpreter-callable clone(2) with fixed GC-free native
# workers.  Mirrors nl-num-standalone-smoke's runnable-target guard, then loads
# the smoke directly in target/nelisp (never through the self-host compiler).
nelisp-thread-standalone-smoke: $(if $(wildcard target/nelisp target/nelisp.exe),,standalone-reader)
	@NELISP_STANDALONE_TARGET=$(STANDALONE_GATE_TARGET) $(EMACS) --batch -Q -L lisp -L src -L scripts -l nelisp-standalone-build \
	  --eval '(kill-emacs (if (nelisp-standalone--target-runnable-on-host-p) 0 3))' \
	  >/dev/null 2>&1; \
	host_rc=$$?; \
	if [ "$$host_rc" = 3 ]; then \
	  echo "GATE-SKIP target $(STANDALONE_GATE_TARGET) cannot run on this host"; \
	  exit 0; \
	fi; \
	bin=$(STANDALONE_BIN); \
	if [ ! -f "$$bin" ]; then \
	  echo "GATE-SKIP no nelisp binary in target/ after build attempt"; \
	  exit 0; \
	fi; \
	"$$bin" --load tools/nelisp-thread-standalone-smoke.el

# Doc 199 Tier 3a feasibility spike: bounded ordinary allocating Lisp on
# clone(2) workers.  Separate from Tier 2 so its GC-inhibit/private-env
# contract and mutation proof have an independent gate report.  Run once at
# the host's normal limit and again under the 4 GB virtual-memory ceiling that
# made the Stage 4c empty-chunk-unmap regression deterministic in CI.
nelisp-thread-allocating-standalone-smoke: $(if $(wildcard target/nelisp target/nelisp.exe),,standalone-reader)
	@NELISP_STANDALONE_TARGET=$(STANDALONE_GATE_TARGET) $(EMACS) --batch -Q -L lisp -L src -L scripts -l nelisp-standalone-build \
	  --eval '(kill-emacs (if (nelisp-standalone--target-runnable-on-host-p) 0 3))' \
	  >/dev/null 2>&1; \
	host_rc=$$?; \
	if [ "$$host_rc" = 3 ]; then \
	  echo "GATE-SKIP target $(STANDALONE_GATE_TARGET) cannot run on this host"; \
	  exit 0; \
	fi; \
	bin=$(STANDALONE_BIN); \
	if [ ! -f "$$bin" ]; then \
	  echo "GATE-SKIP no nelisp binary in target/ after build attempt"; \
	  exit 0; \
	fi; \
	"$$bin" --load tools/nelisp-thread-allocating-standalone-smoke.el && \
	for i in 1 2 3 4 5; do \
	  ( ulimit -v 4000000; "$$bin" --load tools/nelisp-thread-allocating-standalone-smoke.el ) \
	    || { echo "[nelisp-thread-allocating-standalone-smoke] FAIL under ulimit -v 4000000 (attempt $$i)"; exit 1; }; \
	done

# Doc 199 Tier 3a/3b enforced read-only global-state ceiling.  Registered
# workers may perform mirror lookups, but mirror/intern-table mutations must
# signal `nelisp-worker-mirror-mutation' and leave shared state unchanged.
nelisp-thread-mirror-guard-standalone-smoke: $(if $(wildcard target/nelisp target/nelisp.exe),,standalone-reader)
	@NELISP_STANDALONE_TARGET=$(STANDALONE_GATE_TARGET) $(EMACS) --batch -Q -L lisp -L src -L scripts -l nelisp-standalone-build \
	  --eval '(kill-emacs (if (nelisp-standalone--target-runnable-on-host-p) 0 3))' \
	  >/dev/null 2>&1; \
	host_rc=$$?; \
	if [ "$$host_rc" = 3 ]; then \
	  echo "GATE-SKIP target $(STANDALONE_GATE_TARGET) cannot run on this host"; \
	  exit 0; \
	fi; \
	bin=$(STANDALONE_BIN); \
	if [ ! -f "$$bin" ]; then \
	  echo "GATE-SKIP no nelisp binary in target/ after build attempt"; \
	  exit 0; \
	fi; \
	"$$bin" --load tools/nelisp-thread-mirror-guard-smoke.el

# Doc 199 Tier 3b first step: enumerate all allocating workers' private root
# reserves while three live-list frames are parked at a parent-controlled
# barrier.  Keep Tier 3a's runnable-target guard and memory-pressure repeat:
# Doc 152 sections 11.43.2-3 showed this defect class can be intermittent and
# invisible without the 4 GB virtual-memory ceiling.
nelisp-thread-percpu-roots-smoke: $(if $(wildcard target/nelisp target/nelisp.exe),,standalone-reader)
	@NELISP_STANDALONE_TARGET=$(STANDALONE_GATE_TARGET) $(EMACS) --batch -Q -L lisp -L src -L scripts -l nelisp-standalone-build \
	  --eval '(kill-emacs (if (nelisp-standalone--target-runnable-on-host-p) 0 3))' \
	  >/dev/null 2>&1; \
	host_rc=$$?; \
	if [ "$$host_rc" = 3 ]; then \
	  echo "GATE-SKIP target $(STANDALONE_GATE_TARGET) cannot run on this host"; \
	  exit 0; \
	fi; \
	bin=$(STANDALONE_BIN); \
	if [ ! -f "$$bin" ]; then \
	  echo "GATE-SKIP no nelisp binary in target/ after build attempt"; \
	  exit 0; \
	fi; \
	"$$bin" --load tools/nelisp-thread-percpu-roots-smoke.el && \
	for i in 1 2 3 4 5; do \
	  ( ulimit -v 4000000; "$$bin" --load tools/nelisp-thread-percpu-roots-smoke.el ) \
	    || { echo "[nelisp-thread-percpu-roots-smoke] FAIL under ulimit -v 4000000 (attempt $$i)"; exit 1; }; \
	done

# Doc 195 §4.6 (channels/go over nelisp-actor).  Same shim/load-by-path
# pattern as the smokes above, but loads packages/nl-clj/generated/
# go-ping-pong-cps.el -- the build-time CPS transform of minimal
# repeated-`<!'/`>!' fixtures plus a `nl-clj-go' ping/pong exchange
# (regenerate with `make nl-clj-async-cps-baseline')
# -- rather than replaying nl-clj-async-test.el's own ERT bodies, most
# of which spawn actors via `nl-clj-go' directly and stay host-Emacs-
# only by design (same reasoning as nl-actor-standalone-smoke above).
# The smoke makes two calls into the baked wrapper-based demo in one
# process and separately gates each wrapper's same-park-point second
# resumption; see the smoke file's Commentary for the resolved gap.
nl-clj-async-standalone-smoke: $(if $(wildcard target/nelisp target/nelisp.exe),,standalone-reader)
	@bin=$(STANDALONE_BIN); \
	if [ ! -f "$$bin" ]; then \
	  echo "GATE-SKIP no nelisp binary in target/ after build attempt"; \
	  exit 0; \
	fi; \
	"$$bin" --load packages/nl-clj/test/nl-clj-async-standalone-smoke.el

# Regenerates packages/nl-clj/generated/go-ping-pong-cps.el from
# nl-clj-async.el's own `nl-clj-async--make-chan-1'/`-blocking-take-1'/
# `-blocking-put-1' plus examples/nl-clj-async/go-ping-pong.el's two
# repeated-resumption fixtures and `nl-clj-async-demo-ping-pong', under
# THIS host's real Emacs + generator.el (AI.md rule 7).  Run this after
# editing any of those,
# then re-run `nl-clj-async-standalone-smoke' to confirm the
# regenerated file still runs correctly standalone before committing
# it -- host-vs-generated parity is NOT checked by this target itself
# (unlike `nelisp-actor-cps-parity' for the sibling package); compare
# `nl-clj-async-demo-ping-pong'/`-standalone' by hand (see
# packages/nl-clj/scripts/nl-clj-async-cps-dump.el's own Commentary).
nl-clj-async-cps-baseline:
	$(EMACS) --batch -Q -L src -L packages/nelisp-actor/src -L packages/nelisp-actor/scripts \
	  -L packages/nl-prelude/src -L packages/nl-safe/src -L packages/nl-clj/src \
	  -l packages/nelisp-actor/scripts/nelisp-actor-cps-dump.el \
	  -l packages/nl-clj/scripts/nl-clj-async-cps-dump.el \
	  --eval "(nl-clj-async-cps-dump-write)"

# Regenerates packages/nelisp-actor/generated/two-actor-exchange-cps.el
# from examples/nelisp-actor/two-actor-exchange.el's `nelisp-demo-ping-
# pong', under THIS host's real Emacs + generator.el (AI.md rule 7: the
# recipe lives next to the generator, not in a session transcript).  Run
# this after editing either that example or nelisp-actor.el's struct/
# macros, then re-run `nl-actor-standalone-smoke' and `nelisp-actor-cps-
# parity' to confirm the regenerated file still matches host-Emacs
# behavior before committing it.
nelisp-actor-cps-baseline:
	$(EMACS) --batch -Q -L src -L packages/nelisp-actor/src \
	  -l packages/nelisp-actor/scripts/nelisp-actor-cps-dump.el \
	  --eval "(nelisp-actor-cps-dump-write \
	            \"examples/nelisp-actor/two-actor-exchange.el\" \
	            'nelisp-demo-ping-pong \
	            \"packages/nelisp-actor/generated/two-actor-exchange-cps.el\" \
	            'nelisp-demo-ping-pong-standalone)"

# Doc 193 §8's host-Emacs half of the §4 verification design: the
# generated forms behave identically to what real generator.el would
# have produced (not just "it runs").  Also covered by plain `make
# test' (the file matches the `packages/*/test/nelisp*-test.el' glob);
# this target is the fast, standalone-file way to run just this check.
nelisp-actor-cps-parity:
	$(EMACS) --batch -Q -L lisp -L src -L test -L bench \
	  $(PACKAGE_SRC_LOADS) $(PACKAGE_TEST_LOADS) \
	  --eval '(setq load-prefer-newer t)' \
	  -l ert -l packages/nelisp-actor/test/nelisp-actor-cps-parity-test.el \
	  -f ert-run-tests-batch-and-exit

# fboundp-liar audit: every name in the reader's builtin fboundp list must
# have a dispatch arm (or be combiner-handled), so `fboundp' never lies the
# way `nelisp--syscall-readdir' did (2026-06-10).
# Doc 170 sec 4.3: unsafe-surface inventory.  Counts nl-check
# `unsafe-call' findings (unsafe primitives outside `nl-unsafe';
# quoted AOT-grammar data is not scanned) across lisp/ and scripts/,
# and fails when the total exceeds tools/unsafe-inventory-baseline.txt.
# Shrinking the surface?  Lower the baseline in the same commit.
unsafe-inventory:
	$(EMACS) --batch -Q -L packages/nl-prelude/src -L packages/nl-safe/src \
	  -L packages/nl-check/src -l tools/nl-check-inventory.el

# Doc 168 Phase 6 / Doc 170 Stage 5 gate data.  Runs the suites that
# drive the dynamic checks with violation logging on, appends the
# records to .nl-violations/corpus.log (git-ignored) and prints the
# tally.  The corpus comes from tests, so it validates the pipeline
# and the record shape rather than sampling real usage -- see the
# header of tools/nl-violation-corpus.el.
nl-violation-corpus:
	$(EMACS) --batch -Q -L packages/nl-prelude/src -L packages/nl-safe/src \
	  -L packages/nl-contract/src -L packages/nl-check/src \
	  -L packages/nelisp-json/src \
	  -L packages/nl-safe/test -L packages/nl-contract/test -L tools \
	  --eval '(setq load-prefer-newer t)' \
	  -l tools/nl-violation-corpus.el

# Bootstrap contract.  The standalone has several bootstraps, each
# assembling its own source, and nothing checked that they agree -- so a
# fact added to one was absent from the others and the same code answered
# differently per entry point.  Measured 2026-08-19 three times over
# (load-path, string-match-p, install-core-macros).  Bootstraps are
# discovered, not listed, so a new one is checked from the day it exists.
.PHONY: bootstrap-contract
bootstrap-contract:
	$(EMACS) --batch -Q -L lisp -L src -L scripts \
	  -l tools/nelisp-bootstrap-contract.el

# Silent-degradation inventory.  Counts error handlers that neither
# record nor re-raise, and fails when a kind exceeds
# tools/fallback-inventory-baseline.txt.  Same ratchet rule as above.
#
# Measured 2026-08-19: a native compile fell back to bytecode inside a
# `condition-case' and printed nothing, so a green compile said nothing
# about whether anything had been compiled natively.  This gate does not
# judge whether a fall is acceptable -- that is not mechanical -- it
# keeps the count from growing quietly.
# Which names would work on a stock Emacs.  Every name this tree defines is
# classified: `nelisp-only' (Emacs does not have it -- code using it does not
# run there), `shared-deferring' (both have it and this tree defers via
# `(unless (fboundp ...))'), or `shared-shadowing' (both have it and this
# tree defines it unconditionally, so its definition lands on top).
#
# The host answer is not a maintained list: the tool runs under `emacs -Q'
# and READS the sources rather than loading them, so `fboundp' in that same
# process IS stock Emacs's answer.
#
# Added 2026-08-19 because nobody -- developer or AI -- could tell which
# definition was in effect at a call site, and three defects fixed that day
# were exactly that.  The ratchet is on `shared-shadowing'; the full table is
# generated into docs/emacs-compat-table.txt so it can be grepped without
# running anything.
.PHONY: prelude-toplevel-check
.PHONY: generated-source-parse
.PHONY: doc200-census
.PHONY: partial-inventory
.PHONY: gate-mutation
.PHONY: gate-selfcheck
.PHONY: parity-coverage
.PHONY: parity-fuzz
.PHONY: inner
.PHONY: emacs-parity
.PHONY: binary-size-ratchet
.PHONY: emacs-compat
emacs-compat:
	$(EMACS) --batch -Q -l tools/nelisp-emacs-compat.el

.PHONY: emacs-compat-table
emacs-compat-table:
	NELISP_EMACS_COMPAT_WRITE=1 $(EMACS) --batch -Q -l tools/nelisp-emacs-compat.el

.PHONY: fallback-inventory
fallback-inventory:
	$(EMACS) --batch -Q -l tools/nelisp-fallback-inventory.el

# Does the classifier answer correctly?  Six handlers with known answers in
# tools/fallback-inventory-fixture/, which the scanner is pointed at instead
# of the tree.  Added 2026-08-19 with the first review of the aggregate,
# which had been wrong in both directions and unnoticed: 23 handlers that
# re-raise or print to stderr counted as silent, 18 that mention a logger
# writing only under a profiling flag counted as innocent.
.PHONY: fallback-inventory-selftest
fallback-inventory-selftest:
	@out="$$(NELISP_FALLBACK_INVENTORY_ROOTS=tools/fallback-inventory-fixture \
	         $(EMACS) --batch -Q -l tools/nelisp-fallback-inventory.el 2>&1)"; \
	echo "$$out" | grep -E '^(silent-fallback|ignore-errors|bare-handler|dbg-note)'; \
	ok=1; \
	echo "$$out" | grep -qE '^silent-fallback +3 ' || { echo "  expected silent-fallback 3"; ok=0; }; \
	echo "$$out" | grep -qE '^ignore-errors +1 ' || { echo "  expected ignore-errors 1"; ok=0; }; \
	echo "$$out" | grep -qE '^bare-handler +1 ' || { echo "  expected bare-handler 1"; ok=0; }; \
	echo "$$out" | grep -qE '^dbg-note +0 ' || { echo "  expected dbg-note 0"; ok=0; }; \
	if [ "$$ok" = 1 ]; then \
	  echo "GATE-COUNT checked=4 findings=0"; \
	  echo "[fallback-inventory-selftest] PASS"; \
	else \
	  echo "GATE-COUNT checked=4 findings=1"; \
	  echo "[fallback-inventory-selftest] FAIL"; exit 1; \
	fi

# Doc 169 defect #6: namespace boundaries, checked instead of enforced
# by a reader extension.  Counts nl-ns cross-file collisions, stray
# definitions, and references through another file's `--' private
# boundary, and fails when any kind exceeds
# tools/ns-inventory-baseline.txt.  Same ratchet rule as above.
ns-inventory:
	$(EMACS) --batch -Q -L packages/nl-prelude/src -L packages/nl-ns/src \
	  -l tools/nl-ns-inventory.el

# Derives the cross-package dependency graph from provide/require and
# fails on a cycle or an unreadable manifest.  Declared here rather than
# in the .PHONY block at the top of this file: that list has produced a
# merge conflict on every integration branch so far, and a one-line
# .PHONY beside its target costs nothing.
#
# Also scans src/ and lisp/, which the package scan does not reach, and
# reports what they require that nothing in the tree provides -- split into
# hard and optional, and ratcheted on the hard count against
# tools/pkg-host-requires-baseline.txt.  Measured 2026-08-19: every host
# library that has stopped the standalone runtime lived in those two
# directories and was invisible to this gate.  The last of them,
# `(require 'seq)' in one file, took the native compiler down with it and
# reported "native compiler unavailable"; finding it meant bisecting a
# require by hand.
.PHONY: pkg-graph
pkg-graph:
	$(EMACS) --batch -Q -L packages/nelisp-pkg/src \
	  -l tools/nelisp-pkg-graph.el

# Prints the files to load for PKG and its dependencies, in order, for
# the standalone runtime (which loads by path rather than through
# `require').  Use -s to get only the paths:
#   make -s pkg-load-order PKG=nl-safe
# Checks the hand-written load lists in the standalone smokes: a path
# that does not exist, or a file loaded before something it requires.
# Both are silent at run time -- `load' on a missing file returns t in
# the standalone runtime instead of signalling.
# Writes each package's manifest.el from the dependencies its code
# actually has.  This is the one-command fix for a drift finding from
# `make pkg-graph'; keys other than :name / :requires are preserved.
#   make pkg-manifest-update [PKG=<name>]
.PHONY: pkg-manifest-update
pkg-manifest-update:
	@$(EMACS) --batch -Q -L packages/nelisp-pkg/src \
	  -l tools/nelisp-pkg-manifest.el

.PHONY: pkg-load-lists
pkg-load-lists:
	$(EMACS) --batch -Q -L packages/nelisp-pkg/src \
	  -l tools/nelisp-pkg-load-lists.el

.PHONY: pkg-load-order
pkg-load-order:
	@$(EMACS) --batch -Q -L packages/nelisp-pkg/src \
	  -l tools/nelisp-pkg-load-order.el

# Every docs/design/*.org whose #+STATUS: line says SHIPPED must carry a
# #+VERIFIED-BY: line naming a gate that exists (tools/nelisp-doc-claims.el).
# Legacy SHIPPED docs predating the header are tolerated via
# tools/nelisp-doc-claims-baseline.txt, the same pinned-baseline shape
# unsafe-inventory/fallback-inventory/pkg-graph use.
.PHONY: doc-claims
doc-claims:
	$(EMACS) --batch -Q -l tools/nelisp-doc-claims.el

# Fails on a cross-file name collision that is not in the accepted set,
# and on an accepted entry that no longer matches anything.  Reaching
# zero findings is not the goal: the bootstrap prelude has to define
# `when' before the file defining it can be read.  Catching the NEXT one
# is the goal -- a stale evaluator sat in packages/nelisp-tramp/src for a
# month, and ten Doc 22 fixes lived only in the prelude, because nobody
# could see either inside a list this long.
ns-gate:
	$(EMACS) --batch -Q -L packages/nl-prelude/src -L packages/nl-condition/src \
	  -L packages/nl-ns/src -l scripts/nl-ns-gate.el

parens-check:
	$(EMACS) --batch -Q -L packages/nl-parens/src -l tools/nl-parens-check.el

reader-surface-audit:
	$(EMACS) --batch -Q -L lisp -L src -L scripts \
	  --eval '(setq load-prefer-newer t)' \
	  -l reader-surface-audit -f nelisp-reader-surface-audit

# ---- standalone-reader-smokes -------------------------------------------
#
# The individual reader smokes, run as one gate (28 -> 29 on
# integration/wave3: fix/standalone-reader-input-hardening added
# `standalone-reader-malformed-input-smoke'; 34 -> 35 on
# fix/ffi-surface-availability: added `standalone-reader-ffi-unsupported-
# smoke', which pins the DEFAULT static build's `nl-ffi-call' availability --
# see that target's own comment).  The "29" in the prose below has drifted
# from the list's actual length before this change too; unchanged here
# rather than reworked without re-verifying its own history.
#
# `standalone-reader-test' runs 19 checks built into the build script (13
# base + 1 initial exit-code assertion, +4 from an earlier integration: each
# of fix/control-flow-wrong-values (`...-control-flow-smoke'),
# fix/standalone-reader-input-hardening (`...-malformed-input-smoke'),
# fix/elc-artifact-void-invocation-name (`...-elc-smoke'), and
# feat/stdlib-hooks-map-fixnum (`...-stdlib-completion-smoke') added one to
# the dolist, +1 from Doc 186 (`...-char-table-smoke') -- measured with
# `GATE-COUNT checked=19 findings=0' on this branch.
# These 29 are separate make targets testing separate things -- format
# directives, getenv, TLS, processes, match-data, intern-soft, the FFI bridge
# -- and until 2026-08-21 nothing ran them, which is how a `print-circle'
# defect in the unit cache key and a wrong-signed `mod' both sat in the tree.
#
# One gate rather than 29 entries in gates.expected: 29 reports would demand 29
# freshness checks for one build's worth of evidence, and a ledger nobody can
# keep current is a ledger nobody reads.  The count is the signal -- 29 checked
# is the claim, and a target that stops existing shows up as a smaller number.
STANDALONE_READER_SMOKES = \
  standalone-reader-async-core-smoke \
  standalone-reader-bignum-smoke \
  standalone-reader-catch-throw-tag-smoke \
  standalone-reader-checked \
  standalone-reader-checked-soak \
  standalone-reader-cond-let-shape-smoke \
  standalone-reader-current-time-smoke \
  standalone-reader-defvar-alloc-smoke \
  standalone-reader-declare-strip-smoke \
  standalone-reader-derived-mode-shape-smoke \
  standalone-reader-dns-smoke \
  standalone-reader-elt-smoke \
  standalone-reader-ffi-smoke \
  standalone-reader-ffi-unsupported-smoke \
  standalone-reader-fileattrs-smoke \
  standalone-reader-fmt-smoke \
  standalone-reader-getenv-smoke \
  standalone-reader-hosts-file-smoke \
  standalone-reader-intern-soft-loop-smoke \
  standalone-reader-intern-soft-smoke \
  standalone-reader-load-smoke \
  standalone-reader-malformed-input-smoke \
  standalone-reader-match-data-smoke \
  standalone-reader-mod-float-smoke \
  standalone-reader-nested-backquote-macro-smoke \
  standalone-reader-network-process-nowait-smoke \
  standalone-reader-network-process-server-smoke \
  standalone-reader-network-process-smoke \
  standalone-reader-nonblocking-socket-smoke \
  standalone-reader-number-token-smoke \
  standalone-reader-pcase-quote-literal-smoke \
  standalone-reader-prelude-equal-reload-smoke \
  standalone-reader-prelude-test \
  standalone-reader-process-adapter-smoke \
  standalone-reader-process-adapter-smoke-red \
  standalone-reader-process-smoke \
  standalone-reader-reader-parity-smoke \
  standalone-reader-realrt-smoke \
  standalone-reader-regexp-lead-filter-smoke \
  standalone-reader-recursion-guard-smoke \
  standalone-reader-repl-idle-pump-smoke \
  standalone-reader-repl-smoke \
  standalone-reader-require-provide-smoke \
  standalone-reader-shadow-smoke \
  standalone-reader-splitstring-perf-smoke \
  standalone-reader-tls-smoke \
  standalone-reader-winpath-smoke

# Every one of the 34 sub-targets below ultimately execs the SAME
# target/nelisp binary, so if this host cannot run its target none of them
# can -- asked once here rather than 34 times.  2026-08-23 Windows
# inventory: without this, the aggregate ran all 34, 32 hit `Exec format
# error'/permission failures individually, and it reported a plain FAIL
# instead of a reasoned skip.  Same predicate/convention as
# `standalone-reader-test'.
#
# Per-smoke logs go under target/tmp/, not /tmp/: MSYS2 `make' and Git
# Bash `tail' disagreed about where `/tmp' lives in that same run (`make'
# wrote to C:\msys64\tmp, the shell's own `tail /tmp/...' looked
# elsewhere), so the aggregate could not read or remove its own logs.
# target/ is inside the repo checkout -- one namespace, not two.
.PHONY: standalone-reader-smokes
standalone-reader-smokes:
	@NELISP_STANDALONE_TARGET=$(STANDALONE_GATE_TARGET) $(EMACS) --batch -Q -L lisp -L src -L scripts -l nelisp-standalone-build \
	  --eval '(kill-emacs (if (nelisp-standalone--target-runnable-on-host-p) 0 3))' \
	  >/dev/null 2>&1; \
	host_rc=$$?; \
	if [ "$$host_rc" = 3 ]; then \
	  echo "GATE-SKIP target $(STANDALONE_GATE_TARGET) cannot run on this host"; \
	  echo "[reader-smokes] SKIP: target cannot run on this host"; \
	  exit 0; \
	fi; \
	MAKE="$(MAKE)" tools/nelisp-reader-smokes.sh \
	  --target "$(STANDALONE_GATE_TARGET)" $(STANDALONE_READER_SMOKES)

standalone-reader-test:
	$(EMACS) --batch -Q -L lisp -L src -L scripts \
	  --eval '(setq load-prefer-newer t)' \
	  -l nelisp-standalone-build -f nelisp-standalone-reader-test

nelisp-performance-gate:
	./tools/nelisp-performance-gate.sh

nelisp-nelix-command-gate:
	./tools/nelisp-nelix-command-gate.sh

nelisp-native-artifact-gate:
	./tools/nelisp-native-artifact-gate.sh

nelisp-nelix-native-hot-gate:
	./tools/nelisp-nelix-native-hot-gate.sh

nelisp-nelix-operational-gate: nelisp-nelix-command-gate nelisp-nelix-native-hot-gate

nelisp-runtime-image-cache-gate:
	./tools/nelisp-runtime-image-cache-gate.sh

nelisp-source-command-substrate-gate:
	./tools/nelisp-source-command-substrate-gate.sh

# One probe corpus (tools/nelisp-substrate-parity-corpus.el), every entry
# point (bare-file, --load, runtime-image, compiled artifact, source-cache,
# source-fallback, and host Emacs for the shared part), diffed line by line
# against the bare-file baseline.  See tools/nelisp-substrate-parity.el's
# header for why: this branch's costliest misdiagnosis was a primitive
# probed in one substrate and generalized to another.
.PHONY: substrate-parity-smoke
substrate-parity-smoke:
	./tools/nelisp-substrate-parity-smoke.sh

# Task A (presence sweep): the definable-name surface (~354 names --
# scripts/nelisp-stdlib-prelude.el's top-level functions union
# nelisp--primitive-symbols in src/nelisp-eval.el, see
# tools/nelisp-substrate-presence-gen.el), one `fboundp' probe per name,
# through the exact same corpus/diff/ledger machinery as
# substrate-parity-smoke above but as a second corpus and a second ledger
# (tools/substrate-presence-accepted.el) so the two finding counts never
# conflate.  Its own gate, not folded into `nelisp-ai.sh extras': ~2400
# process launches, measured too slow for that tier's budget.
.PHONY: substrate-presence-sweep
substrate-presence-sweep:
	./tools/nelisp-substrate-presence-sweep.sh

# The corpus above is GENERATED and checked in; this is its drift check,
# same shape as ns-inventory's checked-in baseline except an exact content
# comparison rather than a ratcheted count, because the corpus is fully
# deterministic.  Seconds, no binary -- lives in `check', unlike the sweep
# itself.
.PHONY: substrate-presence-corpus-check
substrate-presence-corpus-check:
	$(EMACS) --batch -Q -L packages/nl-prelude/src -L packages/nl-ns/src \
	  -l tools/nelisp-substrate-presence-gen.el \
	  --eval '(kill-emacs (nelisp-substrate-presence-gen-check))'

.PHONY: substrate-presence-corpus-regen
substrate-presence-corpus-regen:
	NELISP_SUBSTRATE_PRESENCE_GEN_WRITE=1 $(EMACS) --batch -Q -L packages/nl-prelude/src -L packages/nl-ns/src \
	  -l tools/nelisp-substrate-presence-gen.el

# Fast focused loop for CLI load work.  Builds/relinks target/nelisp using the
# incremental unit cache, then checks only `--load' output instead of running
# the full reader CLI/runtime-image/REPL smoke.
standalone-reader-load-smoke: standalone-reader
	@mkdir -p target
	@printf '%s\n' '(+ 40 3)' > target/standalone-reader-load-smoke.el
	@out="$$($(STANDALONE_BIN) --load target/standalone-reader-load-smoke.el)"; \
	if [ "$$out" = "43" ]; then \
	  echo "[standalone-reader-load-smoke] PASS: --load -> $$out"; \
	else \
	  echo "[standalone-reader-load-smoke] FAIL: --load -> $$out"; \
	  exit 1; \
	fi

# Feature registry and dependency-failure contract.  The missing-feature case
# is caught only to assert its condition type; require itself must signal it.
standalone-reader-require-provide-smoke: standalone-reader
	@mkdir -p target
	@printf '%s\n' '(list (progn (provide (quote zzz)) (featurep (quote zzz))) (featurep (quote never-provided)) (condition-case err (progn (require (quote no-such-feature-xyz)) (quote no-signal)) (file-missing (car err))) (require (quote no-such-feature-xyz) nil t) (progn (provide (quote zzz)) (require (quote zzz))))' > target/standalone-reader-require-provide-smoke.el
	@out="$$($(STANDALONE_BIN) --load target/standalone-reader-require-provide-smoke.el)"; \
	if [ "$$out" = "(t nil file-missing nil zzz)" ]; then \
	  echo "[standalone-reader-require-provide-smoke] PASS: -> $$out"; \
	else \
	  echo "[standalone-reader-require-provide-smoke] FAIL: -> $$out (expected (t nil file-missing nil zzz))"; \
	  exit 1; \
	fi

# Doc 170 Stage 2: checked-allocator smoke (redzone / generation tags /
# alloc-site / poison / leak scan).  Three runs against the same binary:
#   1. env OFF — behaviour must match the stock reader (43) and the
#      report head must be (0 0 ...) = checked mode fully disabled.
#   2. NELISP_ALLOC_CHECK=1 — same compute still yields 43 (the checked
#      suffix must not change program behaviour).
#   3. NELISP_ALLOC_CHECK=1 — churn garbage across a form boundary +
#      explicit garbage-collect, then assert via the report:
#      enable=1, armed=1, generation/checked-allocs/verified-frees > 0,
#      redzone violations = 0, alloc-site id round-trips.
# The runtime env probe is wired on the Windows and Linux standalone
# targets.  macOS has no boot env yet, so enable there via
# `(nelisp--debug-switch 19)' instead -- and note that switch stamps and
# poisons but deliberately does NOT arm the verifier (arming mid-run
# false-positives on blocks allocated before it), so run 3's armed=1
# assertion cannot pass that way.  That is why this whole target could
# only ever run on Windows until the Linux boot probe landed
# (2026-08-19); measured on linux-x86_64 the same day, run 3 reports
# 269820 verified frees and 0 redzone violations.
# NB: pass NELISP_STANDALONE_TARGET as a make VARIABLE (not just env) —
# MSYS make drops it from recipe environments otherwise:
#   make standalone-reader-checked NELISP_STANDALONE_TARGET=windows-x86_64
standalone-reader-checked: standalone-reader
	@mkdir -p target
	@printf '%s\n' '(nelisp--alloc-check-report)' > target/alloc-check-report.el
	@printf '%s\n' '(+ 40 3)' > target/alloc-check-compute.el
	@printf '%s\n' \
	  '(nelisp--debug-switch 21 42)' \
	  '(let* ((i 0) (acc nil)) (while (< i 200) (setq acc (cons i acc)) (setq i (+ i 1))) 0)' \
	  '(garbage-collect)' \
	  '(nelisp--alloc-check-report)' \
	  > target/alloc-check-smoke.el
	@bin=$(STANDALONE_BIN); \
	off="$$($$bin --load target/alloc-check-report.el | tail -n 1)"; \
	set -- $$(echo "$$off" | tr -d '()'); \
	if [ "$$1" = "0" ] && [ "$$2" = "0" ]; then \
	  echo "[standalone-reader-checked] PASS: default-off report head -> $$off"; \
	else \
	  echo "[standalone-reader-checked] FAIL: checked mode not off by default -> $$off"; \
	  exit 1; \
	fi; \
	out="$$($$bin --load target/alloc-check-compute.el | tail -n 1)"; \
	on="$$(NELISP_ALLOC_CHECK=1 $$bin --load target/alloc-check-compute.el | tail -n 1)"; \
	if [ "$$out" = "43" ] && [ "$$on" = "43" ]; then \
	  echo "[standalone-reader-checked] PASS: compute parity off/on -> $$out/$$on"; \
	else \
	  echo "[standalone-reader-checked] FAIL: compute parity off/on -> $$out/$$on"; \
	  exit 1; \
	fi; \
	rep="$$(NELISP_ALLOC_CHECK=1 $$bin --load target/alloc-check-smoke.el | tail -n 1)"; \
	set -- $$(echo "$$rep" | tr -d '()'); \
	if [ "$$1" = "1" ] && [ "$$2" = "1" ] && [ "$${3:-0}" -gt 0 ] \
	   && [ "$${4:-0}" -gt 0 ] && [ "$${5:-0}" -gt 0 ] && [ "$$6" = "0" ] \
	   && [ "$$8" = "42" ]; then \
	  echo "[standalone-reader-checked] PASS: $$rep"; \
	else \
	  echo "[standalone-reader-checked] FAIL: $$rep"; \
	  exit 1; \
	fi

# `elt' on an empty sequence used to SEGFAULT the process: no bounds check
# on the vector arm, and an else-arm that fell through to `str-byte-at' and
# dereferenced nil.  Both crash inputs are pinned here because a crash is
# the one failure a value-comparing test cannot report -- there is no value
# to compare, only an exit code.
.PHONY: standalone-reader-elt-smoke
standalone-reader-elt-smoke: $(if $(wildcard target/nelisp target/nelisp.exe),,standalone-reader)
	@bin=$(STANDALONE_BIN); \
	fail=0; \
	for expr in '(elt nil 0)' '(elt nil 5)' '(elt nil -1)' '(elt (list) 0)'; do \
	  out="$$($$bin --eval "$$expr" 2>&1)"; rc=$$?; \
	  if [ "$$rc" -ne 0 ]; then \
	    echo "[elt-smoke] FAIL $$expr exited $$rc (139 = SIGSEGV)"; fail=1; \
	  elif [ "$$out" != "nil" ]; then \
	    echo "[elt-smoke] FAIL $$expr -> $$out, expected nil"; fail=1; \
	  else \
	    echo "[elt-smoke] ok   $$expr -> nil"; \
	  fi; \
	done; \
	for pair in '(elt (list 1 2 3) 1)|2' '(elt [10 20 30] 2)|30' '(elt "abc" 1)|98' '(elt "あい" 0)|12354' '(elt (list 1 2) 5)|nil' \
	  '(condition-case e (elt [] 0) (args-out-of-range (quote signalled)))|signalled' \
	  '(condition-case e (elt [1 2 3] 5) (args-out-of-range (quote signalled)))|signalled'; do \
	  expr="$${pair%%|*}"; want="$${pair##*|}"; \
	  out="$$($$bin --eval "$$expr" 2>&1)"; rc=$$?; \
	  if [ "$$rc" -ne 0 ] || [ "$$out" != "$$want" ]; then \
	    echo "[elt-smoke] FAIL $$expr -> $$out (rc=$$rc), expected $$want"; fail=1; \
	  else \
	    echo "[elt-smoke] ok   $$expr -> $$out"; \
	  fi; \
	done; \
	if [ "$$fail" -ne 0 ]; then exit 1; fi; \
	echo "[elt-smoke] PASS"

# Doc 201 §4 item 1 / §5.2: structurally asserts that the literal-separator
# fast path (fixed dev/nelisp commit `70cd5852', generalized in `2d433761')
# bypasses the regexp engine.  Measures
# `split-string' itself on a synthetic ~60-entry, ~2KB literal-separator
# PATH -- not the full `executable-find' (whose own `expand-file-name'/
# `file-exists-p' loop costs ~1.8-2.5s independent of this defect on this
# machine, which would swamp an ~8x split-string regression's margin) --
# so this gate isolates the one thing it exists to catch. A synthetic
# PATH (not the host's real one, which varies by machine/CI) keeps the
# gate deterministic.  Elapsed time remains diagnostic, but the verdict is
# exact: `nlre--string-match-calls' must stay zero.  The former absolute 1.0s
# bound was host-sensitive: on WSL the injected path measured 0.596-0.653s
# and stayed green despite being ~14x slower than the 0.043-0.045s clean path.
standalone-reader-splitstring-perf-smoke: $(if $(wildcard target/nelisp target/nelisp.exe),,standalone-reader)
	@bin=$(STANDALONE_BIN); \
	mkdir -p target; \
	printf '%s\n' \
	  '(let* ((n 60) (path (format "/fake/nelisp-perf-smoke/dir-0")))' \
	  '  (let ((i 1)) (while (< i n) (setq path (concat path ";" (format "/fake/nelisp-perf-smoke/dir-%d" i))) (setq i (1+ i))))' \
	  '  (setq nlre--string-match-calls 0)' \
	  '  (let* ((t0 (float-time))' \
	  '         (dirs (split-string path ";"))' \
	  '         (elapsed (- (float-time) t0))' \
	  '         (regexp-calls nlre--string-match-calls))' \
	  '    (princ (format "n-dirs=%d elapsed=%.3f regexp-calls=%d pass=%S\n"' \
	  '                   (length dirs) elapsed regexp-calls' \
	  '                   (and (= (length dirs) n) (= regexp-calls 0))))))' \
	  > target/standalone-reader-splitstring-perf-smoke.el; \
	out="$$($$bin --load target/standalone-reader-splitstring-perf-smoke.el 2>&1)"; \
	case "$$out" in \
	  *"pass=t"*) echo "[splitstring-perf-smoke] PASS: $$out";; \
	  *) echo "[splitstring-perf-smoke] FAIL: $$out (expected pass=t -- split-string may have regressed to the regexp-engine path)"; exit 1;; \
	esac

# Doc 201 §4 item 1: the `path-separator' (§1) and `file-exists-p' (§2)
# fixes shipped with no gate at all -- their repro lived outside this repo
# (dev/nelisp-v11-ime-verify/) and was never ported.  Writing this gate
# found a THIRD defect of the same shape that §2's own verification could
# not see: `expand-file-name' was POSIX-only, so on windows-nt a
# drive-letter directory came back as "/C:/Windows/System32/cmd.exe" and
# `executable-find' STILL found nothing on a windows-native build with §1
# and §2 both in.  §2 could not see it because none of the programs it
# probed for (python/kakasi/look) were installed on that host -- "not
# found" was the correct answer either way, so the end-to-end path was
# never once run against a program that IS present.  This gate runs it
# against a marker file it creates itself.
#
# A FOURTH defect of the same shape turned up once the gate existed:
# `nl_os_getcwd' was a `wf_write_nil' stub on windows only, so
# `default-directory' was nil there and every relative name resolved
# against "/" -- the failure the POSIX and macOS arms of that same function
# each carry a comment about having already fixed.  Hence the `dd'
# assertions: absolute, slash-terminated, and actually used by
# `expand-file-name'.
#
# Portable by construction: the marker's absolute directory comes from
# `pwd -W' (a drive-letter path under MSYS/Git Bash, which is where the
# defect lives) falling back to plain `pwd', so the same assertions run
# unchanged on Linux, where they stand as a regression guard.
.PHONY: standalone-reader-winpath-smoke
standalone-reader-winpath-smoke: $(if $(wildcard target/nelisp target/nelisp.exe),,standalone-reader)
	@bin=$(STANDALONE_BIN); \
	mkdir -p target/tmp/winpath-smoke; \
	: > target/tmp/winpath-smoke/nelisp-doc201-marker; \
	dir="$$(pwd -W 2>/dev/null || pwd)/target/tmp/winpath-smoke"; \
	printf '%s\n' \
	  "(let* ((dir \"$$dir\")" \
	  '       (marker (concat dir "/nelisp-doc201-marker"))' \
	  '       (sep-ok (equal path-separator (if (eq system-type (quote windows-nt)) ";" ":")))' \
	  '       (abs-ok (and (file-name-absolute-p dir) t))' \
	  '       (exp-ok (equal (expand-file-name "nelisp-doc201-marker" dir) marker))' \
	  '       (yes-ok (and (file-exists-p marker) t))' \
	  '       (no-ok (null (file-exists-p (concat dir "/nelisp-doc201-absent"))))' \
	  '       (dd default-directory)' \
	  '       (dd-ok (and (stringp dd) (> (length dd) 0)' \
	  '                   (and (file-name-absolute-p dd) t)' \
	  '                   (eq (aref dd (1- (length dd))) ?/)' \
	  '                   (equal (expand-file-name "nelisp-doc201-rel") (concat dd "nelisp-doc201-rel"))))' \
	  '       (found (progn (setenv "PATH" (concat dir path-separator "/nelisp-doc201/no-such-dir"))' \
	  '                     (executable-find "nelisp-doc201-marker")))' \
	  '       (find-ok (equal found marker)))' \
	  '  (princ (format "sep=%S abs-ok=%S exp-ok=%S yes-ok=%S no-ok=%S dd=%S found=%S pass=%S\n"' \
	  '                 path-separator abs-ok exp-ok yes-ok no-ok dd found' \
	  '                 (and sep-ok abs-ok exp-ok yes-ok no-ok dd-ok find-ok))))' \
	  > target/standalone-reader-winpath-smoke.el; \
	out="$$($$bin --load target/standalone-reader-winpath-smoke.el 2>&1)"; \
	case "$$out" in \
	  *"pass=t"*) echo "[winpath-smoke] PASS: $$out";; \
	  *) echo "[winpath-smoke] FAIL: $$out (expected pass=t -- path-separator, file-exists-p or expand-file-name regressed)"; exit 1;; \
	esac

# Doc 201 §4 item 2: `nl_os_stat_path'/`nl_os_lstat_path'/`nl_os_open_dir'/
# `nl_os_getdents64' were unconditional -ENOSYS stubs on the windows-native
# target (Doc 151 §B), so `file-attributes' answered a size of -38 and
# `directory-files' answered nil for every directory that exists.  They are
# now GetFileAttributesExW and FindFirstFileW/FindNextFileW/FindClose.
# `nelisp--syscall-stat' moved onto the real st_mode at the same time: its
# access(2) trichotomy asked whether a path accepts a trailing slash, and
# Windows says yes for an ordinary file, so `file-regular-p' answered nil
# and `file-directory-p' answered t for every regular file on that target.
#
# The fixture is built by this recipe (10 bytes, one subdirectory), so
# every assertion has a known right answer and none of them depend on the
# host's own filesystem.  `file-attribute-modification-time' is compared
# as a POSIX second count because that is what this runtime's
# `file-attributes' puts there -- not the list Emacs returns.
.PHONY: standalone-reader-fileattrs-smoke
standalone-reader-fileattrs-smoke: $(if $(wildcard target/nelisp target/nelisp.exe),,standalone-reader)
	@bin=$(STANDALONE_BIN); \
	d=target/tmp/fileattrs-smoke; \
	rm -rf $$d; mkdir -p $$d/sub; \
	printf '0123456789' > $$d/ten-bytes.txt; \
	dir="$$(pwd -W 2>/dev/null || pwd)/$$d"; \
	printf '%s\n' \
	  "(let* ((dir \"$$dir\")" \
	  '       (f (concat dir "/ten-bytes.txt"))' \
	  '       (sub (concat dir "/sub"))' \
	  '       (attrs (file-attributes f))' \
	  '       (mtime (file-attribute-modification-time attrs))' \
	  '       (size-ok (equal (file-attribute-size attrs) 10))' \
	  '       (mtime-ok (and (integerp mtime) (> mtime 1700000000)))' \
	  '       (reg-ok (and (file-regular-p f) (not (file-directory-p f))))' \
	  '       (dir-ok (and (file-directory-p sub) (not (file-regular-p sub))))' \
	  '       (names (directory-files dir))' \
	  '       (names-ok (and (= (length names) 2) (and (member "sub" names) t)' \
	  '                      (and (member "ten-bytes.txt" names) t)))' \
	  '       (absent-ok (and (null (file-attributes (concat dir "/nope")))' \
	  '                       (null (directory-files (concat dir "/nope"))))))' \
	  '  (princ (format "size-ok=%S mtime=%S reg-ok=%S dir-ok=%S names=%S absent-ok=%S pass=%S\n"' \
	  '                 size-ok mtime reg-ok dir-ok names absent-ok' \
	  '                 (and size-ok mtime-ok reg-ok dir-ok names-ok absent-ok))))' \
	  > target/standalone-reader-fileattrs-smoke.el; \
	out="$$($$bin --load target/standalone-reader-fileattrs-smoke.el 2>&1)"; \
	case "$$out" in \
	  *"pass=t"*) echo "[fileattrs-smoke] PASS: $$out";; \
	  *) echo "[fileattrs-smoke] FAIL: $$out (expected pass=t -- stat, readdir or the file/directory classification regressed)"; exit 1;; \
	esac

# Doc 201 §5.4: `nlre-string-match' retried at every start position and did
# full work at each one -- a fresh `make-vector' plus a walk into
# `nlre--match-list''s dispatch chain -- even where the pattern's first node
# is a literal the character at that position plainly is not.  That is the
# cost behind skk-version.el's ~1.9s for one `(dolist (p load-path)
# (string-match "ddskk-[0-9]+\\.[0-9]+" p))', i.e. ~59ms per call against a
# ~51-character string.
#
# The ratio remains a same-process diagnostic, and both halves run in the same
# process on the same 42 strings: sweep A uses a pattern whose first node is
# a mandatory literal (the shape the filter can skip positions for), sweep B
# a pattern with no leading literal (the shape it cannot help).  Neither
# matches, so both are full scans.  The verdict is structural rather than
# timed: `nlre--leading-filter-calls' must report exactly the 42 sweep-A
# calls and none of sweep B.  The old ratio < 0.25 verdict was host-sensitive:
# WSL measured 0.272-0.301 injected and 0.065-0.071 clean, while CI's runner
# stayed green with the same injection.
#
# The thirteen correctness cases are not filler.  A leading-character filter
# that fires where it must not silently SKIPS REAL MATCHES, which no timing
# assertion can see -- so the anchored, case-folded, starred, grouped and
# empty-pattern shapes are each checked, and every expected value here was
# read out of stock Emacs 30.1 running the same expressions, not asserted
# from the spec.  `r7'/`g2' specifically cover the reused capture vector: the
# group matches inside a FAILED attempt at position 0 before the successful
# one at 2, so a scan that forgot to clear it reports the stale 0.
.PHONY: standalone-reader-regexp-lead-filter-smoke
standalone-reader-regexp-lead-filter-smoke: $(if $(wildcard target/nelisp target/nelisp.exe),,standalone-reader)
	@bin=$(STANDALONE_BIN); \
	mkdir -p target; \
	printf '%s\n' \
	  '(let* ((got (list (string-match "abc" "xxabcyy")' \
	  '                  (string-match "abc" "xxabxx")' \
	  '                  (let ((case-fold-search t)) (string-match "ABC" "xxabc"))' \
	  '                  (let ((case-fold-search nil)) (string-match "ABC" "xxabc"))' \
	  '                  (string-match "^b" "a\nb")' \
	  '                  (string-match "a*" "bbb")' \
	  '                  (string-match "x" "")' \
	  '                  (string-match "" "abc")' \
	  '                  (string-match "b" "abc" 2)))' \
	  '       (want (list 2 nil 2 nil 2 0 nil 0 nil))' \
	  '       (r6 (string-match "\\(a+\\)\\(b+\\)" "zzaabbb"))' \
	  '       (g1 (list (match-beginning 1) (match-end 1)' \
	  '                 (match-beginning 2) (match-end 2)))' \
	  '       (r7 (string-match "\\(a\\)c" "abac"))' \
	  '       (g2 (list (match-beginning 1) (match-end 1)))' \
	  '       (correct-ok (and (equal got want) (equal r6 2) (equal g1 (list 2 4 4 7))' \
	  '                        (equal r7 2) (equal g2 (list 2 3))))' \
	  '       (paths nil) (i 0))' \
	  '  (while (< i 42)' \
	  '    (setq paths (cons (format "/home/someone/.emacs.d/elpa/package-%02d/lisp" i) paths))' \
	  '    (setq i (1+ i)))' \
	  '  (setq nlre--leading-filter-calls 0)' \
	  '  (let* ((t0 (float-time))' \
	  '         (_a (dolist (p paths) (string-match "ddskk-[0-9]+\\.[0-9]+" p)))' \
	  '         (ta (- (float-time) t0))' \
	  '         (t1 (float-time))' \
	  '         (_b (dolist (p paths) (string-match "[0-9]+zzz" p)))' \
	  '         (tb (- (float-time) t1))' \
	  '         (ratio (/ ta tb))' \
	  '         (filter-calls nlre--leading-filter-calls)' \
	  '         (filter-ok (= filter-calls (length paths))))' \
	  '    (princ (format "got=%S r6=%S g1=%S r7=%S g2=%S lead=%.3f nolead=%.3f ratio=%.3f filter-calls=%d pass=%S\n"' \
	  '                   got r6 g1 r7 g2 ta tb ratio filter-calls' \
	  '                   (and correct-ok filter-ok)))))' \
	  > target/standalone-reader-regexp-lead-filter-smoke.el; \
	out="$$($$bin --load target/standalone-reader-regexp-lead-filter-smoke.el 2>&1)"; \
	case "$$out" in \
	  *"pass=t"*) echo "[regexp-lead-filter-smoke] PASS: $$out";; \
	  *) echo "[regexp-lead-filter-smoke] FAIL: $$out (expected pass=t -- the leading-character filter is skipping real matches, or is no longer firing)"; exit 1;; \
	esac

# Doc 201 §6.9 item 5.  The standalone has TWO readers: the native parser
# that drives `load', and `nelisp--rd-one', a reader written in interpreted
# Elisp that `read-from-string' resolves to.  Nothing compared them, and
# they disagreed: `?x' read as the SYMBOL `?x' where Emacs and the native
# parser both answer the integer 120 -- `nelisp--rd-one' had no `?' arm at
# all.  Ninety-odd call sites in this tree use `read-from-string'.
#
# This gate reads the same corpus through BOTH and requires them to agree,
# which is the check whose absence let one of them be wrong.  Every
# expected value in `target/rd-*.txt' was read out of stock Emacs when the
# corpus was written; the gate does not re-derive them, it requires the two
# in-tree readers to answer the same thing -- a claim it can check without
# an Emacs to hand, on any target.
#
# `read' takes the native path (see its own comment in the prelude), so
# `read' vs `read-from-string' IS native vs Elisp.
.PHONY: standalone-reader-reader-parity-smoke
standalone-reader-reader-parity-smoke: $(if $(wildcard target/nelisp target/nelisp.exe),,standalone-reader)
	@mkdir -p target; \
	printf '%s\n' \
	  '(let ((cases (list "?x" "?A" "?0" "? " "?)" "?(" "?;" "?\x27"' \
	  '                   "?\\n" "?\\t" "?\\r" "?\\f" "?\\e" "?\\s" "?\\\\" "?\\a"' \
	  '                   "?\\b" "?\\d" "?\\0" "?\\\"" "?\\?"' \
	  '                   "(?a ?b)" "(list ?x 1)" "[?a ?b]"' \
	  '                   "42" "-7" "1.5" "abc" "\"s\"" "(a . b)" "()" "nil" "t"' \
	  '                   ":kw" "(a b) trailing" "  (ws)"))' \
	  '      (bad 0) (checked 0))' \
	  '  (dolist (s cases)' \
	  '    (setq checked (1+ checked))' \
	  '    (let ((native (condition-case e (read s) (error (list (quote ERR) (car e)))))' \
	  '          (elisp  (condition-case e (car (read-from-string s)) (error (list (quote ERR) (car e))))))' \
	  '      (unless (equal native elisp)' \
	  '        (setq bad (1+ bad))' \
	  '        (princ (format "  DISAGREE %S native=%S elisp=%S\n" s native elisp)))))' \
	  '  (princ (format "checked=%d disagreements=%d chars-ok=%S pass=%S\n"' \
	  '                 checked bad (equal (read "?x") 120) (= bad 0))))' \
	  > target/standalone-reader-reader-parity-smoke.el; \
	out="$$($(STANDALONE_BIN) --load target/standalone-reader-reader-parity-smoke.el 2>&1)"; \
	case "$$out" in \
	  *"pass=t"*) echo "[reader-parity-smoke] PASS: $$out";; \
	  *) echo "[reader-parity-smoke] FAIL: $$out (expected pass=t -- the native reader and nelisp--rd-one disagree)"; exit 1;; \
	esac

# Doc 201 §5.3: §5.1 replaced `nl_sf_defvar'/`nl_sf_defconst''s per-call AST
# synthesis (intern 5 symbols, build 9 cons cells, re-enter the interpreter
# on the synthetic form) with direct `nelisp_mirror_is_bound'/
# `nl_env_set_value' calls, and shipped without a gate.  Measured with the
# allocator's own counters (`(nelisp--debug-switch 24)' arms them,
# `(nelisp--debug-switch 0)' reads bucket/linear/bump hits at list positions
# 10/11/12): ~1629 allocations per top-level `defvar' before the fix, 66
# after.  The 200-per-form budget below sits 3x above the fixed number and
# 8x below the regressed one, and the counters are exact rather than timed,
# so this bound does not move with machine load the way a wall-clock one
# would.
#
# The two semantic assertions are not decoration: the fix's whole risk was
# that skipping the synthetic `(if (boundp ...) nil (set ...))' form would
# also skip its boundp gate.  `defvar' must leave an already-bound value
# alone; `defconst' must overwrite it.
.PHONY: standalone-reader-defvar-alloc-smoke
standalone-reader-defvar-alloc-smoke: $(if $(wildcard target/nelisp target/nelisp.exe),,standalone-reader)
	@bin=$(STANDALONE_BIN); \
	mkdir -p target; \
	f=target/standalone-reader-defvar-alloc-smoke.el; \
	echo '(nelisp--debug-switch 24)' > $$f; \
	i=1; while [ $$i -le 200 ]; do \
	  echo "(defvar nelisp-doc201-v$$i $$i)"; i=$$((i+1)); \
	done >> $$f; \
	printf '%s\n' \
	  '(setq nelisp-doc201-st (nelisp--debug-switch 0))' \
	  '(nelisp--debug-switch 25)' \
	  '(setq nelisp-doc201-keep 41)' \
	  '(defvar nelisp-doc201-keep 99)' \
	  '(defconst nelisp-doc201-const 1)' \
	  '(defconst nelisp-doc201-const 2)' \
	  '(let* ((total (+ (nth 10 nelisp-doc201-st) (nth 11 nelisp-doc201-st)' \
	  '                 (nth 12 nelisp-doc201-st)))' \
	  '       (per (/ total 200))' \
	  '       (budget 200)' \
	  '       (alloc-ok (< per budget))' \
	  '       (keep-ok (= nelisp-doc201-keep 41))' \
	  '       (const-ok (= nelisp-doc201-const 2)))' \
	  '  (princ (format "per-form=%d budget=%d keep-ok=%S const-ok=%S pass=%S\n"' \
	  '                 per budget keep-ok const-ok' \
	  '                 (and alloc-ok keep-ok const-ok))))' \
	  >> $$f; \
	out="$$($$bin --load $$f 2>&1)"; \
	case "$$out" in \
	  *"pass=t"*) echo "[defvar-alloc-smoke] PASS: $$out";; \
	  *) echo "[defvar-alloc-smoke] FAIL: $$out (expected pass=t -- defvar/defconst regressed to AST synthesis, or lost their boundp gate)"; exit 1;; \
	esac

# A name the standalone provides natively AND the prelude redefines
# unconditionally has two implementations, and which one runs depends on
# whether the prelude was loaded.  This evaluates the same expressions both
# ways and requires the same answers.  See the case file's commentary for
# the three defects of this shape found by hand on 2026-08-19.
.PHONY: standalone-reader-shadow-smoke
# The shadow smoke compares the standalone against ITSELF (native builtins vs
# the prelude's redefinitions).  That cannot see a case where both halves are
# wrong the same way -- which is most of what an Emacs-compatibility runtime
# gets wrong.  This target compares the same file against STOCK EMACS, printer
# to printer, and requires the two to be byte-identical.
#
# Both sides print through `format "%S"' deliberately.  Reading the
# standalone's own value echo instead compares Emacs's printer against a
# DIFFERENT NeLisp printer (the native `nelisp--repr'), and the two differ on
# backslash escaping inside a nested string -- an hour went into that mirage
# on 2026-08-19.
# The edit-check loop, measured rather than assumed on 2026-08-19: the
# standalone rebuild is ~14s and the seven gates together are ~6s, while
# `make test' alone is ~70s.  So the loop worth optimising was never the
# build -- it was running the full ERT suite after every one-line change.
# This target is what to run between edits; run `make test' before the
# commit, not before each measurement.
# `emacs-parity' checks a corpus I wrote; this SEARCHES for cases I did not
# think to write.  It generates calls to the names both runtimes define,
# from an argument pool weighted toward the shapes that actually broke
# things (empty sequence, improper list, negative index, index past the end,
# non-ASCII, wrong type entirely), prints both answers, and shrinks any
# disagreement to a minimal call.
#
# Not wired into CI as a blocking gate: it reports where the two differ, not
# which one is right, and that is a reading job.  Run it, read it, and turn
# what it finds into `emacs-parity' cases -- those are the ones that stay.
#
#   NELISP_FUZZ_SEED=7 NELISP_FUZZ_CASES=4000 make parity-fuzz
#   NELISP_FUZZ_ONLY='^string-' make parity-fuzz
# "buffer ops / text properties / overlays / coding systems are not covered"
# was an impression until this printed the number: 100 of 424 shared names,
# 23%.  It counts MENTIONS, not exercise, so it is a floor to push up rather
# than a score -- `parity-fuzz' is what searches the space.  Two numbers that
# measure different things beat one that pretends to measure both.
# Every gate says whether what it looked at was clean.  None of them said
# whether they looked at anything -- and three were green while seeing
# nothing on 2026-08-19.  This runs each gate and requires its `checked'
# count inside a band.
# `gate-selfcheck' asks whether each gate looked at anything.  This asks the
# harder question: would it CATCH something.  Each row injects a known
# defect, requires the gate to go red, and restores the file.
# The worst defects fixed on 2026-08-19 were all one shape: a function that
# takes an argument Emacs defines, ignores it, and ANSWERS.  The tell is
# mechanical -- an `_'-prefixed parameter in a function stock Emacs also
# defines -- so the list is visible now and every site has to be
# acknowledged with what it does instead.
# An argument check added to a ONE-LINE defun lands after its closing paren
# and becomes a top-level form: it runs during the prelude load and fails
# with `void-variable' on the parameter name, nowhere near the function.
# That happened twice on 2026-08-20.
prelude-toplevel-check:
	@$(EMACS) --batch -Q -l tools/nelisp-prelude-toplevel-check.el

# The standalone build emits several Elisp programs as string literals, so
# their parens are invisible to `parens-check', which reads the .el file and
# not the text it produces.  One dropped paren on 2026-08-19 nested the
# artifact command dispatch inside an `unless' that never runs, and
# `compile-elisp-artifact' silently did nothing for two days.
generated-source-parse:
	@$(EMACS) --batch -Q -L lisp -L src -L scripts \
	  --eval '(setq load-prefer-newer t)' \
	  -l tools/nelisp-generated-source-parse.el \
	  -f nelisp-generated-source-parse-run

# Doc 200 option A cannot safely add string tags until every existing Str (5)
# and MutStr (6) test/write has a stable audit row.  This source-only reader
# regenerates the live (FILE, ENCLOSING, KIND, NTH) keys and compares only
# those keys with the ledger, preserving its human STATUS and NOTE columns.
doc200-census:
	@$(EMACS) --batch -Q -l tools/nelisp-doc200-tag-census.el \
	  -f nelisp-doc200-tag-census-run

partial-inventory:
	@$(EMACS) --batch -Q -l tools/nelisp-partial-inventory.el

gate-mutation:
	@tools/nelisp-gate-mutation.sh

# Verify ONE gate's mutation row(s) without paying for the whole sweep.
#
#   make gate-mutation-verify GATE=standalone-midform-gc-bounded
#
# Authoring a row obliges you to prove it is REACHED -- inject, see RED,
# restore, see GREEN.  Three rows in this repo's history shipped without that
# proof and each cost a CI round: one aimed at a path its gate never walked,
# one was quietly made non-lethal by a later improvement, and one was RED
# locally on every attempt but green in CI.  A full `gate-mutation' sweep is
# too slow to run per row, which is exactly why those proofs got skipped.
.PHONY: gate-mutation-verify
gate-mutation-verify:
	@test -n "$(GATE)" || { echo "usage: make gate-mutation-verify GATE=<gate-name>"; exit 2; }
	@NELISP_GATE_MUTATION_ONLY="$(GATE)" tools/nelisp-gate-mutation.sh

version-consistency:
	@bash tools/nelisp-version-consistency.sh

gate-selfcheck:
	@$(EMACS) --batch -Q -l tools/nelisp-gate-selfcheck.el

parity-coverage:
	@$(EMACS) --batch -Q -l tools/nelisp-parity-coverage.el

parity-fuzz: $(if $(wildcard target/nelisp target/nelisp.exe),,standalone-reader)
	@NELISP_REPO_ROOT=$(CURDIR) $(EMACS) --batch -Q -l tools/nelisp-parity-fuzz.el

inner: standalone-reader emacs-parity
	@$(MAKE) --no-print-directory standalone-reader-shadow-smoke
	@$(MAKE) --no-print-directory standalone-reader-elt-smoke
	@$(MAKE) --no-print-directory standalone-reader-prelude-test
	@echo "[inner] build + parity + standalone smokes clean"

# The reference side of this gate is whatever `$(EMACS)' answers LIVE, not
# a frozen file -- so a host running a different Emacs major version is not
# testing a NeLisp defect, it is testing whether stock Emacs agrees with
# itself release to release.  The standalone's own answers were built and
# read against Emacs 30.1 (`docs/emacs-compat-table.txt' header reads
# "emacs 30.1, 7114 names", generated by `make emacs-compat-table' running
# the development host's own Emacs; `packages/nl-ns/baseline/emacs-30.1.el'
# is the same 30.1 pin for the separate namespace tooling), and CI runs an
# Emacs 29.4 lane alongside 30.1 (.github/workflows/ci.yml matrix).
# Measured on GitHub Actions run 32606582250 (2026-08-23): the 29.4 lane
# failed this gate with a 1,844-line diff while the 30.1 lane, same commit,
# same corpus, passed -- real Emacs 29.4 answers some of these expressions
# differently than Emacs 30.x does, and NeLisp matches 30.x.  A host
# outside the pinned major reports a reasoned GATE-SKIP instead of failing
# on a version gap this gate cannot close by running harder.
# `NELISP_EMACS_PARITY_HOST_VERSION' overrides the detected host version --
# it exists so this guard can be exercised without a second Emacs install;
# unset, it always reflects the real `$(EMACS) --version'.
#
# The wrapped-corpus file below (target/emacs-parity.el) used to be
# assembled from three separate shell steps -- `printf ... >', `cat ...
# >>', `printf ... >>' -- each its own process writing to the same path.
# 2026-08-23 Windows inventory: on that host the generated file lacked its
# opening `(princ (format "%S" (progn' wrapper entirely and ended with an
# unmatched `)))', so Emacs read it as the corpus's own top-level forms
# followed by a stray close-paren and failed with `invalid-read-syntax'.
# That exact three-step shell sequence could not be reproduced failing on
# Linux (a mock run with the same commands under bash still writes a
# correct file here), so the precise MSYS2/Git-Bash/Windows-Emacs
# mechanism -- interleaved writes, `\r'-corrupted line continuation, or
# something else entirely -- is not confirmed from this host. Rather than
# guess at that mechanism, generation now goes through ONE Emacs process
# and ONE `write-region' call instead of three separate shell redirects
# to the same file: this cannot exhibit "some but not all of three
# sequential writes landed", because there is only one write. Not
# Windows-verified; the owner's next runbook run on Windows is the actual
# proof this holds there too.
# `wc -c' is piped through `tr -d " "' below because BSD wc right-pads its
# count.  Without that, the line reads `GATE-COUNT checked=   19900' and the
# `checked=\([0-9]+\)' parsers in tools/nelisp-gate-selfcheck.el and
# tools/ai/nelisp-ai.sh match nothing -- so a gate that had just compared
# 19,900 bytes was reported as one that examined nothing, on macOS only.
# `binary-size-ratchet' below already strips it; this target and the
# checked-allocator soak did not.
emacs-parity: $(if $(wildcard target/nelisp target/nelisp.exe),,standalone-reader)
	@mkdir -p target; \
	host_version="$${NELISP_EMACS_PARITY_HOST_VERSION:-$$($(EMACS) --version | head -1 | grep -oE '[0-9]+\.[0-9]+' | head -1)}"; \
	case "$$host_version" in \
	  30.*) : ;; \
	  *) echo "GATE-SKIP emacs-parity requires stock Emacs 30.x (host has $$host_version); the reference answers are version-pinned"; \
	     echo "[emacs-parity] SKIP: host Emacs $$host_version is outside the pinned 30.x range"; \
	     exit 0;; \
	esac; \
	$(EMACS) --batch -Q --eval '(with-temp-buffer (insert "(princ (format \"%S\" (progn\n") (goto-char (point-max)) (insert-file-contents "test/nelisp-shadow-differential-cases.el") (goto-char (point-max)) (insert ")))\n") (write-region (point-min) (point-max) "target/emacs-parity.el" nil (quote silent)))'; \
	bin=./target/nelisp; \
	case "$(NELISP_STANDALONE_TARGET)$$NELISP_STANDALONE_TARGET" in \
	  windows*) bin=./target/nelisp.exe;; \
	esac; \
	$(EMACS) --batch -Q -l target/emacs-parity.el > target/emacs-parity-emacs.txt 2>/dev/null; \
	$$bin --load target/emacs-parity.el > target/emacs-parity-nelisp.txt 2>/dev/null; \
	if [ ! -s target/emacs-parity-emacs.txt ]; then \
	  echo "[emacs-parity] FAIL: Emacs produced no output -- the cases file did not evaluate"; exit 1; \
	fi; \
	n=$$(wc -c < target/emacs-parity-emacs.txt | tr -d ' '); \
	head -c $$n target/emacs-parity-nelisp.txt > target/emacs-parity-nelisp-head.txt; \
	if cmp -s target/emacs-parity-emacs.txt target/emacs-parity-nelisp-head.txt; then findings=0; else findings=1; fi; \
	echo "GATE-COUNT checked=$$n findings=$$findings"; \
	if [ "$$findings" = 0 ]; then \
	  echo "[emacs-parity] PASS: $$n bytes identical to stock Emacs"; \
	else \
	  echo "[emacs-parity] FAIL: the standalone answers differently from stock Emacs"; \
	  fold -w100 target/emacs-parity-emacs.txt > target/emacs-parity-e.f; \
	  fold -w100 target/emacs-parity-nelisp-head.txt > target/emacs-parity-n.f; \
	  diff target/emacs-parity-e.f target/emacs-parity-n.f | head -40; \
	  exit 1; \
	fi

# `binary_identity()' in tools/ai/nelisp-ai.sh has always computed
# target/nelisp's size; nothing compared it to anything.  This is the
# comparison, pinned against tools/nelisp-binary-size-baseline.txt the same
# way unsafe-inventory/fallback-inventory/pkg-graph pin theirs -- raise
# `size' in that file, in the commit that explains the growth.  Consumes
# whichever binary is already in target/ (built here only if neither
# target/nelisp nor target/nelisp.exe exists yet), the same conditional
# prerequisite `emacs-parity' above uses, rather than forcing a fresh
# build for a check that only needs to weigh what is already there.  The
# baseline itself is Linux x86_64 only (like several other measured-here
# gates); a differently-targeted build reports a reasoned GATE-SKIP
# instead of comparing an ELF from a different linker against a number
# that was never measured for it.
binary-size-ratchet: $(if $(wildcard target/nelisp target/nelisp.exe),,standalone-reader)
	@target="$(NELISP_STANDALONE_TARGET)$$NELISP_STANDALONE_TARGET"; \
	if [ -n "$$target" ] && [ "$$target" != "linux-x86_64" ]; then \
	  echo "GATE-SKIP baseline pinned to linux-x86_64 only, target=$$target"; \
	  echo "[binary-size-ratchet] SKIP: not the pinned target"; \
	  exit 0; \
	fi; \
	bin=./target/nelisp; \
	if [ ! -f "$$bin" ]; then \
	  echo "GATE-COUNT checked=0 findings=1"; \
	  echo "[binary-size-ratchet] FAIL: $$bin not found"; \
	  exit 1; \
	fi; \
	baseline=$$(awk '$$1=="size"{print $$2}' tools/nelisp-binary-size-baseline.txt); \
	slack=$$(awk '$$1=="slack-pct"{print $$2}' tools/nelisp-binary-size-baseline.txt); \
	if [ -z "$$baseline" ] || [ -z "$$slack" ]; then \
	  echo "GATE-COUNT checked=0 findings=1"; \
	  echo "[binary-size-ratchet] FAIL: tools/nelisp-binary-size-baseline.txt missing 'size' or 'slack-pct'"; \
	  exit 1; \
	fi; \
	actual=$$(wc -c < "$$bin" | tr -d ' '); \
	ceiling=$$(( baseline + baseline * slack / 100 )); \
	if [ "$$actual" -le "$$ceiling" ]; then findings=0; else findings=1; fi; \
	echo "GATE-COUNT checked=1 findings=$$findings"; \
	if [ "$$findings" = 0 ]; then \
	  echo "[binary-size-ratchet] PASS: $$bin is $$actual bytes (baseline $$baseline, ceiling $$ceiling, slack $$slack%)"; \
	else \
	  over=$$(( actual - baseline )); \
	  echo "[binary-size-ratchet] FAIL: $$bin is $$actual bytes, $$over over baseline $$baseline -- exceeds ceiling $$ceiling ($$slack% slack).  If this growth is real and explained, raise 'size' in tools/nelisp-binary-size-baseline.txt in the same commit."; \
	  exit 1; \
	fi

standalone-reader-shadow-smoke: $(if $(wildcard target/nelisp target/nelisp.exe),,standalone-reader)
	@mkdir -p target
	@cp test/nelisp-shadow-differential-cases.el target/shadow-native.el
	@printf '%s\n' '(load "scripts/nelisp-stdlib-prelude.el")' > target/shadow-prelude.el
	@cat test/nelisp-shadow-differential-cases.el >> target/shadow-prelude.el
	@bin=$(STANDALONE_BIN); \
	native="$$($$bin --load target/shadow-native.el 2>&1 | tail -n 1)"; \
	prelude="$$($$bin --load target/shadow-prelude.el 2>&1 | tail -n 1)"; \
	case "$$native" in \
	  "("*) : ;; \
	  *) echo "[shadow-smoke] FAIL: the native run produced no list -> $$native"; exit 1;; \
	esac; \
	case "$$prelude" in \
	  "("*) : ;; \
	  *) echo "[shadow-smoke] FAIL: the prelude run produced no list -> $$prelude"; exit 1;; \
	esac; \
	if [ "$$native" = "$$prelude" ]; then \
	  echo "[shadow-smoke] PASS: native and prelude agree"; \
	  echo "[shadow-smoke]   $$native"; \
	else \
	  echo "[shadow-smoke] FAIL: the prelude answers differently from the native builtins"; \
	  echo "[shadow-smoke]   native  $$native"; \
	  echo "[shadow-smoke]   prelude $$prelude"; \
	  exit 1; \
	fi

# Doc 170 section 5.3: the soak, run with the verifying allocator armed,
# with redzone corruption and leaks each a blocker.
#
# Section 5 rates this allocator the highest bug-detection-per-effort item
# in that design, and until 2026-08-19 it could not be armed off Windows at
# all -- `nl_os_environ_init' was a no-op, so NELISP_ALLOC_CHECK=1 never
# reached the boot probe.  It reads envp off the Linux entry stack now, so
# this lane exists.
#
# Two blockers, from `(nelisp--alloc-check-report)':
#
#   redzone   violations must be 0.  A guard word is stamped into every
#             allocation's suffix and checked on free, so a write past the
#             end of a block is caught at the free rather than wherever the
#             corruption later surfaced.
#
#   leak      live-blocks must not grow across rounds.  Each round runs the
#             same workload and ends with `garbage-collect', so what is
#             still reachable afterwards is retention, not garbage.  A
#             round-over-round rise means the runtime is holding something
#             the previous round already finished with.
#
# ROUNDS defaults to 3 for a CI-shaped run; the release lane passes more.
# Deliberately NOT the 1h wall-clock of `soak-1h': an hour of the same loop
# adds confidence about time, and rounds add confidence about repetition,
# which is what a leak test actually needs.
#
# The rounds run INSIDE one process.  A first cut ran each round as its own
# `--load' and compared live-blocks across them, which cannot fail: a fresh
# process starts with a fresh heap, so the numbers were identical by
# construction and the leak blocker was decorative.
#
# The leak blocker allows a few blocks of slack, and the number comes from
# measurement rather than taste.  A strict `>' comparison failed on this
# workload: run to run the settled census wobbles by a single block out of
# about 145000, so the gate went red carrying no information -- which is how
# a gate stops being read.  Eight rounds showed the wobble is not retention:
# 144839, then 145065 held flat for six more rounds, so the runtime settles
# and stays settled.
#
# The slack sits between the two measured magnitudes.  Observed noise is 1
# block; the known-answer leak -- 2000 conses held past the collect -- moves
# it by 4000 per round (149866 154092 158092, measured 2026-08-19).  8 is
# comfortably above the first and 500x below the second, so the blocker
# still fires on anything that accumulates and ignores what does not.
.PHONY: standalone-reader-checked-soak
STANDALONE_CHECKED_SOAK_ROUNDS ?= 3
STANDALONE_CHECKED_SOAK_SLACK ?= 8
standalone-reader-checked-soak: $(if $(wildcard target/nelisp target/nelisp.exe),,standalone-reader)
	@mkdir -p target
	@printf '%s\n' \
	  '(load "scripts/nelisp-stdlib-prelude.el")' \
	  '(defun checked-soak-round ()' \
	  '  (let* ((i 0) (acc nil))' \
	  '    (while (< i 4000) (setq acc (cons (make-string 48 66) acc)) (setq i (+ i 1))))' \
	  '  (let* ((i 0) (acc nil))' \
	  '    (while (< i 4000) (setq acc (cons (make-vector 12 i) acc)) (setq i (+ i 1))))' \
	  '  (let* ((i 0) (acc nil))' \
	  '    (while (< i 8000) (setq acc (cons (cons i i) acc)) (setq i (+ i 1))))' \
	  '  (garbage-collect)' \
	  '  (princ (format "ROUND %S\n" (nelisp--alloc-check-report))))' \
	  '(let ((r 0))' \
	  '  (while (< r $(STANDALONE_CHECKED_SOAK_ROUNDS))' \
	  '    (checked-soak-round)' \
	  '    (setq r (+ r 1))))' \
	  > target/checked-soak.el
	@bin=$(STANDALONE_BIN); \
	rounds_file=target/checked-soak-rounds.txt; \
	NELISP_ALLOC_CHECK=1 $$bin --load target/checked-soak.el 2>&1 \
	  | grep '^ROUND ' > $$rounds_file || true; \
	n=$$(wc -l < $$rounds_file | tr -d ' '); \
	if [ "$$n" -ne "$(STANDALONE_CHECKED_SOAK_ROUNDS)" ]; then \
	  echo "[checked-soak] FAIL: $$n of $(STANDALONE_CHECKED_SOAK_ROUNDS) round(s) reported -- the run died partway"; \
	  exit 1; \
	fi; \
	i=0; settled=""; last=""; lives=""; \
	while read -r _tag rest; do \
	  i=$$(( i + 1 )); \
	  set -- $$(echo "$$rest" | tr -d "()"); \
	  echo "[checked-soak] round $$i/$(STANDALONE_CHECKED_SOAK_ROUNDS) armed=$$2 verified-frees=$$5 violations=$$6 live-blocks=$$9"; \
	  if [ "$$1" != "1" ] || [ "$$2" != "1" ]; then \
	    echo "[checked-soak] FAIL round $$i: allocator not enabled+armed (enable=$$1 armed=$$2) -- the boot env probe did not fire, so nothing below was checked"; \
	    exit 1; \
	  fi; \
	  if [ "$${5:-0}" -le 0 ]; then \
	    echo "[checked-soak] FAIL round $$i: 0 frees verified -- the workload never reached the verifier"; \
	    exit 1; \
	  fi; \
	  if [ "$$6" != "0" ]; then \
	    echo "[checked-soak] FAIL round $$i: $$6 redzone violation(s), first bad header $$7, alloc site $$8"; \
	    exit 1; \
	  fi; \
	  lives="$$lives $$9"; \
	  if [ "$$i" -eq 2 ]; then settled=$$9; fi; \
	  last=$$9; \
	done < $$rounds_file; \
	rm -f $$rounds_file; \
	echo "[checked-soak] live-blocks per round:$$lives"; \
	if [ -z "$$settled" ]; then \
	  echo "[checked-soak] PASS (fewer than 2 rounds: no leak comparison possible)"; \
	  exit 0; \
	fi; \
	if [ "$$(( last - settled ))" -gt $(STANDALONE_CHECKED_SOAK_SLACK) ]; then \
	  echo "[checked-soak] FAIL: live blocks grew $$settled -> $$last after the first round, past the $(STANDALONE_CHECKED_SOAK_SLACK)-block slack (each round ends with garbage-collect, so this is retention rather than garbage)"; \
	  exit 1; \
	fi; \
	echo "[checked-soak] PASS"

# Doc 168 Phase 6 gate data collection (Doc 170 sections 3.3 / 5).  Runs
# the checked-allocator workloads with NELISP_ALLOC_CHECK=1 and appends
# one timestamped line per workload -- `<UTC-ISO8601> <workload>
# <(nelisp--alloc-check-report)>` -- to .alloc-check/reports.log.  The
# log is developer-local (gitignored, like .anvil-worklog): the Phase 6
# go/no-go gate ("start static checking only if > 50% of dynamically
# caught violations are statically decidable") is computed by a human
# from this data plus the nl-safe violation dumps
# (packages/nl-safe/src/nl-safe-report.el).  Workloads (alloc-site ids
# stamped via debug-switch 21 for provenance):
#   churn   (42) -- the standalone-reader-checked stress script
#   heavy   (43) -- larger cons/string/vector churn across several
#                   top-level form boundaries + explicit GCs
#   prelude (44) -- full scripts/nelisp-stdlib-prelude.el load
# A prebuilt target/nelisp[.exe] is used as-is (no rebuild); only when
# no binary exists does the standalone-reader build run.
#   make alloc-check-collect NELISP_STANDALONE_TARGET=windows-x86_64
alloc-check-collect: $(if $(wildcard target/nelisp target/nelisp.exe),,standalone-reader)
	@mkdir -p target .alloc-check
	@printf '%s\n' \
	  '(nelisp--debug-switch 21 42)' \
	  '(let* ((i 0) (acc nil)) (while (< i 200) (setq acc (cons i acc)) (setq i (+ i 1))) 0)' \
	  '(garbage-collect)' \
	  '(nelisp--alloc-check-report)' \
	  > target/alloc-check-collect-churn.el
	@printf '%s\n' \
	  '(nelisp--debug-switch 21 43)' \
	  '(let* ((i 0) (acc nil)) (while (< i 5000) (setq acc (cons i acc)) (setq i (+ i 1))) 0)' \
	  '(let* ((i 0) (acc nil)) (while (< i 5000) (setq acc (cons (make-string 64 65) acc)) (setq i (+ i 1))) 0)' \
	  '(garbage-collect)' \
	  '(let* ((i 0) (acc nil)) (while (< i 2000) (setq acc (cons (make-vector 16 i) acc)) (setq i (+ i 1))) 0)' \
	  '(let* ((i 0) (acc nil)) (while (< i 5000) (setq acc (cons (cons i i) acc)) (setq i (+ i 1))) 0)' \
	  '(garbage-collect)' \
	  '(nelisp--alloc-check-report)' \
	  > target/alloc-check-collect-heavy.el
	@# NB: no explicit (garbage-collect) after the prelude load -- under
	@# NELISP_ALLOC_CHECK=1 that combination dies silently (empty output,
	@# exit 0; binary of 2026-08-15, defect to peel separately).  The
	@# form-boundary GC already produces verified-free counts.
	@printf '%s\n' \
	  '(nelisp--debug-switch 21 44)' \
	  '(load "scripts/nelisp-stdlib-prelude.el")' \
	  '(nelisp--alloc-check-report)' \
	  > target/alloc-check-collect-prelude.el
	@bin=./target/nelisp; \
	case "$(NELISP_STANDALONE_TARGET)$$NELISP_STANDALONE_TARGET" in \
	  windows*) bin=./target/nelisp.exe;; \
	esac; \
	log=.alloc-check/reports.log; \
	for w in churn heavy prelude; do \
	  rep="$$(NELISP_ALLOC_CHECK=1 $$bin --load target/alloc-check-collect-$$w.el | tail -n 1)"; \
	  ts="$$(date -u +%Y-%m-%dT%H:%M:%SZ)"; \
	  case "$$rep" in \
	    "("*) printf '%s %s %s\n' "$$ts" "$$w" "$$rep" >> "$$log"; \
	          echo "[alloc-check-collect] $$w -> $$rep";; \
	    *) echo "[alloc-check-collect] FAIL: workload $$w produced no report -> $$rep"; \
	       exit 1;; \
	  esac; \
	done; \
	echo "[alloc-check-collect] appended 3 report line(s) to $$log"

# Doc 163 Phase C regression: `intern-soft' real soft-fail semantics.
# The base reader image has no stdlib prelude auto-loaded (plain --load
# only has the ~175 natively-dispatched builtins; `intern-soft' is a
# regular elisp function defined in lisp/nelisp-stdlib-misc.el), so the
# script `load's that file itself first -- same pattern
# `standalone-reader-prelude-equal-reload-smoke' uses for the stdlib
# prelude.  Asserts, in one --load: (1) a never-interned name misses
# (nil) BEFORE anything interns it; (2) once `intern' actually interns
# that same name, `intern-soft' now HITS and returns the (interned)
# symbol; (3) a DIFFERENT, still-never-interned name misses on TWO
# CONSECUTIVE `intern-soft' calls -- proving `intern-soft' itself has no
# interning side effect (a bug in `nl_intern_lookup' that accidentally
# inserted on a miss would turn the second nil into a symbol).
# Deep recursion must SIGNAL, not die.  The `excessive-lisp-nesting'
# guard was always implemented, but rec_max sat above the real native
# ceiling, so recursion past it was a silent exit 127 instead of a
# catchable error (2026-08-16: measured ceiling ~136k rec levels
# against a comment claiming ~404k).  Doc 152 Stage 3's root frames add
# a nearer fixed-region ceiling (this probe measures root-depth=3N+6),
# so rec_max is now calibrated below both limits.  This asserts the guard
# fires and the process survives, so a future rec_max or frame-size change
# cannot quietly restore the silent death.
standalone-reader-recursion-guard-smoke: standalone-reader
	@bin=$(STANDALONE_BIN); \
	timeout 180 $$bin --load tools/recursion-guard-smoke.el

# The environment, read back through `getenv' from a child that was given
# one.  Written on wip/uncommitted-2026-08-18 by whoever first noticed that
# `getenv' answered nil, and brought over here with the fix rather than
# rewritten -- a second smoke asking the same question would be a second
# owner of the answer.
#
# It answered nil because nothing ever filled the list `getenv' reads: three
# implementations of it in this tree, all reading an in-process alist, and no
# startup step connecting that alist to the process.  Everything keyed on the
# environment was therefore dead, the native-exec cache root among them --
# it fell past XDG_CACHE_HOME and HOME to /tmp on every run.
.PHONY: standalone-reader-getenv-smoke
# The environment the parent set must reach `getenv' inside the standalone
# (fix/windows-env-inherit).  Two variables, because the two halves fail
# differently: a plain value proves inheritance at all, and a PATH-SHAPED
# value proves the value arrives byte for byte rather than mangled.
#
# It used to assert on `HOME=/tmp/...' and was RED on every windows-native
# run for a reason that had nothing to do with the runtime: MSYS2 rewrites
# path-shaped environment variables when it execs a native PE, so the binary
# saw `C:\Users\...\Temp\nelisp-getenv-smoke-home' and the gate compared it
# against a string the shell had never actually passed.  Measured while
# fixing it: `HOME' is rewritten even with `MSYS2_ENV_CONV_EXCL' set (MSYS
# special-cases it), an ordinary variable is rewritten without it, and an
# ordinary variable named in `MSYS2_ENV_CONV_EXCL' round-trips exactly.  So
# the path-shaped half uses its own variable under that exclusion, and
# `HOME' is now only asserted to be inherited and non-empty -- which is what
# `expand-file-name "~"' actually needs from it, and is true on every host.
# `MSYS2_ENV_CONV_EXCL' is an unused variable everywhere else.
standalone-reader-getenv-smoke: standalone-reader
	@mkdir -p target
	@printf '%s\n' '(list (and (stringp (getenv "HOME")) (> (length (getenv "HOME")) 0)) (getenv "NELISP_ENV_SMOKE_DIR") (getenv "NELISP_ENV_SMOKE"))' > target/standalone-reader-getenv-smoke.el
	@out="$$(MSYS2_ENV_CONV_EXCL=NELISP_ENV_SMOKE_DIR NELISP_ENV_SMOKE_DIR=/tmp/nelisp-getenv-smoke-home NELISP_ENV_SMOKE=nelisp-getenv-smoke $(STANDALONE_BIN) --load target/standalone-reader-getenv-smoke.el)"; \
	if [ "$$out" = '(t "/tmp/nelisp-getenv-smoke-home" "nelisp-getenv-smoke")' ]; then \
	  echo "[standalone-reader-getenv-smoke] PASS: --load -> $$out"; \
	else \
	  echo "[standalone-reader-getenv-smoke] FAIL: --load -> $$out (expected (t \"/tmp/nelisp-getenv-smoke-home\" \"nelisp-getenv-smoke\"))"; \
	  exit 1; \
	fi

standalone-reader-intern-soft-smoke: standalone-reader
	@mkdir -p target
	@printf '%s\n' \
	  '(load "lisp/nelisp-stdlib-misc.el")' \
	  '(list (intern-soft "nelisp-doc163-fresh-a") (progn (intern "nelisp-doc163-fresh-a") (intern-soft "nelisp-doc163-fresh-a")) (intern-soft "nelisp-doc163-fresh-b") (intern-soft "nelisp-doc163-fresh-b"))' \
	  > target/standalone-reader-intern-soft-smoke.el
	@out="$$($(STANDALONE_ULIMIT); timeout $(STANDALONE_SMOKE_TIMEOUT) $(STANDALONE_BIN) --load target/standalone-reader-intern-soft-smoke.el)"; \
	if [ "$$out" = "(nil nelisp-doc163-fresh-a nil nil)" ]; then \
	  echo "[standalone-reader-intern-soft-smoke] PASS: -> $$out"; \
	else \
	  echo "[standalone-reader-intern-soft-smoke] FAIL: -> $$out (expected (nil nelisp-doc163-fresh-a nil nil))"; \
	  exit 1; \
	fi

# Reader number-token classification, against a table of what host Emacs
# answers.  A token starting with a digit is a number only when it matches
# integer or float syntax exactly; `7.1.4' is a symbol there.
#
# Measured 2026-08-19: the native lexer counted WHETHER a dot appeared, not
# how many, so `7.1.4' lexed as a float, the parser gave up on it, and
# `nelisp--read-all-from-string-native' returned the forms it already had --
# indistinguishable from end of input.  `src/nelisp-cc-arm64.el' stopped
# loading at its `:phase '7.1.4', `load' returned t, the file's `(provide ...)'
# never ran, and the native compiler was reported unavailable.  The stdlib
# prelude decides the same question separately and was wrong differently:
# it called the token a number and `string-to-number' answered 7.  The smoke
# checks both readers, because there are two of them.
.PHONY: standalone-reader-number-token-smoke
standalone-reader-number-token-smoke: standalone-reader
	@out="$$($(STANDALONE_ULIMIT); timeout $(STANDALONE_SMOKE_TIMEOUT) $(STANDALONE_BIN) --load scripts/standalone-number-token-smoke.el)"; \
	echo "$$out"; \
	if echo "$$out" | grep -q 'NUMBER-TOKEN-SMOKE cases=16 mismatches=0'; then \
	  echo "[standalone-reader-number-token-smoke] PASS"; \
	else \
	  echo "[standalone-reader-number-token-smoke] FAIL"; \
	  exit 1; \
	fi

# Doc 190 Phase A (bignums): reading (a literal past most-positive-fixnum/
# most-negative-fixnum promotes to a Sexp tag-13 Bignum instead of
# wrapping), printing (prin1/read round-trip), comparison (eql/=/</> across
# bignum-bignum and bignum-fixnum), integerp/numberp/type-of, the
# deliberate non-boundary (arithmetic still signals overflow-error, no
# promotion), and a GC stress round.  ulimit mirrors the number-token
# smoke's own bound -- generous for this workload (500 short-lived
# bignums), tight enough to fail loudly on a real leak.
.PHONY: standalone-reader-bignum-smoke
standalone-reader-bignum-smoke: standalone-reader
	@out="$$($(STANDALONE_ULIMIT); timeout $(STANDALONE_SMOKE_TIMEOUT) $(STANDALONE_BIN) --load scripts/standalone-bignum-smoke.el)"; \
	echo "$$out"; \
	if echo "$$out" | grep -q 'BIGNUM-SMOKE cases=54 mismatches=0'; then \
	  echo "[standalone-reader-bignum-smoke] PASS"; \
	else \
	  echo "[standalone-reader-bignum-smoke] FAIL"; \
	  exit 1; \
	fi

# Doc 163 Phase C regression: the EXACT Gnus message.el `cited-text-face'
# discovery-loop shape that hung before this fix (see docs/design/163-
# magit-bundle-intern-soft-hang.org).  Pre-interns levels 1-4 (mirroring
# message.el defining `message-cited-text-1' .. `-4' a few lines above
# `message-font-lock-keywords' in the real bundle), then runs the loop
# unmodified.  Before the fix `intern-soft' never soft-failed, so this
# looped forever allocating a fresh interned name every iteration until
# `ulimit -v' was exhausted (rc=88, ~40s in the full-bundle repro).  After
# the fix the loop terminates the moment level 5 (never interned) is
# probed, exactly matching real Emacs (`maxlevel' ends at 5).  Bounded by
# both `ulimit -v' (4 GiB) and `timeout' so a regression fails loudly and
# quickly instead of hanging the test suite.
standalone-reader-intern-soft-loop-smoke: standalone-reader
	@mkdir -p target
	@printf '%s\n' \
	  '(load "lisp/nelisp-stdlib-misc.el")' \
	  '(intern "message-cited-text-1")' \
	  '(intern "message-cited-text-2")' \
	  '(intern "message-cited-text-3")' \
	  '(intern "message-cited-text-4")' \
	  '(let ((maxlevel 1) (cited-text-face t)) (while (setq cited-text-face (intern-soft (format "message-cited-text-%d" maxlevel))) (setq maxlevel (1+ maxlevel))) maxlevel)' \
	  > target/standalone-reader-intern-soft-loop-smoke.el
	@out="$$($(STANDALONE_ULIMIT); timeout $(STANDALONE_SMOKE_TIMEOUT) $(STANDALONE_BIN) --load target/standalone-reader-intern-soft-loop-smoke.el)"; \
	if [ "$$out" = "5" ]; then \
	  echo "[standalone-reader-intern-soft-loop-smoke] PASS: -> $$out"; \
	else \
	  echo "[standalone-reader-intern-soft-loop-smoke] FAIL: -> $$out (expected 5)"; \
	  exit 1; \
	fi

# Regression smoke for the native `format' directive arms in the reader's
# m5_fmt_loop (scripts/nelisp-standalone-build.el).  Before the Doc147 fix,
# %i/%X/%o/%c fell through to the default arm: emitting "%X" literally AND
# failing to consume the argument.  Asserts all four now render correctly
# alongside the pre-existing %d/%x.
standalone-reader-fmt-smoke: standalone-reader
	@mkdir -p target
	@printf '%s\n' '(format "i=%i x=%x X=%X o=%o c=%c" 42 255 255 64 65)' > target/standalone-reader-fmt-smoke.el
	@out="$$($(STANDALONE_BIN) --load target/standalone-reader-fmt-smoke.el)"; \
	if [ "$$out" = '"i=42 x=ff X=FF o=100 c=A"' ]; then \
	  echo "[standalone-reader-fmt-smoke] PASS: --load -> $$out"; \
	else \
	  echo "[standalone-reader-fmt-smoke] FAIL: --load -> $$out"; \
	  exit 1; \
	fi

# Regression smoke for the stdlib prelude's `equal' idempotent native-capture
# guard (fix 4503ba28, "make nelisp--native-X captures idempotent across
# re-load").  The prelude is baked into the reader image AND can be re-loaded
# at runtime (scripts/nelisp-stdlib-prelude.el, Doc 22 A3): before that fix, a
# second `(load ...)' of the prelude re-captured the ALREADY-INSTALLED elisp
# `equal' wrapper into `nelisp--native-equal', turning its native delegate
# into a self-call and breaking every subsequent `equal' call.  Investigated
# 2026-07: this exact regression was independently observed against a STALE
# vendored nelisp copy (nelisp-emacs-lib's vendor/nelisp, which predates this
# fix); it does not reproduce against this reader.  This smoke pins that down
# so a future prelude edit cannot silently reintroduce it.
standalone-reader-prelude-equal-reload-smoke: standalone-reader
	@mkdir -p target
	@printf '%s\n' \
	  '(load "scripts/nelisp-stdlib-prelude.el")' \
	  '(list (equal 1 1) (equal 1 2) (equal (list 1 2 3) (list 1 2 3)) (equal [1 2 3] [1 2 3]))' \
	  > target/standalone-reader-prelude-equal-reload-smoke.el
	@out="$$($(STANDALONE_BIN) --load target/standalone-reader-prelude-equal-reload-smoke.el)"; \
	if [ "$$out" = "(t nil t t)" ]; then \
	  echo "[standalone-reader-prelude-equal-reload-smoke] PASS: -> $$out"; \
	else \
	  echo "[standalone-reader-prelude-equal-reload-smoke] FAIL: -> $$out (expected (t nil t t))"; \
	  exit 1; \
	fi

standalone-reader-declare-strip-smoke: standalone-reader
	@mkdir -p target
	@printf '%s\n' \
	  '(defmacro ds-m1 (x) (declare (debug (form))) (list (quote quote) x))' \
	  '(defmacro ds-m2 (x) "doc" (declare (debug (form))) (declare (indent 1)) (list (quote quote) x))' \
	  '(defmacro ds-m3 (x) (declare (debug (form))) "doc" (list (quote quote) x))' \
	  '(defun ds-f1 () "doc" (declare (indent 0)) (interactive) (+ 41 1))' \
	  '(defun ds-f2 () (declare (indent 0)))' \
	  '(let ((fn (symbol-function (quote ds-f1)))) (list (ds-m1 foo) (ds-m2 bar) (ds-m3 baz) (ds-f1) (equal (car (cdr (cdr (cdr fn)))) (quote (interactive))) (ds-f2)))' \
	  > target/standalone-reader-declare-strip-smoke.el
	@out="$$($(STANDALONE_BIN) --repl --no-prompt < target/standalone-reader-declare-strip-smoke.el | tail -1)"; \
	if [ "$$out" = "(foo bar baz 42 t nil)" ]; then \
	  echo "[standalone-reader-declare-strip-smoke] PASS: -> $$out"; \
	else \
	  echo "[standalone-reader-declare-strip-smoke] FAIL: -> $$out (expected (foo bar baz 42 t nil))"; \
	  exit 1; \
	fi

# Regression smoke for user-defined macros whose expansion-producing body is a
# backquote template, INCLUDING one macro's body invoking ANOTHER backquote
# macro through `,@' splicing -- the shape of the Track H
# `define-derived-mode' substrate bridge (nelisp-emacs-lib commit bd6d06d,
# Doc 33 section 8 item 221) before that commit rewrote it backquote-free.
# Feeds the REPL's one-physical-line-per-form input (this reader's --repl
# loop reads and evaluates exactly one physical line at a time with no
# continuation; every form below is written on its own physical line for
# that reason -- a multi-physical-line form is silently dropped, which is
# the REPL's documented contract, not a defect).  Investigated 2026-07: with
# input correctly normalized to one physical line per form, nested
# macro-calling-macro backquote templates (including a computed `,'
# expression, `,@' splicing, and a nil-vs-non-nil "parent" argument mirroring
# define-derived-mode's own shape) fully expand and evaluate on this reader;
# this smoke pins that down so a future reader change cannot silently
# reintroduce a nested-macro/backquote regression.
standalone-reader-nested-backquote-macro-smoke: standalone-reader
	@mkdir -p target
	@printf '%s\n' \
	  '(defmacro nbm-inner (child parent doc &rest body) `(progn (defvar ,(intern (concat (symbol-name child) "-hook")) nil) (put (quote ,child) (quote nbm-test-parent) (quote ,parent)) (defun ,child () ,doc (interactive) ,@body 42) (quote ,child)))' \
	  '(defmacro nbm-outer (child parent &optional doc &rest body) `(nbm-inner ,child ,parent ,doc ,@body))' \
	  '(nbm-outer nbm-child-a nil "control: nil parent" (setq nbm-var-a 1))' \
	  '(nbm-outer nbm-child-b nbm-child-a "target: non-nil parent" (setq nbm-var-b 2))' \
	  '(list (fboundp (quote nbm-child-a)) (fboundp (quote nbm-child-b)) (nbm-child-a) (nbm-child-b) (get (quote nbm-child-b) (quote nbm-test-parent)))' \
	  > target/standalone-reader-nested-backquote-macro-smoke.el
	@out="$$($(STANDALONE_BIN) --repl --no-prompt < target/standalone-reader-nested-backquote-macro-smoke.el | tail -1)"; \
	if [ "$$out" = "(t t 42 42 nbm-child-a)" ]; then \
	  echo "[standalone-reader-nested-backquote-macro-smoke] PASS: -> $$out"; \
	else \
	  echo "[standalone-reader-nested-backquote-macro-smoke] FAIL: -> $$out (expected (t t 42 42 nbm-child-a))"; \
	  exit 1; \
	fi

# Regression smoke for the exact backquote shape underlying vendor
# `define-derived-mode' (emacs-lisp/derived.el): a wrapper form whose
# children are (1) a single-comma element `(,(or parent 'base-fn))', (2)
# plain siblings, (3) a `,(when PARENT `(progn ...))' branch that is nil
# on the PARENT=nil arm and a two-level-deep nested-backquote-through-comma
# `(progn ...)' on the non-nil arm, followed by (4) more plain sibling
# forms (`use-local-map'/`set-syntax-table'/`setq'-shaped calls in the real
# macro).  Investigated 2026-07 (Doc merge 39e45d90 follow-up): a session
# reported that expanding UNMODIFIED vendor derived.el's
# `define-derived-mode' "misfolds" those trailing sibling forms into the
# tail of the preceding form.  Root-caused: NOT a backquote/macroexpansion
# defect.  Reproduced the exact vendor macro (byte-identical text) against
# this reader with an accurate stub environment (real Emacs
# `define-abbrev-table' side-effects a `set' on its table symbol before a
# self-referencing `defvar' initializer reads it back; an earlier ad hoc
# stub used during triage returned nil without binding anything).  With an
# accurate stub, BOTH the printed `macroexpand-1' structure (siblings
# intact, no misfold, for both the nil-parent and chained non-nil-parent
# case) AND the resulting mode functions (`fboundp'/`commandp' both t) are
# fully correct end-to-end -- matching real Emacs.  The original inaccurate
# stub triggered a SEPARATE, genuine defect (silent, uncatchable-by-
# `condition-case' abandonment of the rest of a compound top-level form
# after an unbound-variable reference, with no diagnostic -- see
# FINDINGS.md) which was mistaken for a structural misfold because later
# sibling forms in the same top-level `progn' never ran.  That defect is
# real but lives in the core eval/signal-flag substrate (`nl_eval_inner' /
# `nelisp_eval_call' and the M6 stash flag @268435472 in
# scripts/nelisp-standalone-build.el), not in the backquote engine, and is
# out of scope for a DSL-level fix; this smoke pins down the part that IS
# correct (the backquote engine) so it cannot silently regress.
standalone-reader-derived-mode-shape-smoke: standalone-reader
	@mkdir -p target
	@printf '%s\n' \
	  '(defmacro ddm-shape (child parent) (let ((map (intern (concat (symbol-name child) "-map")))) `(wrapper-hooks (,(or parent (quote base-fn))) (setq major-mode (quote ,child)) ,(when parent `(progn (setup-parent (quote ,parent)) ,(when t `(let ((p (parent-of ,map))) (maybe-set-parent ,map p))))) (use-local-map ,map) (set-syntax-table ,map) (setq local-abbrev-table ,map))))' \
	  '(list (equal (macroexpand-1 (quote (ddm-shape ddm-child-a nil))) (quote (wrapper-hooks (base-fn) (setq major-mode (quote ddm-child-a)) nil (use-local-map ddm-child-a-map) (set-syntax-table ddm-child-a-map) (setq local-abbrev-table ddm-child-a-map)))) (equal (macroexpand-1 (quote (ddm-shape ddm-child-b ddm-parent))) (quote (wrapper-hooks (ddm-parent) (setq major-mode (quote ddm-child-b)) (progn (setup-parent (quote ddm-parent)) (let ((p (parent-of ddm-child-b-map))) (maybe-set-parent ddm-child-b-map p))) (use-local-map ddm-child-b-map) (set-syntax-table ddm-child-b-map) (setq local-abbrev-table ddm-child-b-map)))))' \
	  > target/standalone-reader-derived-mode-shape-smoke.el
	@out="$$($(STANDALONE_BIN) --load target/standalone-reader-derived-mode-shape-smoke.el)"; \
	if [ "$$out" = "(t t)" ]; then \
	  echo "[standalone-reader-derived-mode-shape-smoke] PASS: -> $$out"; \
	else \
	  echo "[standalone-reader-derived-mode-shape-smoke] FAIL: -> $$out (expected (t t))"; \
	  exit 1; \
	fi

# Regression for the `pcase' root cause behind the fix/reader-backquote-macro
# investigation (magit #17 M2 blocker, nelisp-emacs-lib Doc 33 item 239): a
# `(quote DATUM)' pcase pattern holding a COMPOUND datum (here a 2-element
# list) must match by `equal' (structural), not `eq' (identity).  `eq' only
# happens to work for quoted symbols/keywords (interned, so `eq'-comparable);
# a freshly-consed runtime list is never `eq' to an equal-shaped quoted
# literal, so a clause like `('(t t) ...)' silently never wins and pcase
# falls through to the next, less-specific clause instead -- exactly the
# defect vendor cond-let.el's `cond-let--prepare-clauses' hits when its
# `(pcase (list ...) ('(t t) 'cond-let--when-let*) (`(t ,_) 'cond-let--when-let)
# ...)' dispatch picks the wrong helper macro and produces `void-variable: x'
# (see the fuller end-to-end shape in
# standalone-reader-cond-let-shape-smoke below).
standalone-reader-pcase-quote-literal-smoke: standalone-reader
	@mkdir -p target
	@printf '%s\n' \
	  '(list (pcase (list t t) ((quote (t t)) (quote AA)) (`(t ,_) (quote AB)) (`(nil ,_) (quote BB))) (pcase (list nil t) ((quote (nil t)) (quote BA)) (`(t ,_) (quote AB)) (`(nil ,_) (quote BB))))' \
	  > target/standalone-reader-pcase-quote-literal-smoke.el
	@out="$$($(STANDALONE_BIN) --load target/standalone-reader-pcase-quote-literal-smoke.el)"; \
	if [ "$$out" = "(AA BA)" ]; then \
	  echo "[standalone-reader-pcase-quote-literal-smoke] PASS: -> $$out"; \
	else \
	  echo "[standalone-reader-pcase-quote-literal-smoke] FAIL: -> $$out (expected (AA BA))"; \
	  exit 1; \
	fi

# Regression for the M6 catch/throw tag-match bug (magit #17 M2 blocker):
# `nl_ct_catch_check_tag' (scripts/nelisp-standalone-build.el) used to reuse
# `nelisp_eq_symbol' for the catch/throw tag comparison, but that primitive
# tag-checks BOTH operands as `Sexp::Symbol' (tag 4) and returns "not equal"
# whenever either side isn't a Symbol box.  `t' and `nil' self-evaluate to
# the dedicated `Sexp::T' (tag 1) / `Sexp::Nil' (tag 0) singletons, NOT
# Symbol boxes, so `(catch t (throw t ...))' / `(catch nil (throw nil
# ...))' always mismatched and fell through to `no-catch' -- exactly the
# control-flow idiom vendor llama.el's `llama--collect'/`llama--fontify'
# use internally (reached from magit/transient via the `##' macro).  Covers
# t tag, nil tag, an ordinary symbol tag (pre-existing-working baseline),
# throw-less catch, a same-tag nested catch, and a mismatched-tag nested
# catch (inner `t' catch must NOT swallow an outer-bound `nil' throw).
standalone-reader-catch-throw-tag-smoke: standalone-reader
	@mkdir -p target
	@printf '%s\n' \
	  '(list (catch t (throw t (quote a))) (catch nil (throw nil (quote b))) (catch (quote tag) (throw (quote tag) (quote c))) (catch t 42) (catch nil (catch t (throw t (quote inner)))) (catch nil (catch t (throw nil (quote outer)))))' \
	  > target/standalone-reader-catch-throw-tag-smoke.el
	@out="$$($(STANDALONE_BIN) --load target/standalone-reader-catch-throw-tag-smoke.el)"; \
	if [ "$$out" = "(a b c 42 inner outer)" ]; then \
	  echo "[standalone-reader-catch-throw-tag-smoke] PASS: -> $$out"; \
	else \
	  echo "[standalone-reader-catch-throw-tag-smoke] FAIL: -> $$out (expected (a b c 42 inner outer))"; \
	  exit 1; \
	fi

# End-to-end regression for the same root cause, shaped exactly like vendor
# cond-let.el's `cond-let--prepare-clauses' / `cond-let--when-let*' /
# `cond-let--when-let' (magit #17 M2 blocker; nelisp-emacs-lib Doc 33 item
# 239's minimal repro `(cond-let* ([x 1] [x (+ x 1)] x) (t 99))' raised
# `void-variable: x' against the pre-fix reader).  Self-contained (does NOT
# load vendor/cond-let.el or anything from nelisp-emacs-lib): re-derives just
# enough of the same two-part mechanism using only what this reader's own
# baked-in prelude already provides (`pcase' / `pcase-let' / backquote /
# `catch'+`throw') --
#   (1) a clause-preparation helper that builds its expansion via nested
#       backquote/`,@'-splicing and dispatches between two sibling
#       backquote-bodied macros through a `pcase' whose patterns mix a
#       quoted-list literal (`'(t 2)') with backquote patterns (`` `(t ,_)'');
#   (2) `my-when-let*' (sequential, `let*'-based -- correct for chained
#       bindings that reference an earlier binding of the SAME name) versus
#       `my-when-let' (parallel, `let'-based -- wrong for that shape, and
#       will itself raise `void-variable' if ever mis-selected again).
# Before the fix, the quote-literal clause never matched (silently, `eq'
# instead of `equal'), so the dispatch always picked `my-when-let' and the
# second binding's `(+ x 1)' referenced `x' before it was bound ->
# `void-variable: x'.  After the fix, `my-when-let*' is correctly selected
# and the whole thing evaluates to 2 (1, then (and 1 (+ 1 1)) = 2), matching
# real Emacs's actual `cond-let*' semantics for the same shape.
standalone-reader-cond-let-shape-smoke: standalone-reader
	@mkdir -p target
	@printf '%s\n' \
	  '(defun my-macroexp-progn (forms) (if (cdr forms) (cons (quote progn) forms) (car forms)))' \
	  '(defun my-prepare-varlist (varlist) (let (prevvar) (list (mapcar (lambda (binding) (pcase-let ((`(,var ,form) binding)) (prog1 (if prevvar `(,var (and ,prevvar ,form)) (list var form)) (setq prevvar var)))) varlist) prevvar)))' \
	  '(defmacro my-when-let* (varlist bodyform) (let* ((res (my-prepare-varlist varlist)) (newvarlist (nth 0 res)) (lastvar (nth 1 res))) `(let* ,newvarlist (when ,lastvar ,bodyform))))' \
	  '(defmacro my-when-let (varlist bodyform) `(let ,varlist (when ,(car (car (last varlist))) ,bodyform)))' \
	  '(defun my-prepare-clauses (sequential clauses) (let (body) (dolist (clause (reverse clauses)) (let (varlist) (while (vectorp (car clause)) (push (append (pop clause) nil) varlist)) (push (if varlist (let ((macro-sym (pcase (list (and body t) (and sequential (length (reverse varlist)))) ((quote (t 2)) (quote my-when-let*)) (`(t ,_) (quote my-when-let)) ((quote (nil 2)) (quote my-when-let*)) (`(nil ,_) (quote my-when-let))))) `(,macro-sym ,(reverse varlist) ,(if body `(throw (quote my-cond-let-tag) ,(my-macroexp-progn clause)) (my-macroexp-progn clause)))) (my-macroexp-progn clause)) body))) body))' \
	  '(defmacro my-cond-let* (&rest clauses) `(catch (quote my-cond-let-tag) ,@(my-prepare-clauses t clauses)))' \
	  '(my-cond-let* ([x 1] [x (+ x 1)] x) (t 99))' \
	  > target/standalone-reader-cond-let-shape-smoke.el
	@out="$$($(STANDALONE_BIN) --load target/standalone-reader-cond-let-shape-smoke.el)"; \
	if [ "$$out" = "2" ]; then \
	  echo "[standalone-reader-cond-let-shape-smoke] PASS: -> $$out"; \
	else \
	  echo "[standalone-reader-cond-let-shape-smoke] FAIL: -> $$out (expected 2)"; \
	  exit 1; \
	fi

# fix/small-primitives-parity: `mod' silently returned 0 for any float
# operand pair (e.g. `(mod 5.5 2)' => 0.0 instead of 1.5) because the
# quotient it computed the remainder from (`scripts/nelisp-stdlib-prelude.el')
# used this reader's TRUE (non-truncating) float `/', so `b * (a/b)'
# collapsed back to exactly `a'.  Covers all 4 sign combinations for
# float/float and mixed int/float, the pre-existing (and intentionally
# unchanged) all-integer floor-mod path, and zero-divisor semantics: NaN
# (via `floatp') when a float operand is involved vs. a caught `error' when
# both operands are integers -- matching host Emacs on both counts.
#
# Each check reduces to a boolean rather than returning the raw `mod'
# result directly: the `--load' value printer mis-renders a Float nested
# inside a list as `#<object>' (a separate, pre-existing printer quirk,
# unrelated to this fix -- `--eval' on the same expressions prints the
# floats correctly), so comparing with `=' and collecting `t'/`nil' side-
# steps that entirely.
standalone-reader-mod-float-smoke: standalone-reader
	@mkdir -p target
	@printf '%s\n' \
	  '(list (= (mod 5.5 2) 1.5) (= (mod -5.5 2) 0.5) (= (mod 5.5 -2) -0.5) (= (mod -5.5 -2) -1.5) (= (mod 5 2.0) 1.0) (= (mod -5 2.0) 1.0) (= (mod 5 -2.0) -1.0) (= (mod -5 -2.0) -1.0) (= (mod 5.5 2.5) 0.5) (= (mod -5.5 2.5) 2.0) (= (mod 5.5 -2.5) -2.0) (= (mod -5.5 -2.5) -0.5) (= (mod 7 -3) -2) (= (mod -7 3) 2) (= (mod 7 3) 1) (= (mod -7 -3) -1) (= (mod 10 3) 1) (floatp (mod 5.5 0.0)) (floatp (mod 5.0 0)) (eq (condition-case nil (progn (mod 5 0) (quote no-error)) (error (quote caught-error))) (quote caught-error)) (= (+ 1.5 2.5) 4.0) (= (- 1.5 0.5) 1.0) (= (* 2.0 3.0) 6.0) (= (float 3) 3.0) (= (/ 1.0 3.0) 0.3333333333333333) (= (/ 10.0 2.0) 5.0))' \
	  > target/standalone-reader-mod-float-smoke.el
	@out="$$(NELISP_STANDALONE_TARGET=$(STANDALONE_GATE_TARGET) $(EMACS) --batch -Q -L lisp -L src -L scripts \
	  -l nelisp-standalone-build \
	  --eval '(if (nelisp-standalone--target-runnable-on-host-p) (princ (nelisp-standalone--output-path t)) (kill-emacs 77))')"; \
	rc=$$?; \
	if [ $$rc -eq 77 ]; then \
	  echo "[standalone-reader-mod-float-smoke] SKIP: target $(STANDALONE_GATE_TARGET) cannot run on this host"; \
	  exit 0; \
	elif [ $$rc -ne 0 ]; then \
	  exit $$rc; \
	fi; \
	out="$$($$out --load target/standalone-reader-mod-float-smoke.el)"; \
	if [ "$$out" = "(t t t t t t t t t t t t t t t t t t t t t t t t t t)" ]; then \
	  echo "[standalone-reader-mod-float-smoke] PASS: -> $$out"; \
	else \
	  echo "[standalone-reader-mod-float-smoke] FAIL: -> $$out"; \
	  exit 1; \
	fi

# fix/small-primitives-parity: `match-data' / `save-match-data' were
# entirely unimplemented (`fboundp' nil) even though `match-beginning' /
# `match-end' / `match-string' already worked off the same `nlre--last-caps'
# vector (Doc 143's pure-elisp regexp matcher).  Covers: `match-data'
# flattening to the host (BEG0 END0 BEG1 END1 ...) shape with `nil nil' for
# a non-participating group, and `save-match-data' isolating a nested
# `string-match' from the caller's match state -- including when the body
# signals an error, via `unwind-protect'.
standalone-reader-match-data-smoke: standalone-reader
	@mkdir -p target
	@printf '%s\n' \
	  '(list (progn (string-match "\\(a\\)\\(b\\)" "xabZ") (match-data)) (progn (string-match "a" "xaZ") (save-match-data (string-match "Z" "xaZ")) (match-beginning 0)) (progn (string-match "a" "xaZ") (condition-case nil (save-match-data (string-match "Z" "xaZ") (error "boom")) (error nil)) (match-beginning 0)))' \
	  > target/standalone-reader-match-data-smoke.el
	@out="$$($(STANDALONE_BIN) --load target/standalone-reader-match-data-smoke.el)"; \
	if [ "$$out" = "((1 3 1 2 2 3) 1 1)" ]; then \
	  echo "[standalone-reader-match-data-smoke] PASS: -> $$out"; \
	else \
	  echo "[standalone-reader-match-data-smoke] FAIL: -> $$out"; \
	  exit 1; \
	fi

# fix/small-primitives-parity: `current-time' was void-function.  Minimal
# polyfill derived from the already-working `float-time': decomposes the
# epoch-seconds double into host Emacs's (HIGH LOW USEC PSEC) shape (PSEC
# always 0, see the defun's comment for why).  Asserts HIGH*65536+LOW
# reconstructs a whole-second count inside two `float-time' reads bracketing
# `current-time', and that USEC/PSEC are in-range.  Comparing against one
# later read raced at a second boundary: measured 0/170 failures unloaded and
# 1/300 under CPU load, with only this equality false.  The closed interval is
# normally one exact second; if a boundary is crossed, it contains precisely
# the possible seconds at which the bracketed `current-time' read occurred.
standalone-reader-current-time-smoke: standalone-reader
	@mkdir -p target
	@printf '%s\n' \
	  '(let* ((before (float-time)) (tm (current-time)) (after (float-time)) (hi (nth 0 tm)) (lo (nth 1 tm)) (us (nth 2 tm)) (ps (nth 3 tm)) (secs (+ (* hi 65536) lo))) (list (= (length tm) 4) (and (>= secs (floor before)) (<= secs (floor after))) (and (>= us 0) (< us 1000000)) (= ps 0)))' \
	  > target/standalone-reader-current-time-smoke.el
	@out="$$($(STANDALONE_BIN) --load target/standalone-reader-current-time-smoke.el)"; \
	if [ "$$out" = "(t t t t)" ]; then \
	  echo "[standalone-reader-current-time-smoke] PASS: -> $$out"; \
	else \
	  echo "[standalone-reader-current-time-smoke] FAIL: -> $$out"; \
	  exit 1; \
	fi

# Phase 47.D Step C / D1 / F1: runtime FFI smoke.  Linux builds its opt-in
# dynamic reader (NELISP_READER_DYNAMIC=1) with the full table; windows-x86_64
# builds its ordinary PE reader with the UCRT-mapped libc/libm subset.  Covers:
# libc int->int (toupper), libm double->double f64
# marshalling (sqrt/pow/ldexp(f64+i64)/hypot via XMM args + xmm0 return), GnuTLS
# const char* return (D1: gnutls_check_version), FreeType pointer-out-params (F1:
# FT_Init_FreeType + FT_Library_Version).  Self-contained (does NOT depend on the default static
# `standalone-reader').  GnuTLS/FreeType assertions are version-prefix checks so
# they survive minor library bumps.
standalone-reader-ffi-smoke:
	@mkdir -p target
ifeq ($(STANDALONE_GATE_TARGET),windows-x86_64)
	@$(EMACS) --batch -Q -L lisp -L src -L scripts \
	  --eval '(setq load-prefer-newer t)' \
	  -l nelisp-standalone-build -f nelisp-standalone-build-reader
else
	@NELISP_READER_DYNAMIC=1 $(EMACS) --batch -Q -L lisp -L src -L scripts \
	  --eval '(setq load-prefer-newer t)' \
	  -l nelisp-standalone-build -f nelisp-standalone-build-reader
endif
	@chmod +x target/nelisp
	@printf '%s\n' '(nl-ffi-call "toupper" 97)' > target/standalone-reader-ffi-smoke.el
	@out="$$($(STANDALONE_BIN) --load target/standalone-reader-ffi-smoke.el)"; \
	if [ "$$out" = "65" ]; then \
	  echo "[ffi-smoke libc] PASS: (nl-ffi-call \"toupper\" 97) -> $$out"; \
	else \
	  echo "[ffi-smoke libc] FAIL: -> $$out (expected 65)"; exit 1; \
	fi
	@if [ "$(STANDALONE_GATE_TARGET)" = "windows-x86_64" ]; then \
	  printf '%s\n' '(nl-ffi-call "toupper" -1)' > target/standalone-reader-ffi-s32.el; \
	  out="$$($(STANDALONE_BIN) --load target/standalone-reader-ffi-s32.el)"; \
	  if [ "$$out" = "-1" ]; then \
	    echo "[ffi-smoke Win64 s32] PASS: toupper(EOF) -> $$out"; \
	  else \
	    echo "[ffi-smoke Win64 s32] FAIL: -> $$out (expected -1)"; exit 1; \
	  fi; \
	fi
	@: 'f64 FFI: double args/return through XMM.  The reader top-level printer'
	@: 'renders any float (even a literal) as #<object>, so assert on numeric'
	@: 'equality (= -> t) rather than the printed form.'
	@printf '%s\n' '(= (nl-ffi-call "sqrt" 4.0) 2.0)' > target/standalone-reader-ffi-f64.el
	@out="$$($(STANDALONE_BIN) --load target/standalone-reader-ffi-f64.el)"; \
	if [ "$$out" = "t" ]; then \
	  echo "[ffi-smoke f64 libm] PASS: (= (nl-ffi-call \"sqrt\" 4.0) 2.0) -> $$out"; \
	else \
	  echo "[ffi-smoke f64 libm] FAIL: -> $$out (expected t)"; exit 1; \
	fi
	@printf '%s\n' '(list (= (nl-ffi-call "pow" 2.0 10.0) 1024.0) (= (nl-ffi-call "ldexp" 1.5 3) 12.0) (= (nl-ffi-call "hypot" 3.0 4.0) 5.0))' > target/standalone-reader-ffi-f64b.el
	@out="$$($(STANDALONE_BIN) --load target/standalone-reader-ffi-f64b.el)"; \
	if [ "$$out" = "(t t t)" ]; then \
	  echo "[ffi-smoke f64 mixed] PASS: pow(2,10)=1024 / ldexp(1.5,3 :: f64+i64)=12 / hypot(3,4)=5 -> $$out"; \
	else \
	  echo "[ffi-smoke f64 mixed] FAIL: -> $$out (expected (t t t))"; exit 1; \
	fi
ifeq ($(STANDALONE_GATE_TARGET),windows-x86_64)
	@echo "[ffi-smoke D1 gnutls] SKIP: Windows external TLS policy is Stage D2"
	@echo "[ffi-smoke FreeType] SKIP: no Windows DLL mapping in Stage D1"
else
	@printf '%s\n' '(let ((p (nl-ffi-call "gnutls_check_version" 0))) (if (= p 0) "NULL" (unibyte-string (ptr-read-u8 p 0) (ptr-read-u8 p 1))))' > target/standalone-reader-ffi-d1.el
	@out="$$($(STANDALONE_BIN) --load target/standalone-reader-ffi-d1.el)"; \
	case "$$out" in \
	  '"'[0-9].'"') echo "[ffi-smoke D1 gnutls] PASS: gnutls_check_version -> $$out (X.)";; \
	  *) echo "[ffi-smoke D1 gnutls] FAIL: -> $$out (expected \"<digit>.\")"; exit 1;; \
	esac
	@printf '%s\n' '(let* ((s (alloc-bytes 8 8)) (rc (nl-ffi-call "FT_Init_FreeType" s)) (lib (ptr-read-u64 s 0)) (mj (alloc-bytes 4 4)) (mn (alloc-bytes 4 4)) (pt (alloc-bytes 4 4))) (nl-ffi-call "FT_Library_Version" lib mj mn pt) (let ((r (list rc (ptr-read-u32 mj 0)))) (nl-ffi-call "FT_Done_FreeType" lib) r))' > target/standalone-reader-ffi-f1.el
	@out="$$($(STANDALONE_BIN) --load target/standalone-reader-ffi-f1.el)"; \
	case "$$out" in \
	  '(0 '[0-9]*')') echo "[ffi-smoke F1 freetype] PASS: FT_Init+Version -> $$out (rc=0, major)";; \
	  *) echo "[ffi-smoke F1 freetype] FAIL: -> $$out (expected (0 <major>))"; exit 1;; \
	esac
	@font=/usr/share/fonts/truetype/dejavu/DejaVuSans.ttf; \
	if [ ! -f "$$font" ]; then \
	  echo "[ffi-smoke F2 glyph] SKIP: $$font not installed"; \
	else \
	  printf '%s\n' '(let* ((libp (alloc-bytes 8 8))) (nl-ffi-call "FT_Init_FreeType" libp) (let* ((lib (ptr-read-u64 libp 0)) (path "'"$$font"'") (pl (length path)) (pb (alloc-bytes 256 1)) (i 0)) (while (< i pl) (ptr-write-u8 pb i (aref path i)) (setq i (1+ i))) (ptr-write-u8 pb pl 0) (let* ((fp (alloc-bytes 8 8)) (nf (nl-ffi-call "FT_New_Face" lib pb 0 fp)) (face (ptr-read-u64 fp 0))) (nl-ffi-call "FT_Set_Pixel_Sizes" face 0 48) (let* ((gi (nl-ffi-call "FT_Get_Char_Index" face 65)) (ap (alloc-bytes 8 8)) (gr (nl-ffi-call "FT_Get_Advance" face gi 0 ap)) (adv (ptr-read-u64 ap 0))) (nl-ffi-call "FT_Done_Face" face) (nl-ffi-call "FT_Done_FreeType" lib) (list nf gi gr adv)))))' > target/standalone-reader-ffi-f2.el; \
	  out="$$($(STANDALONE_BIN) --load target/standalone-reader-ffi-f2.el 2>/dev/null)"; \
	  case "$$out" in \
	    '(0 '[1-9]*' 0 '[1-9]*')') echo "[ffi-smoke F2 glyph] PASS: FT_New_Face+Get_Advance('A') -> $$out (newface rc, gindex, adv rc, 16.16 advance)";; \
	    *) echo "[ffi-smoke F2 glyph] FAIL: -> $$out (expected (0 <gindex> 0 <advance>))"; exit 1;; \
	  esac; \
	  printf '%s\n' '(let* ((libp (alloc-bytes 8 8))) (nl-ffi-call "FT_Init_FreeType" libp) (let* ((lib (ptr-read-u64 libp 0)) (path "'"$$font"'") (pl (length path)) (pb (alloc-bytes 256 1)) (i 0)) (while (< i pl) (ptr-write-u8 pb i (aref path i)) (setq i (1+ i))) (ptr-write-u8 pb pl 0) (let* ((fp (alloc-bytes 8 8)) (nf (nl-ffi-call "FT_New_Face" lib pb 0 fp)) (face (ptr-read-u64 fp 0))) (nl-ffi-call "FT_Set_Pixel_Sizes" face 0 48) (let* ((gi (nl-ffi-call "FT_Get_Char_Index" face 65)) (lg (nl-ffi-call "FT_Load_Glyph" face gi 0)) (slot (ptr-read-u64 face 152)) (rg (nl-ffi-call "FT_Render_Glyph" slot 0)) (rows (ptr-read-u32 slot 152)) (width (ptr-read-u32 slot 156))) (nl-ffi-call "FT_Done_Face" face) (nl-ffi-call "FT_Done_FreeType" lib) (list lg rg rows width)))))' > target/standalone-reader-ffi-f3.el; \
	  out="$$($(STANDALONE_BIN) --load target/standalone-reader-ffi-f3.el 2>/dev/null)"; \
	  case "$$out" in \
	    '(0 0 '[1-9]*' '[1-9]*')') echo "[ffi-smoke F3 bitmap] PASS: FT_Load_Glyph+FT_Render_Glyph('A') -> $$out (load rc, render rc, bitmap rows, width)";; \
	    *) echo "[ffi-smoke F3 bitmap] FAIL: -> $$out (expected (0 0 <rows> <width>))"; exit 1;; \
	  esac; \
	  printf '%s\n' '(let* ((libp (alloc-bytes 8 8))) (nl-ffi-call "FT_Init_FreeType" libp) (let* ((lib (ptr-read-u64 libp 0)) (path "'"$$font"'") (pl (length path)) (pb (alloc-bytes 256 1)) (i 0)) (while (< i pl) (ptr-write-u8 pb i (aref path i)) (setq i (1+ i))) (ptr-write-u8 pb pl 0) (let* ((fp (alloc-bytes 8 8)) (nf (nl-ffi-call "FT_New_Face" lib pb 0 fp)) (face (ptr-read-u64 fp 0)) (mat (alloc-bytes 32 8)) (vec (alloc-bytes 16 8))) (nl-ffi-call "FT_Set_Pixel_Sizes" face 0 48) (ptr-write-u64 mat 0 131072) (ptr-write-u64 mat 8 0) (ptr-write-u64 mat 16 0) (ptr-write-u64 mat 24 131072) (ptr-write-u64 vec 0 0) (ptr-write-u64 vec 8 0) (nl-ffi-call "FT_Set_Transform" face mat vec) (let* ((gi (nl-ffi-call "FT_Get_Char_Index" face 65)) (lg (nl-ffi-call "FT_Load_Glyph" face gi 0)) (slot (ptr-read-u64 face 152)) (rg (nl-ffi-call "FT_Render_Glyph" slot 0)) (w (ptr-read-u32 slot 156))) (nl-ffi-call "FT_Done_Face" face) (nl-ffi-call "FT_Done_FreeType" lib) w))))' > target/standalone-reader-ffi-f4.el; \
	  out="$$($(STANDALONE_BIN) --load target/standalone-reader-ffi-f4.el 2>/dev/null)"; \
	  if [ "$$out" -ge 55 ] 2>/dev/null; then \
	    echo "[ffi-smoke F4 transform] PASS: FT_Set_Transform(2x via FT_Matrix/FT_Vector) -> 2x-glyph width $$out px (vs ~33 untransformed)"; \
	  else \
	    echo "[ffi-smoke F4 transform] FAIL: -> $$out (expected 2x width >= 55)"; exit 1; \
	  fi; \
	fi
endif
	@echo "[standalone-reader-ffi-smoke] PASS: platform-mapped libc + libm(f64), with optional GnuTLS/FreeType, via nl-ffi-call"

# 2026-08-23: `standalone-reader-ffi-smoke' above force-rebuilds with
# NELISP_READER_DYNAMIC=1, so it only ever tests that one opt-in variant --
# never the plain `make standalone-reader' / `make standalone-reader-test' /
# `tools/build-release-artifact.sh' binary end users and CI's own binary-tier
# gates actually run.  The owner's 2026-08-23 real-machine probe (Windows PE
# + a WSL Debian Linux ELF `target/nelisp', both built the ordinary way) found
# `nl-ffi-call' void on both -- a true finding the smoke above could not have
# caught, since it never builds what those binaries are.  This smoke pins the
# OTHER side of the matrix: on a build with no dynamic FFI linkage, `nl-ffi-
# call' must still be `fboundp' (not void) and must signal the catchable
# `nelisp-unsupported-primitive' condition on every entry point a user can
# reach it from -- bare FILE, --load, --eval, eval-elisp-source, the REPL,
# and a compiled artifact -- not just the one or two paths a smoke happens to
# exercise.  See `nelisp-standalone--applyfn-ffi-unsupported-form' in
# scripts/nelisp-standalone-build.el (the always-installed fallback arm) and
# docs/design/100-phase-47-dynamic-link-elisp.org section 7 (the
# availability matrix) for the fix this pins.  The condition's DATA is the
# symbol `nl-ffi-call' (a one-element list, Emacs `wrong-type-argument'
# style), not a string -- deliberately: `nl-ffi-call' is itself a listed
# `nl-safe-unsafe-primitives' name (packages/nl-safe/src/nl-safe.el), so its
# construction lives in the one file `tools/unsafe-kernel.txt' allows to
# mention it, built from packed symbol-name bytes rather than a string
# literal (see that function's own commentary for why a first version of
# this fix, which put a string-carrying `defun' in the prelude instead,
# tripped `unsafe-inventory').
# On targets without a native import-backed FFI this gate must build the STATIC
# default (hence `-u NELISP_READER_DYNAMIC')
# and then assert against it.  It used to unset NELISP_STANDALONE_TARGET as
# well, which made the build fall back to linux-x86_64 -- so on a Windows
# host it cross-built an ELF into `target/nelisp' and then ran
# `target/nelisp.exe', a binary it had not built, and reported the mismatch
# as a gate failure.  The target is passed explicitly now; only the dynamic
# flag is unset, which is the one thing this gate is actually about.
standalone-reader-ffi-unsupported-smoke:
ifeq ($(STANDALONE_GATE_TARGET),windows-x86_64)
	@echo "[standalone-reader-ffi-unsupported-smoke] PASS: not applicable; windows-x86_64 has an unconditional PE-import-backed FFI subset (covered by ffi-smoke)"
else
	@mkdir -p target
	@env -u NELISP_READER_DYNAMIC NELISP_STANDALONE_TARGET=$(STANDALONE_GATE_TARGET) $(EMACS) --batch -Q -L lisp -L src -L scripts \
	  --eval '(setq load-prefer-newer t)' \
	  -l nelisp-standalone-build -f nelisp-standalone-build-reader
	@chmod +x $(STANDALONE_BIN)
	@case "$$(file -b $(STANDALONE_BIN) 2>/dev/null)" in \
	  *"dynamically linked"*) echo "[ffi-unsupported-smoke] FAIL: target/nelisp is dynamically linked -- not the static default this smoke must test"; exit 1;; \
	esac
	@printf '%s\n' '(fboundp (quote nl-ffi-call))' > target/standalone-reader-ffi-unsupported-fboundp.el
	@out="$$($(STANDALONE_BIN) --load target/standalone-reader-ffi-unsupported-fboundp.el)"; \
	if [ "$$out" = "t" ]; then \
	  echo "[ffi-unsupported-smoke fboundp] PASS: (fboundp 'nl-ffi-call) -> t (static default build)"; \
	else \
	  echo "[ffi-unsupported-smoke fboundp] FAIL: -> $$out (expected t; nl-ffi-call must never be void)"; exit 1; \
	fi
	@caught='(condition-case e (nl-ffi-call "toupper" 97) (nelisp-unsupported-primitive (car (cdr e))))'; \
	printf '%s\n' "$$caught" > target/standalone-reader-ffi-unsupported-load.el; \
	out="$$($(STANDALONE_BIN) --load target/standalone-reader-ffi-unsupported-load.el)"; \
	if [ "$$out" = "nl-ffi-call" ]; then \
	  echo "[ffi-unsupported-smoke --load] PASS: condition-case caught nelisp-unsupported-primitive"; \
	else \
	  echo "[ffi-unsupported-smoke --load] FAIL: -> $$out (expected nl-ffi-call)"; exit 1; \
	fi; \
	printf '%s\n' "(prin1 $$caught)" > target/standalone-reader-ffi-unsupported-bare.el; \
	out="$$($(STANDALONE_BIN) target/standalone-reader-ffi-unsupported-bare.el)"; \
	if [ "$$out" = "nl-ffi-call" ]; then \
	  echo "[ffi-unsupported-smoke bare-FILE] PASS: condition-case caught nelisp-unsupported-primitive"; \
	else \
	  echo "[ffi-unsupported-smoke bare-FILE] FAIL: -> $$out (expected nl-ffi-call)"; exit 1; \
	fi; \
	out="$$($(STANDALONE_BIN) --eval "$$caught")"; \
	if [ "$$out" = "nl-ffi-call" ]; then \
	  echo "[ffi-unsupported-smoke --eval] PASS: condition-case caught nelisp-unsupported-primitive"; \
	else \
	  echo "[ffi-unsupported-smoke --eval] FAIL: -> $$out (expected nl-ffi-call)"; exit 1; \
	fi; \
	printf '%s\n' '(+ 1 2)' > target/standalone-reader-ffi-unsupported-src.el; \
	out="$$($(STANDALONE_BIN) eval-elisp-source target/standalone-reader-ffi-unsupported-src.el "$$caught")"; \
	if [ "$$out" = "nl-ffi-call" ]; then \
	  echo "[ffi-unsupported-smoke eval-elisp-source] PASS: condition-case caught nelisp-unsupported-primitive"; \
	else \
	  echo "[ffi-unsupported-smoke eval-elisp-source] FAIL: -> $$out (expected nl-ffi-call)"; exit 1; \
	fi; \
	out="$$(printf '%s\n' "$$caught" | $(STANDALONE_BIN) --repl --no-prompt 2>&1)"; \
	if [ "$$out" = "nl-ffi-call" ]; then \
	  echo "[ffi-unsupported-smoke REPL] PASS: condition-case caught nelisp-unsupported-primitive"; \
	else \
	  echo "[ffi-unsupported-smoke REPL] FAIL: -> $$out (expected nl-ffi-call)"; exit 1; \
	fi; \
	$(STANDALONE_BIN) compile-elisp-artifact --kind nelc \
	  --input target/standalone-reader-ffi-unsupported-load.el \
	  --output target/standalone-reader-ffi-unsupported.nelc > /dev/null; \
	out="$$($(STANDALONE_BIN) eval-elisp-artifact target/standalone-reader-ffi-unsupported.nelc "$$caught")"; \
	if [ "$$out" = "nl-ffi-call" ]; then \
	  echo "[ffi-unsupported-smoke compiled-artifact] PASS: condition-case caught nelisp-unsupported-primitive"; \
	else \
	  echo "[ffi-unsupported-smoke compiled-artifact] FAIL: -> $$out (expected nl-ffi-call)"; exit 1; \
	fi
	@echo "[standalone-reader-ffi-unsupported-smoke] PASS: nl-ffi-call is fboundp and raises nelisp-unsupported-primitive (never void) on the static default build, across bare FILE / --load / --eval / eval-elisp-source / REPL / compiled artifact"
endif

# Phase 47.D D2: REAL TLS 1.3 handshake from the pure-elisp reader.  The Linux
# recipe below remains the original GnuTLS dynamic-build probe: it opens a raw
# TCP socket (syscall-direct socket/connect to 1.1.1.1:443), then drives a full
# GnuTLS client handshake via nl-ffi-call: global_init -> allocate credentials ->
# init(CLIENT) -> server_name_set(SNI) -> set_default_priority -> credentials_set
# -> transport_set_int2(fd) -> handshake -> protocol_get_version/name, then D3:
# gnutls_record_send an HTTP/1.1 GET and gnutls_record_recv the reply, asserting
# a "HTTP" status line comes back.  The first record_recv returns
# GNUTLS_E_AGAIN(-28); a second sequential (unrolled) call delivers the decrypted
# response.  NOTE: driving record_recv from a `while' retry loop instead crashes
# *inside* libgnutls (NULL deref) — see Doc 100 6 "misdiagnosis" entry; the
# unrolled form is the robust shape and is what we ship.  Then bye -> deinit.
# Asserts TLS1.x negotiated AND a real HTTPS response.  NETWORK-GATED: skips if
# 1.1.1.1:443 is not reachable (not part of the hermetic gate).
standalone-reader-tls-smoke: $(if $(filter windows%,$(STANDALONE_GATE_TARGET)),standalone-reader-tls-smoke-windows,standalone-reader-tls-smoke-linux)
	@:

standalone-reader-tls-smoke-linux:
	@mkdir -p target
	@if ! timeout 6 bash -c 'exec 3<>/dev/tcp/1.1.1.1/443' 2>/dev/null; then \
	  echo "[tls-smoke D2] SKIP: no egress to 1.1.1.1:443"; exit 0; \
	fi; \
	NELISP_READER_DYNAMIC=1 $(EMACS) --batch -Q -L lisp -L src -L scripts \
	  --eval '(setq load-prefer-newer t)' \
	  -l nelisp-standalone-build -f nelisp-standalone-build-reader; \
	chmod +x target/nelisp; \
	printf '%s\n' '(let* ((fd (syscall-direct 41 2 1 0 0 0 0)) (sa (alloc-bytes 16 8)) (i 0) (host "one.one.one.one") (hl (length host)) (hbuf (alloc-bytes 32 1)) (j 0) (req "GET / HTTP/1.1\r\nHost: one.one.one.one\r\nConnection: close\r\n\r\n") (rl (length req)) (rbuf (alloc-bytes 256 1)) (k 0)) (while (< j hl) (ptr-write-u8 hbuf j (aref host j)) (setq j (1+ j))) (while (< k rl) (ptr-write-u8 rbuf k (aref req k)) (setq k (1+ k))) (while (< i 16) (ptr-write-u8 sa i 0) (setq i (1+ i))) (ptr-write-u8 sa 0 2) (ptr-write-u8 sa 2 1) (ptr-write-u8 sa 3 187) (ptr-write-u8 sa 4 1) (ptr-write-u8 sa 5 1) (ptr-write-u8 sa 6 1) (ptr-write-u8 sa 7 1) (let ((crc (syscall-direct 42 fd sa 16 0 0 0)) (credp (alloc-bytes 8 8)) (sessp (alloc-bytes 8 8))) (nl-ffi-call "gnutls_global_init") (nl-ffi-call "gnutls_certificate_allocate_credentials" credp) (nl-ffi-call "gnutls_init" sessp 2) (let* ((cred (ptr-read-u64 credp 0)) (sess (ptr-read-u64 sessp 0))) (nl-ffi-call "gnutls_server_name_set" sess 1 hbuf hl) (nl-ffi-call "gnutls_set_default_priority" sess) (nl-ffi-call "gnutls_credentials_set" sess 1 cred) (nl-ffi-call "gnutls_transport_set_int2" sess fd fd) (let* ((hs (nl-ffi-call "gnutls_handshake" sess)) (ver (nl-ffi-call "gnutls_protocol_get_version" sess)) (np (nl-ffi-call "gnutls_protocol_get_name" ver)) (nm (if (= np 0) "?" (unibyte-string (ptr-read-u8 np 0) (ptr-read-u8 np 1) (ptr-read-u8 np 2) (ptr-read-u8 np 3)))) (sent (nl-ffi-call "gnutls_record_send" sess rbuf rl)) (resp (alloc-bytes 512 1)) (g1 (nl-ffi-call "gnutls_record_recv" sess resp 511)) (got (nl-ffi-call "gnutls_record_recv" sess resp 511)) (st (if (> got 0) (unibyte-string (ptr-read-u8 resp 0) (ptr-read-u8 resp 1) (ptr-read-u8 resp 2) (ptr-read-u8 resp 3)) "?"))) (nl-ffi-call "gnutls_bye" sess 0) (nl-ffi-call "gnutls_deinit" sess) (nl-ffi-call "gnutls_certificate_free_credentials" cred) (nl-ffi-call "gnutls_global_deinit") (syscall-direct 3 fd 0 0 0 0 0) (list hs nm sent st)))))' > target/standalone-reader-tls-smoke.el; \
	out="$$(timeout $(STANDALONE_SMOKE_TIMEOUT) $(STANDALONE_BIN) --load target/standalone-reader-tls-smoke.el 2>/dev/null)"; \
	case "$$out" in \
	  '(0 "TLS'*'"HTTP")') echo "[tls-smoke D2+D3] PASS: real handshake + HTTPS GET -> $$out (handshake, proto, bytes-sent, response)";; \
	  *) echo "[tls-smoke D2+D3] FAIL: -> $$out (expected (0 \"TLS..\" <sent> \"HTTP\"))"; exit 1;; \
	esac

# windows-x86_64 uses the target-native Winsock -> Schannel primitive family.
# The first DecryptMessage from this Cloudflare endpoint has returned
# SEC_I_RENEGOTIATE in the live slice-2 probe, so the post-handshake path and
# its SECBUFFER_EXTRA carry-over are part of what the "HTTP" assertion covers.
# A second run drains through the peer's close_notify/TCP EOF, then proves the
# close edge contract: post-EOF close succeeds, the caller can still close the
# socket, and every context-taking primitive rejects closed and never-issued
# handles with a catchable error.
# NETWORK-GATED exactly like the Linux recipe; not part of the hermetic set.
standalone-reader-tls-smoke-windows:
	@mkdir -p target
	@if ! timeout 6 bash -c 'exec 3<>/dev/tcp/1.1.1.1/443' 2>/dev/null; then \
	  echo "[tls-smoke D2] SKIP: no egress to 1.1.1.1:443"; exit 0; \
	fi; \
	NELISP_STANDALONE_TARGET=$(STANDALONE_GATE_TARGET) $(EMACS) --batch -Q -L lisp -L src -L scripts \
	  --eval '(setq load-prefer-newer t)' \
	  -l nelisp-standalone-build -f nelisp-standalone-build-reader; \
	printf '%s\n' '(let* ((fd (nelisp-socket-connect "1.1.1.1" 443)) (ctx (nelisp-tls-connect fd "one.one.one.one")) (req "GET / HTTP/1.1\r\nHost: one.one.one.one\r\nConnection: close\r\n\r\n") (sent (nelisp-tls-send ctx req)) (resp (nelisp-tls-recv ctx 512)) (st (if (>= (length resp) 4) (unibyte-string (aref resp 0) (aref resp 1) (aref resp 2) (aref resp 3)) "?")) (proto (nelisp-tls-protocol ctx))) (nelisp-tls-close ctx) (nelisp-socket-close fd) (list 0 proto sent st))' > target/standalone-reader-tls-smoke.el; \
	out="$$(timeout $(STANDALONE_SMOKE_TIMEOUT) $(STANDALONE_BIN) --load target/standalone-reader-tls-smoke.el 2>/dev/null)"; \
	case "$$out" in \
	  '(0 "TLS'*'"HTTP")') echo "[tls-smoke D2+D3 windows] PASS: Winsock + Schannel HTTPS GET + close -> $$out";; \
	  *) echo "[tls-smoke D2+D3 windows] FAIL: -> $$out (expected (0 \"TLS..\" <sent> \"HTTP\"))"; exit 1;; \
	esac; \
	printf '%s\n' '(let* ((fd (nelisp-socket-connect "1.1.1.1" 443)) (ctx (nelisp-tls-connect fd "one.one.one.one")) (req "GET / HTTP/1.1\r\nHost: one.one.one.one\r\nConnection: close\r\n\r\n") (sent (nelisp-tls-send ctx req)) (part (nelisp-tls-recv ctx 4096))) (while (> (length part) 0) (setq part (nelisp-tls-recv ctx 4096))) (let ((closed (nelisp-tls-close ctx)) (socket-closed (nelisp-socket-close fd)) (twice (condition-case e (progn (nelisp-tls-close ctx) (quote missed)) (nelisp-tls-error (car e)))) (never (condition-case e (progn (nelisp-tls-close 0) (quote missed)) (nelisp-tls-error (car e)))) (closed-protocol (condition-case e (progn (nelisp-tls-protocol ctx) (quote missed)) (nelisp-tls-error (car e)))) (closed-send (condition-case e (progn (nelisp-tls-send ctx "x") (quote missed)) (nelisp-tls-error (car e)))) (closed-recv (condition-case e (progn (nelisp-tls-recv ctx 16) (quote missed)) (nelisp-tls-error (car e)))) (never-protocol (condition-case e (progn (nelisp-tls-protocol 424242) (quote missed)) (nelisp-tls-error (car e)))) (never-send (condition-case e (progn (nelisp-tls-send 424242 "x") (quote missed)) (nelisp-tls-error (car e)))) (never-recv (condition-case e (progn (nelisp-tls-recv 424242 8) (quote missed)) (nelisp-tls-error (car e))))) (list closed socket-closed twice never (list closed-protocol closed-send closed-recv never-protocol never-send never-recv))))' > target/standalone-reader-tls-close-edges.el; \
	edges="$$(timeout $(STANDALONE_SMOKE_TIMEOUT) $(STANDALONE_BIN) --load target/standalone-reader-tls-close-edges.el 2>/dev/null)"; \
	if [ "$$edges" = "(nil nil nelisp-tls-error nelisp-tls-error (nelisp-tls-error nelisp-tls-error nelisp-tls-error nelisp-tls-error nelisp-tls-error nelisp-tls-error))" ]; then \
	  echo "[tls-smoke close edges windows] PASS: ownership plus closed/never-issued protocol/send/recv -> $$edges"; \
	else \
	  echo "[tls-smoke close edges windows] FAIL: -> $$edges"; exit 1; \
	fi

# Runtime smoke for the reader's process substrate (call-process /
# start-process / pipe read, scripts/nelisp-standalone-build.el).  The host ERT
# only checks the emitted C structure, NOT real fork/execve/wait4 behaviour, so
# this exercises the freestanding binary against actual subprocesses.
# POSIX-only (Windows builds emit -1 stubs, covered by the target ERT).
# Split by what the target actually provides, because the two halves have
# different answers on windows-native and lumping them together made the
# gate report a working thing as broken.
#
# SYNCHRONOUS `call-process' WORKS on windows-x86_64: the W32 spawn model
# (`nelisp-standalone--fileio-process-call-forms', scripts/nelisp-
# standalone-build.el -- ArgvQuote-compatible command line, STARTUPINFOW
# with STARTF_USESTDHANDLES, CreateProcessW/WaitForSingleObject/
# GetExitCodeProcess) is fully implemented there.  This gate reported
# `call-process exit=1' only because it hardcoded `/bin/sh', which
# CreateProcessW cannot resolve, so the arm returned its "could not create"
# literal.  Verified by hand: `(nelisp-process-call-process
# "C:/Windows/System32/cmd.exe" nil nil nil "/c" "exit 7")' answers 7.
#
# ASYNCHRONOUS `nelisp-process-start' does NOT work there: `nl_os_process_
# fork'/`_pipe'/`_dup2'/`_set_nonblock'/`_poll_readable' are all literal
# `-1' stubs in that target's os-base-forms, so the three async thirds
# cannot pass until a CreatePipe/CreateProcessW async spawn exists.  They
# report a reasoned GATE-SKIP there rather than a failure -- a gate that
# says why it did not run is not the same as one that ran and passed, and
# this repository's own rules turn on telling those apart.
PROCESS_SMOKE_SH = $(if $(filter windows%,$(STANDALONE_GATE_TARGET)),C:/Windows/System32/cmd.exe,/bin/sh)
PROCESS_SMOKE_CFLAG = $(if $(filter windows%,$(STANDALONE_GATE_TARGET)),/c,-c)
standalone-reader-process-smoke: standalone-reader
	@mkdir -p target
	@printf '%s\n' '(nelisp-process-call-process "$(PROCESS_SMOKE_SH)" nil nil nil "$(PROCESS_SMOKE_CFLAG)" "exit 7")' > target/standalone-reader-process-smoke-cp.el
	@printf '%s\n' '(let* ((p (nelisp-process-start "/bin/sh" "-c" "printf process-smoke-ok"))) (nelisp-process-wait p) (nelisp-process-read-output p 64))' > target/standalone-reader-process-smoke-async.el
	@printf '%s\n' '(let* ((p (nelisp-process-start "/bin/cat")) (w (nelisp-process-write p "cat-roundtrip"))) (nelisp-process-close-stdin p) (nelisp-process-wait p) (let* ((out (nelisp-process-read-output p 64)) (ev (nelisp-process-poll p)) (ready (aref ev 0)) (exited (aref ev 1)) (code (aref ev 2))) (list w out ready exited code)))' > target/standalone-reader-process-smoke-cat.el
	@printf '%s\n' '(let* ((p (nelisp-process-start "/bin/sh" "-c" "sleep 1; printf sleepy")) (ev0 (nelisp-process-poll p)) (r0 (aref ev0 0)) (e0 (aref ev0 1))) (nelisp-process-wait p) (let* ((ev1 (nelisp-process-poll p)) (r1 (aref ev1 0)) (e1 (aref ev1 1)) (out (nelisp-process-read-output p 64))) (list r0 e0 r1 e1 out)))' > target/standalone-reader-process-smoke-poll.el
	@set +e; $(STANDALONE_BIN) target/standalone-reader-process-smoke-cp.el; cp_rc=$$?; set -e; \
	if [ "$$cp_rc" != "7" ]; then \
	  echo "[standalone-reader-process-smoke] FAIL: call-process exit=$$cp_rc (expected 7 from $(PROCESS_SMOKE_SH))"; \
	  exit 1; \
	fi; \
	echo "[standalone-reader-process-smoke] call-process: PASS (exit=$$cp_rc via $(PROCESS_SMOKE_SH))"; \
	if [ -n "$(filter windows%,$(STANDALONE_GATE_TARGET))" ]; then \
	  echo "GATE-SKIP async process on $(STANDALONE_GATE_TARGET): nl_os_process_fork/_pipe/_dup2/_poll_readable are -1 stubs on this target"; \
	  echo "[standalone-reader-process-smoke] PASS: call-process exit=$$cp_rc; async thirds skipped with a reason"; \
	  exit 0; \
	fi; \
	out="$$($(STANDALONE_BIN) --load target/standalone-reader-process-smoke-async.el)"; \
	cat_out="$$($(STANDALONE_BIN) --load target/standalone-reader-process-smoke-cat.el)"; \
	poll_out="$$($(STANDALONE_BIN) --load target/standalone-reader-process-smoke-poll.el)"; \
	if [ "$$out" = '"process-smoke-ok"' ] && [ "$$cat_out" = '(13 "cat-roundtrip" 1 1 0)' ] && [ "$$poll_out" = '(0 0 1 1 "sleepy")' ]; then \
	  echo "[standalone-reader-process-smoke] PASS: call-process exit=$$cp_rc, read-output -> $$out, cat -> $$cat_out, poll -> $$poll_out"; \
	else \
	  echo "[standalone-reader-process-smoke] FAIL: call-process exit=$$cp_rc, read-output -> $$out, cat -> $$cat_out, poll -> $$poll_out"; \
	  exit 1; \
	fi

# Doc 184 P0: `packages/nelisp-eventloop/src/nelisp-async-core.el' is the
# actor/generator-free half of the timer queue (`nelisp-async.el' pulls in
# `nelisp-actor' -> `generator', which is unreachable standalone -- Doc 184
# S1.4/S1.5).  This smoke is the doc's own P0 exit criterion: the file
# loads standalone with no generator error, and a REPEAT timer re-arms and
# fires more than once across two `--fire-due' calls, closing
# `tools/partial-accepted.txt''s `run-at-time' entry for anything that
# loads this module.
standalone-reader-async-core-smoke: standalone-reader
	@mkdir -p target
	@printf '%s\n' \
	  '(progn (fboundp (quote nelisp-async-core-run-at-time)))' \
	  > target/standalone-reader-async-core-smoke-load.el
	@printf '%s\n' \
	  '(let ((n 0) (tm nil)) (setq tm (nelisp-async-core-run-at-time 0 0.01 (lambda () (setq n (1+ n))))) (nelisp-async-core--fire-due (+ (nelisp-async-core--now) 0.001)) (nelisp-async-core--nanosleep 0.02) (nelisp-async-core--fire-due (nelisp-async-core--now)) (> n 1))' \
	  > target/standalone-reader-async-core-smoke-repeat.el
	@load_out="$$($(STANDALONE_BIN) --eval '(progn (load "packages/nelisp-eventloop/src/nelisp-async-core.el") (load "target/standalone-reader-async-core-smoke-load.el"))')"; \
	repeat_out="$$($(STANDALONE_BIN) --eval '(progn (load "packages/nelisp-eventloop/src/nelisp-async-core.el") (load "target/standalone-reader-async-core-smoke-repeat.el"))')"; \
	if [ "$$load_out" = "t" ] && [ "$$repeat_out" = "t" ]; then \
	  echo "[standalone-reader-async-core-smoke] PASS: loads standalone (no generator error), REPEAT re-arms and fires >1 across two fire-due calls -> load=$$load_out repeat=$$repeat_out"; \
	else \
	  echo "[standalone-reader-async-core-smoke] FAIL: load=$$load_out repeat=$$repeat_out"; \
	  exit 1; \
	fi

# Doc 184 P1/P2: `packages/nelisp-process-adapter/src/nelisp-process-adapter.el'
# closes the measured gaps in the prelude's own partial standard-name
# adapter (Doc 184 S1.3): `:filter' silently dropped, no
# process-filter/set-process-filter/process-sentinel/set-process-sentinel,
# `accept-process-output' ignoring PROCESS/SECONDS/MILLISEC and draining
# every pending process while collapsing every sentinel status to the
# literal string "finished\n".  Against-the-bug: RED is `make-network-
# process' being void-function and this same filter/REPEAT shape failing
# with target/nelisp built from a tree WITHOUT this adapter loaded (see
# `standalone-reader-process-adapter-smoke-red' below); GREEN is this
# target, all against the SAME binary with only the `--eval' load list
# differing -- the fix is a loadable upgrade layer, not a native/binary
# change (Doc 184 S2's decided direction).
# Doc 194 P0 note on the `netproc' probe two blocks below: `make-network-
# process' is no longer the Doc 184 S1.7/P4 unconditional-refusal stub
# this probe originally exercised.  `:name "x"' alone (no `:host'/
# `:service') still signals -- now because `:service' is a required,
# type-checked argument in the real implementation, not because every
# call unconditionally refused -- so this probe's expected `(car e)'
# changed from the literal symbol `error' to `wrong-type-argument'.  The
# real positive-path proof (a loopback client actually connecting,
# sending, and receiving bytes, including the against-the-bug RED of
# `make-network-process' being void-function on the `feat/socket-
# primitives-p1' base this doc builds on) lives in its own dedicated
# `standalone-reader-network-process-smoke' below, per Doc 194 P0's own
# exit criterion.
NELISP_PROCESS_ADAPTER_LOAD_1 = (load "packages/nelisp-eventloop/src/nelisp-async-core.el")
NELISP_PROCESS_ADAPTER_LOAD_2 = (load "packages/nelisp-process-adapter/src/nelisp-process-adapter.el")
PROCESS_ADAPTER_EXIT0_COMMAND = $(if $(filter windows%,$(STANDALONE_GATE_TARGET)),(list "C:/Windows/System32/cmd.exe" "/d" "/c" "exit 0"),(list "/bin/sh" "-c" "exit 0"))
PROCESS_ADAPTER_EXIT7_COMMAND = $(if $(filter windows%,$(STANDALONE_GATE_TARGET)),(list "C:/Windows/System32/cmd.exe" "/d" "/c" "exit 7"),(list "/bin/sh" "-c" "exit 7"))
PROCESS_ADAPTER_CAT_COMMAND = $(if $(filter windows%,$(STANDALONE_GATE_TARGET)),(list "$(STANDALONE_BIN)" "target/standalone-reader-process-child-cat.el"),(list "/bin/cat"))
PROCESS_ADAPTER_ECHO_COMMAND = $(if $(filter windows%,$(STANDALONE_GATE_TARGET)),(list "$(STANDALONE_BIN)" "target/standalone-reader-process-child-echo.el"),(list "/bin/echo" "hi"))
PROCESS_ADAPTER_SLEEP_COMMAND = $(if $(filter windows%,$(STANDALONE_GATE_TARGET)),(list "$(STANDALONE_BIN)" "target/standalone-reader-process-child-sleep.el"),(list "/bin/sleep" "1"))
standalone-reader-process-adapter-smoke: standalone-reader
	@mkdir -p target
	@printf '%s\n' '(let (chunk) (while (setq chunk (read-stdin-bytes 4096)) (princ chunk)))' > target/standalone-reader-process-child-cat.el
	@printf '%s\n' '$(NELISP_PROCESS_ADAPTER_LOAD_1)' '(nelisp-async-core--nanosleep 10)' > target/standalone-reader-process-child-sleep.el
	# The child fixture measured 0.557-0.838s quiet and at most 2.56s with four
	# concurrent children.  Windows gets 3.0s for the first output delivery:
	# 0.44s above that loaded maximum, yet 177s below the gate's 180s timeout,
	# so a child/pump hang still fails loudly.  Once started, the second pump
	# keeps the original 0.3s bound; the deliberate live-child 0.2s stays short.
	@printf '%s\n' \
	  '$(NELISP_PROCESS_ADAPTER_LOAD_1)' \
	  '$(NELISP_PROCESS_ADAPTER_LOAD_2)' \
	  '(let* (chunks (p (make-process :name "cat" :command $(PROCESS_ADAPTER_CAT_COMMAND) :filter (lambda (_p c) (push c chunks))))) (nelisp-process-write p "first-") (accept-process-output p $(if $(filter windows%,$(STANDALONE_GATE_TARGET)),3.0,0.3)) (nelisp-process-write p "second") (accept-process-output p 0.3) (nelisp-process-close-stdin p) (delete-process p) (nreverse chunks))' \
	  > target/standalone-reader-process-adapter-smoke-filter.el
	@printf '%s\n' \
	  '$(NELISP_PROCESS_ADAPTER_LOAD_1)' \
	  '$(NELISP_PROCESS_ADAPTER_LOAD_2)' \
	  '(list (let (msgs (p (make-process :name "ok" :command $(PROCESS_ADAPTER_EXIT0_COMMAND) :sentinel (lambda (_p m) (push m msgs))))) (accept-process-output p 1) (accept-process-output p 1) (car msgs)) (let (msgs (p (make-process :name "bad" :command $(PROCESS_ADAPTER_EXIT7_COMMAND) :sentinel (lambda (_p m) (push m msgs))))) (accept-process-output p 1) (accept-process-output p 1) (car msgs)) (let (msgs (p (make-process :name "sl" :command $(PROCESS_ADAPTER_SLEEP_COMMAND) :sentinel (lambda (_p m) (push m msgs))))) (accept-process-output p 0.2) (delete-process p) (car msgs)))' \
	  > target/standalone-reader-process-adapter-smoke-sentinel.el
	@printf '%s\n' \
	  '$(NELISP_PROCESS_ADAPTER_LOAD_1)' \
	  '$(NELISP_PROCESS_ADAPTER_LOAD_2)' \
	  '(let* (fired2 (p1 (make-process :name "fast" :command $(PROCESS_ADAPTER_EXIT0_COMMAND))) (p2 (make-process :name "slow" :command $(PROCESS_ADAPTER_SLEEP_COMMAND) :sentinel (lambda (_p m) (setq fired2 m))))) (accept-process-output p1 1) (let ((res (list fired2 (process-live-p p2)))) (delete-process p2) res))' \
	  > target/standalone-reader-process-adapter-smoke-narrow.el
	@printf '%s\n' \
	  '$(NELISP_PROCESS_ADAPTER_LOAD_1)' \
	  '$(NELISP_PROCESS_ADAPTER_LOAD_2)' \
	  '(condition-case e (progn (make-network-process :name "x") (quote no-error)) (error (car e)))' \
	  > target/standalone-reader-process-adapter-smoke-netproc.el
	@printf '%s\n' \
	  '$(NELISP_PROCESS_ADAPTER_LOAD_1)' \
	  '$(NELISP_PROCESS_ADAPTER_LOAD_2)' \
	  '(let ((n 0)) (run-at-time 0 0.02 (lambda () (setq n (1+ n)))) (accept-process-output nil 0.3) (>= n 2))' \
	  > target/standalone-reader-process-adapter-smoke-repeat.el
	@filter_out="$$($(STANDALONE_BIN) --load target/standalone-reader-process-adapter-smoke-filter.el)"; \
	sentinel_out="$$($(STANDALONE_BIN) --load target/standalone-reader-process-adapter-smoke-sentinel.el)"; \
	narrow_out="$$($(STANDALONE_BIN) --load target/standalone-reader-process-adapter-smoke-narrow.el)"; \
	netproc_out="$$($(STANDALONE_BIN) --load target/standalone-reader-process-adapter-smoke-netproc.el)"; \
	repeat_out="$$($(STANDALONE_BIN) --load target/standalone-reader-process-adapter-smoke-repeat.el)"; \
	if [ "$$filter_out" = '("first-" "second")' ] && \
	   [ "$$sentinel_out" = "$$(printf '(\042finished\n\042 \042exited abnormally with code 7\n\042 \042terminated\n\042)')" ] && \
	   [ "$$narrow_out" = "(nil t)" ] && \
	   [ "$$netproc_out" = "wrong-type-argument" ] && \
	   [ "$$repeat_out" = "t" ]; then \
	  echo "[standalone-reader-process-adapter-smoke] PASS: filter=$$filter_out sentinel=$$sentinel_out narrow(fired2,proc2-live)=$$narrow_out make-network-process=$$netproc_out repeat-through-shared-loop=$$repeat_out"; \
	else \
	  echo "[standalone-reader-process-adapter-smoke] FAIL: filter=$$filter_out sentinel=$$sentinel_out narrow=$$narrow_out netproc=$$netproc_out repeat=$$repeat_out"; \
	  exit 1; \
	fi

# Default-bootstrap regression guard for the two smokes above (retired
# from its original job as of integration/wave6 phase 2A). Doc 184's fix
# used to be an opt-in loadable upgrade layer -- this exact target used
# to prove the PRE-fix defect shape still reproduced when neither new
# file was `--load'ed, because the default binary never carried the
# fix. Phase 2A wired `packages/nelisp-eventloop/src/nelisp-async-core.el'
# and `packages/nelisp-process-adapter/src/nelisp-process-adapter.el''s
# source directly into `nelisp-standalone--reader-repl-prelude-source'
# (scripts/nelisp-standalone-build.el), so every standalone build now
# carries the fix baked in -- the old RED assertion (filter=nil,
# repeat=1) can no longer reproduce on ANY build, unfixed or not, and a
# real run against this exact recipe confirmed that (filter=("hi\n")
# repeat=0). This target now asserts the opposite claim, and is the
# thing that actually matters going forward: the fix ships WITHOUT an
# explicit `--load', so a future change to the prelude-source
# concatenation cannot silently drop it back to opt-in-only without
# this smoke catching it.
standalone-reader-process-adapter-smoke-red: standalone-reader
	@mkdir -p target
	@printf '%s\n' '(princ "hi\n")' > target/standalone-reader-process-child-echo.el
	# Same 0.557-0.838s quiet / 2.56s four-child loaded fixture measurement as
	# the full adapter gate: Windows gets 3.0s, leaving 0.44s above the measured
	# maximum and 177s below the 180s outer timeout, so a hang remains a failure.
	@printf '%s\n' \
	  '(let (got) (make-process :name "t" :command $(PROCESS_ADAPTER_ECHO_COMMAND) :filter (lambda (_p chunk) (push chunk got))) (accept-process-output nil $(if $(filter windows%,$(STANDALONE_GATE_TARGET)),3.0,1)) got)' \
	  > target/standalone-reader-process-adapter-smoke-red-filter.el
	@printf '%s\n' \
	  '(let ((n 0)) (run-at-time 0 0.02 (lambda () (setq n (1+ n)))) (accept-process-output nil 0.3) (>= n 2))' \
	  > target/standalone-reader-process-adapter-smoke-red-repeat.el
	@printf '%s\n' \
	  '(fboundp (quote process-filter))' \
	  > target/standalone-reader-process-adapter-smoke-red-fboundp.el
	@filter_out="$$($(STANDALONE_BIN) --load target/standalone-reader-process-adapter-smoke-red-filter.el)"; \
	repeat_out="$$($(STANDALONE_BIN) --load target/standalone-reader-process-adapter-smoke-red-repeat.el)"; \
	fboundp_out="$$($(STANDALONE_BIN) --load target/standalone-reader-process-adapter-smoke-red-fboundp.el)"; \
	filter_expect="$$(printf '(\042hi\n\042)')"; \
	if [ "$$filter_out" = "$$filter_expect" ] && [ "$$repeat_out" = "t" ] && [ "$$fboundp_out" = "t" ]; then \
	  echo "[standalone-reader-process-adapter-smoke-red] PASS: default binary, NO --load of either new file -- process-filter fboundp=$$fboundp_out, filter fires=$$filter_out, REPEAT-through-shared-loop=$$repeat_out (Doc 184 P1/P2 ships in the default bootstrap, integration/wave6 phase 2A)"; \
	else \
	  echo "[standalone-reader-process-adapter-smoke-red] FAIL: expected the default-bootstrap FIXED shape (fboundp=t, filter=(\"hi\\n\"), repeat=t) without loading either new file, got fboundp=$$fboundp_out filter=$$filter_out repeat=$$repeat_out -- the default prelude no longer carries Doc 184 P1/P2"; \
	  exit 1; \
	fi

# Doc 194 P0 exit criterion: `make-network-process'/`open-network-stream',
# the synchronous CLIENT path over Phase 1's own `nelisp-socket-*'
# primitives (feat/socket-primitives-p1).  Against-the-bug: RED is
# `make-network-process' being void-function on that base with neither
# new file loaded (reproduced verbatim below, same binary as GREEN);
# GREEN is this target -- same shape as `standalone-reader-socket-smoke'
# (Makefile:2096) but through the ELISP entry points instead of the raw
# primitives directly, per Doc 194's own P0 exit criterion text: build a
# real listener with `nelisp-socket-listen'/-accept (Phase 1's raw
# primitives, used here ONLY as the test's own server harness -- P0 does
# not build `:server t'), then `(open-network-stream ...)' against it,
# assert `process-status' reads `open' IMMEDIATELY (no `accept-process-
# output' call, matching Doc 194 S1.3's own measured timing against real
# Emacs 30.1), send/receive a UTF-8 Japanese payload BOTH directions,
# `delete-process' transitions status to `closed'.  A second case:
# connect to a closed port, assert the `condition-case ((file-error)
# ...)' idiom -- the one every existing `open-network-stream' caller
# already uses -- catches it, not a bare `nelisp-socket-error' leaking
# unmapped through the standard-name entry point.  A third case, UPDATED
# by doc 194 P4 (feat/network-process-p345): `:nowait t' USED to signal
# loudly here (P0-P2's own guard, "against-the-bug" for THIS phase per
# doc 194's own P4 exit criterion is exactly this flip) -- now it returns
# a real `network-process' immediately with `process-status' reading
# `connect' \(measured against real Emacs 30.1 during P4's own
# implementation: the identical status symbol\), never signalling; the
# async completion / sentinel-firing positive proof lives in its own
# dedicated `standalone-reader-network-process-nowait-smoke' below,
# P4's own exit criterion.  `:server t' UPDATED the same way by doc 194
# P5, same commit as this comment: it USED to signal loudly (P0-P2's own
# guard) -- now it returns a real listening `network-process' with
# `process-status' reading `listen' \(measured against real Emacs 30.1
# during P5's own implementation: the identical status symbol\), never
# signalling; the auto-accept / multi-client positive proof lives in its
# own dedicated `standalone-reader-network-process-server-smoke' below,
# P5's own exit criterion.  A fourth
# case: an ordinary native subprocess and a `network-process' coexisting
# in the SAME `nelisp-process-adapter--live' poll-set registry (Doc 194
# S3.1's own design) do not interfere with each other -- the subprocess's
# sentinel still fires normally and the network process is left
# untouched (its own async wiring is Doc 194 P3/P4, not this pass; see
# `nelisp-process-adapter--drain-and-fire''s network-process guard).
standalone-reader-network-process-smoke: standalone-reader
	@mkdir -p target
	@printf '%s\n' \
	  '(condition-case e (make-network-process :name "x" :host "127.0.0.1" :service 1) (error (quote (quote void-function-red))))' \
	  > target/standalone-reader-network-process-smoke-red.el
	@printf '%s\n' \
	  '$(NELISP_PROCESS_ADAPTER_LOAD_1)' \
	  '$(NELISP_PROCESS_ADAPTER_LOAD_2)' \
	  '(let* ((lfd (nelisp-socket-listen "127.0.0.1" 55901)) (cli (open-network-stream "cli" nil "127.0.0.1" 55901)) (status-immediate (process-status cli)) (sfd (nelisp-socket-accept lfd))) (process-send-string cli "ping-\346\227\245\346\234\254\350\252\236") (let ((srv-got (nelisp-socket-recv sfd 4096))) (nelisp-socket-send sfd "pong-\343\201\223\343\202\223\343\201\253\343\201\241\343\201\257") (let ((cli-got (nelisp-socket-recv (aref cli 3) 4096))) (delete-process cli) (nelisp-socket-close sfd) (nelisp-socket-close lfd) (list status-immediate (equal srv-got "ping-\346\227\245\346\234\254\350\252\236") (equal cli-got "pong-\343\201\223\343\202\223\343\201\253\343\201\241\343\201\257") (process-status cli) (process-live-p cli) (processp cli)))))' \
	  > target/standalone-reader-network-process-smoke-roundtrip.el
	@printf '%s\n' \
	  '$(NELISP_PROCESS_ADAPTER_LOAD_1)' \
	  '$(NELISP_PROCESS_ADAPTER_LOAD_2)' \
	  '(list (condition-case err (progn (open-network-stream "bad" nil "127.0.0.1" 1) (quote uncaught)) (file-error (car err))) (condition-case err (let ((p (make-network-process :name "x" :host "127.0.0.1" :service 80 :nowait t))) (prog1 (process-status p) (delete-process p))) (error (quote signalled))) (condition-case err (let ((p (make-network-process :name "x" :host "127.0.0.1" :service 55903 :server t))) (prog1 (process-status p) (delete-process p))) (error (quote signalled))))' \
	  > target/standalone-reader-network-process-smoke-refused.el
	@printf '%s\n' \
	  '$(NELISP_PROCESS_ADAPTER_LOAD_1)' \
	  '$(NELISP_PROCESS_ADAPTER_LOAD_2)' \
	  '(let* ((lfd (nelisp-socket-listen "127.0.0.1" 55902)) (net (open-network-stream "netcli" nil "127.0.0.1" 55902)) (sfd (nelisp-socket-accept lfd)) msgs (sub (make-process :name "echo" :command $(PROCESS_ADAPTER_EXIT0_COMMAND) :sentinel (lambda (_p m) (push m msgs))))) (accept-process-output nil 1) (accept-process-output nil 1) (let ((result (list (car msgs) (process-status net) (process-live-p sub)))) (delete-process net) (nelisp-socket-close sfd) (nelisp-socket-close lfd) result))' \
	  > target/standalone-reader-network-process-smoke-mixed.el
	@red_out="$$($(STANDALONE_BIN) --load target/standalone-reader-network-process-smoke-red.el)"; \
	roundtrip_out="$$($(STANDALONE_BIN) --load target/standalone-reader-network-process-smoke-roundtrip.el)"; \
	refused_out="$$($(STANDALONE_BIN) --load target/standalone-reader-network-process-smoke-refused.el)"; \
	mixed_out="$$($(STANDALONE_BIN) --load target/standalone-reader-network-process-smoke-mixed.el)"; \
	if [ "$$red_out" = "(quote void-function-red)" ] && \
	   [ "$$roundtrip_out" = "(open t t closed nil t)" ] && \
	   [ "$$refused_out" = "(file-error connect listen)" ] && \
	   [ "$$mixed_out" = "$$(printf '(\042finished\n\042 open nil)')" ]; then \
	  echo "[standalone-reader-network-process-smoke] PASS: red(void-fn-on-p1-base)=$$red_out roundtrip(status,srv-got,cli-got,closed,live-p,processp)=$$roundtrip_out refused(file-error,nowait,server)=$$refused_out mixed(sub-sentinel,net-status,sub-live)=$$mixed_out"; \
	else \
	  echo "[standalone-reader-network-process-smoke] FAIL: red=$$red_out roundtrip=$$roundtrip_out refused=$$refused_out mixed=$$mixed_out"; \
	  exit 1; \
	fi

# Doc 194 P1 exit criterion: `/etc/hosts' resolution (`nelisp--hosts-
# file-lookup', consulted by `nelisp--resolve-host' before DNS).  A
# fixture `/etc/hosts'-shaped temp file maps a made-up hostname to a
# loopback-reachable IP; `open-network-stream' against that HOSTNAME (not
# an IP literal) succeeds through P0's own client path with no network
# round trip at all -- `nelisp--etc-hosts-file' is let-bound to the
# fixture so the real system table is never touched.  A hostname absent
# from the fixture, with the P2 DNS resolver forced to a closed local
# port (127.0.0.1:1, an immediate ECONNREFUSED -- deterministic and fast,
# unlike pointing at a genuinely unreachable IP, which can hang for the
# OS's own multi-second/minute connect timeout), falls through cleanly to
# a caught `file-error' -- never a hang (wall-clock bounded well under
# the smoke's own timeout), never a wrong-address connect.
#
# The "immediate ECONNREFUSED" in that sentence is a POSIX fact.  Windows
# takes ~2.03s to report a refused loopback connect (measured on this branch
# while writing the socket arm), so the 5000ms bound that is generous on
# POSIX is marginal here: measured 3353-3781ms over 8 quiet runs, but
# 5612-5850ms in 6 of 8 runs under eight-way CPU load.  That is not a rare
# flake, it is a gate that fails whenever the machine is busy.  The windows
# bound is therefore 8000ms -- 2150ms above the loaded maximum, and 2000ms
# below the `timeout 10' on the fallthrough run.
#
# Raising it costs no hang detection, which is the reason it is safe: a real
# hang is caught by that `timeout 10' and shows up as a non-zero `fall_rc',
# not by this comparison.  What this bound guards is "slow but not hung", and
# 8000ms still catches that on windows.
standalone-reader-hosts-file-smoke: standalone-reader
	@mkdir -p target
	@printf '%s\n' \
	  '127.0.0.1 nelisp-p1-fixture-host.invalid nelisp-p1-fixture-alias.invalid' \
	  '# a comment line, and a blank line below' \
	  '' \
	  '203.0.113.9 nelisp-p1-unreachable.invalid' \
	  > target/standalone-reader-hosts-file-smoke-fixture.txt
	@printf '%s\n' \
	  '$(NELISP_PROCESS_ADAPTER_LOAD_1)' \
	  '$(NELISP_PROCESS_ADAPTER_LOAD_2)' \
	  '(setq nelisp--etc-hosts-file "target/standalone-reader-hosts-file-smoke-fixture.txt")' \
	  '(setq nelisp-dns-resolver-ip "127.0.0.1")' \
	  '(setq nelisp-dns-resolver-port 1)' \
	  '(let* ((lfd (nelisp-socket-listen "127.0.0.1" 55904)) (cli (open-network-stream "cli" nil "nelisp-p1-fixture-host.invalid" 55904)) (status (process-status cli)) (sfd (nelisp-socket-accept lfd))) (process-send-string cli "via-hosts-file") (let ((got (nelisp-socket-recv sfd 4096))) (delete-process cli) (nelisp-socket-close sfd) (nelisp-socket-close lfd) (list status (equal got "via-hosts-file") (nelisp--hosts-file-lookup "nelisp-p1-fixture-alias.invalid"))))' \
	  > target/standalone-reader-hosts-file-smoke-positive.el
	@printf '%s\n' \
	  '$(NELISP_PROCESS_ADAPTER_LOAD_1)' \
	  '$(NELISP_PROCESS_ADAPTER_LOAD_2)' \
	  '(setq nelisp--etc-hosts-file "target/standalone-reader-hosts-file-smoke-fixture.txt")' \
	  '(setq nelisp-dns-resolver-ip "127.0.0.1")' \
	  '(setq nelisp-dns-resolver-port 1)' \
	  '(condition-case err (progn (open-network-stream "cli" nil "nelisp-p1-no-such-fixture-entry.invalid" 80) (quote uncaught)) (file-error (quote caught-file-error)))' \
	  > target/standalone-reader-hosts-file-smoke-fallthrough.el
	@pos_out="$$($(STANDALONE_BIN) --load target/standalone-reader-hosts-file-smoke-positive.el)"; \
	start=$$(date +%s%N); \
	fall_out="$$(timeout 10 $(STANDALONE_BIN) --load target/standalone-reader-hosts-file-smoke-fallthrough.el)"; \
	fall_rc=$$?; \
	end=$$(date +%s%N); \
	elapsed_ms=$$(( (end - start) / 1000000 )); \
	if [ "$$pos_out" = "(open t \"127.0.0.1\")" ] && \
	   [ "$$fall_rc" = "0" ] && [ "$$fall_out" = "caught-file-error" ] && \
	   [ "$$elapsed_ms" -lt "$(if $(filter windows%,$(STANDALONE_GATE_TARGET)),8000,5000)" ]; then \
	  echo "[standalone-reader-hosts-file-smoke] PASS: positive(status,roundtrip,alias-lookup)=$$pos_out fallthrough(caught,elapsed_ms)=$$fall_out,$${elapsed_ms}ms (never a hang)"; \
	else \
	  echo "[standalone-reader-hosts-file-smoke] FAIL: positive=$$pos_out fallthrough_rc=$$fall_rc fallthrough=$$fall_out elapsed_ms=$$elapsed_ms"; \
	  exit 1; \
	fi


# Doc 194 P2 exit criterion: DNS-over-TCP/53 (RFC 7766), pure elisp on
# Phase 1's own socket primitives.  Fixture bytes are written as RAW
# binary files by this recipe's own `printf' calls (octal escapes),
# never built via an elisp `(string ...)'/`unibyte-string' call -- Doc
# 194 P2 measured both as broken for byte values >= 128 on this
# substrate (`nelisp--dns-u16-be''s own comment): every elisp-level
# string constructor treats its integer arguments as CODEPOINTS and
# UTF-8-encodes them, so a "byte" >= 128 built that way becomes two raw
# wire bytes, not one.  The test script reads each fixture back via
# `insert-file-contents-literally' (byte-clean, like `nelisp-socket-
# recv'), matching how a real response actually arrives.
#
# Against-the-bug (length-prefix/compression-pointer parsing
# specifically, per Doc 194's own P2 exit criterion text): a truncated
# response and an oversized RDLENGTH both signal the catchable,
# DNS-specific `nelisp-dns-error' through the real guarded parser
# (`nelisp--dns-parse-response'/`nelisp--dns-byte''s own bounds check on
# every read), contrasted with the SAME truncated buffer read through
# the raw, UNGUARDED native `string-byte' primitive this parser is built
# on -- measured to have NO bounds check at all (unlike `aref', which at
# least signals a generic `args-out-of-range'): `(string-byte buf 999)'
# on a 29-byte buffer returns a plain value with no error whatsoever,
# silently wrong rather than loudly wrong -- exactly the defect class a
# missing bounds check in this parser would produce, and why
# `nelisp--dns-byte' exists as the ONLY guard between a truncated
# response and reading out of bounds.  Positive: if this
# environment has TCP egress to the numeric resolver
# (checked with `/dev/tcp' exactly like `standalone-reader-tls-smoke'
# does for its own egress check, SKIPping gracefully rather than failing
# when this sandbox has none), a REAL DNS-over-TCP A-record lookup for a
# well-known hostname resolves to a plausible IPv4 literal and P0's own
# client path connects to it.
standalone-reader-dns-smoke: standalone-reader
	@mkdir -p target
	@printf '\022\064\201\200\000\001\000\001\000\000\000\000\007\145\170\141\155\160\154\145\003\143\157\155\000\000\001\000\001\300\014\000\001\000\001\000\000\001\054\000\004\135\270\330\042' \
	  > target/standalone-reader-dns-smoke-full.bin
	@printf '\022\064\201\200\000\001\000\001\000\000\000\000\007\145\170\141\155\160\154\145\003\143\157\155\000\000\001\000\001' \
	  > target/standalone-reader-dns-smoke-truncated.bin
	@printf '\022\064\201\200\000\001\000\001\000\000\000\000\007\145\170\141\155\160\154\145\003\143\157\155\000\000\001\000\001\300\014\000\001\000\001\000\000\001\054\377\377\135\270\330\042' \
	  > target/standalone-reader-dns-smoke-badrdlen.bin
	@printf '%s\n' \
	  '$(NELISP_PROCESS_ADAPTER_LOAD_1)' \
	  '$(NELISP_PROCESS_ADAPTER_LOAD_2)' \
	  '(defun nelisp-dns-smoke--slurp (f) (with-temp-buffer (insert-file-contents-literally f) (buffer-string)))' \
	  '(let* ((full (nelisp-dns-smoke--slurp "target/standalone-reader-dns-smoke-full.bin")) (truncated (nelisp-dns-smoke--slurp "target/standalone-reader-dns-smoke-truncated.bin")) (bad-rdlength (nelisp-dns-smoke--slurp "target/standalone-reader-dns-smoke-badrdlen.bin"))) (list (nelisp--dns-parse-response full) (condition-case e (nelisp--dns-parse-response truncated) (nelisp-dns-error (quote dns-error-caught))) (condition-case e (progn (string-byte truncated 999) (quote raw-unguarded-no-error)) (error (quote raw-unexpectedly-errored))) (condition-case e (nelisp--dns-byte truncated 999) (nelisp-dns-error (quote guarded-dns-error-caught))) (condition-case e (nelisp--dns-parse-response bad-rdlength) (nelisp-dns-error (quote dns-error-caught))) (nelisp--dns-skip-name full 12) (string-bytes (nelisp--dns-encode-query "example.com"))))' \
	  > target/standalone-reader-dns-smoke-parse.el
	@parse_out="$$($(STANDALONE_BIN) --load target/standalone-reader-dns-smoke-parse.el)"; \
	if [ "$$parse_out" != '("93.184.216.34" dns-error-caught raw-unguarded-no-error guarded-dns-error-caught dns-error-caught 25 31)' ]; then \
	  echo "[standalone-reader-dns-smoke] FAIL: wire-format parse/against-the-bug -> $$parse_out"; \
	  exit 1; \
	fi; \
	if ! timeout 6 bash -c 'exec 3<>/dev/tcp/1.1.1.1/53' 2>/dev/null; then \
	  echo "[standalone-reader-dns-smoke] PASS (parse+against-the-bug only): parse=$$parse_out; SKIP live A-record lookup, no egress to 1.1.1.1:53 in this sandbox"; \
	  exit 0; \
	fi; \
	printf '%s\n' \
	  '$(NELISP_PROCESS_ADAPTER_LOAD_1)' \
	  '$(NELISP_PROCESS_ADAPTER_LOAD_2)' \
	  '(setq nelisp-dns-resolver-ip "1.1.1.1")' \
	  '(let* ((ip (nelisp--dns-resolve-a "example.com")) (parts (split-string ip "\\.")) (nums (mapcar (lambda (s) (string-to-number s)) parts)) (plausible (and (= (length nums) 4) (not (memq nil (mapcar (lambda (n) (and (>= n 0) (<= n 255))) nums)))))) (let* ((cli (open-network-stream "web" nil ip 80))) (let ((status (process-status cli))) (delete-process cli) (list plausible status))))' \
	  > target/standalone-reader-dns-smoke-live.el; \
	live_out="$$(timeout 15 $(STANDALONE_BIN) --load target/standalone-reader-dns-smoke-live.el)"; \
	if [ "$$live_out" = "(t open)" ]; then \
	  echo "[standalone-reader-dns-smoke] PASS: parse+against-the-bug=$$parse_out; live A-record lookup + connect=$$live_out"; \
	else \
	  echo "[standalone-reader-dns-smoke] FAIL: live A-record lookup + connect -> $$live_out"; \
	  exit 1; \
	fi


# Doc 194 P3 exit criterion: the nonblocking primitives in isolation
# (`nelisp-socket-connect'/-accept's NOWAIT argument, `nelisp-socket-poll',
# `nelisp-socket-connect-error') -- no elisp-layer/poll-loop wiring yet
# (that is P4/P5, their own smokes below).  Against-the-bug: implementing
# this smoke is what FOUND the bug -- `nl_socket_poll_impl' was first
# written parameterizing `nl_os_process_poll_readable''s own literal
# `syscall-direct 230 ...' (this doc's own S3.3 item 2 text says to
# parameterize that exact pattern), which measured, under `strace -f'
# during this phase's implementation, to never reach a real `poll'
# syscall at all -- `230' is `clock_nanosleep' on the Linux x86_64 ABI,
# not `poll' (`7'); `nelisp-socket-poll' always answered nil (RED: a
# connected, writable socket read back not-ready, wall-clock ~2ms, no
# hang -- silently wrong rather than loudly wrong, worse than a hang for
# a smoke to catch). Fixed to `syscall-direct 7 ...' (GREEN, this
# target) -- `nl_socket_poll_impl''s own comment records the measurement;
# not this phase's scope to fix the pre-existing, unrelated
# `nl_os_process_poll_readable' bug this uncovered (the subprocess pipe-
# poll path, never called by P4/P5's own wiring).
#
# Positive 1 (connect, real listener): a NOWAIT connect to a real
# loopback listener returns a fd immediately (wall-clock bounded, no
# `accept-process-output'-style blocking call in between); `nelisp-
# socket-poll FD t TIMEOUT' later reports writable within the bound;
# `nelisp-socket-connect-error' reads 0.
# Positive 2 (connect, closed port): the SAME NOWAIT connect to a closed
# local port (1, privileged/unbound) ALSO returns a fd immediately
# (measured this phase: Linux's own nonblocking connect(2) to a closed
# LOOPBACK port returns -EINPROGRESS, not a synchronous -ECONNREFUSED --
# the refusal RST arrives asynchronously) -- `nelisp-socket-poll' reports
# writable once the refusal lands, and `nelisp-socket-connect-error'
# reads the real errno (111, ECONNREFUSED), never 0 and never hanging.
# Positive 3 (accept): a NOWAIT accept against an EMPTY listen queue
# returns the -1 sentinel immediately (not a hang, not a signal); the
# SAME call once a connection is actually pending (an ordinary blocking
# `nelisp-socket-connect' from THIS phase's own test harness) returns a
# real fd.
# Unsupported-primitive proof for the three new names ("or the existing
# target-swap harness Phase 1's own gate uses", this doc's own P3 exit
# text): `test/nelisp-standalone-target-test.el's
# `nelisp-standalone-target-socket-dispatch-non-linux-x86-64-unsupported'
# -- ERT, source-level (`nelisp-standalone--target' let-bound per
# non-linux-x86_64 target, ONE process, no cross-arch binary build/exec
# needed since a Windows PE or aarch64 ELF built on this x86_64 Linux
# host could not run here anyway) -- covers this alongside Phase 1's own
# six names, closing a gap that predates this phase (Phase 1 itself had
# no such ERT-level proof for its own six).
# Doc 194 P4 exit criterion: wiring P3's nonblocking primitives into the
# process adapter's ONE poll loop, and real `:nowait' support in
# `make-network-process'.  Against-the-bug: RED is the P0-P2 guard this
# EXACT smoke's own "refused" case in `standalone-reader-network-process-
# smoke' used to assert (`:nowait t' unconditionally `error's) -- that
# smoke was updated alongside this one (same commit) to assert the
# OPPOSITE, GREEN claim (`process-status' reads `connect', no signal);
# this target is the dedicated positive proof P4's own exit criterion
# asks for.
#
# Case 1 (success): `:nowait t' against a real loopback listener returns
# BEFORE the connect completes (wall-clock bounded, no `accept-process-
# output' call yet); `process-status' reads `connect' immediately;
# `accept-process-output' later drives the SAME shared poll loop to fire
# the sentinel with `open\n' -- measured against real GNU Emacs 30.1
# during this phase's implementation (`(let* ((srv (make-network-process
# :server t ...)) (cli (make-network-process :nowait t ...))) ...)',
# this doc's own reproducible probe) -- and `process-status' reads
# `open' afterward.
# Case 2 (failure): the SAME against a closed port -- `process-status'
# reads `connect' immediately, then `failed' after `accept-process-
# output', with the sentinel fired EXACTLY `(format "failed with code
# %d\n" ERRNO)' -- measured the SAME way against real Emacs 30.1: a bare
# `failed\n' (doc 194's own S1.3/S3.3 prose) is NOT the real string; only
# case 2's own errno differs run to run in general (this smoke pins it
# to the deterministic ECONNREFUSED=111 a closed local port always
# gives), so the smoke asserts the exact formatted string, not just a
# prefix.
# Case 3 (shared loop, two DIFFERENT process kinds, doc 184 P2's own
# "does not drain the other's queue" contract extended across kinds):
# a concurrent ordinary subprocess and a `:nowait' network connect,
# polled through the SAME `accept-process-output' call, each become
# ready independently -- the subprocess's sentinel fires with its own
# real status and the network connect still completes to `open'.
standalone-reader-network-process-nowait-smoke: standalone-reader
	@mkdir -p target
	# Windows 11 measured refused-connect readiness at 2.03-2.05s.  Give the
	# adapter's outer poll loop 3s there (0.95s margin); Linux keeps its proven
	# 2s bound.  This remains a bounded completion assertion, never a hang mask.
	@printf '%s\n' \
	  '$(NELISP_PROCESS_ADAPTER_LOAD_1)' \
	  '$(NELISP_PROCESS_ADAPTER_LOAD_2)' \
	  '(let* (msgs (lfd (nelisp-socket-listen "127.0.0.1" 56010)) (cli (make-network-process :name "cli" :host "127.0.0.1" :service 56010 :nowait t :sentinel (lambda (_p m) (push m msgs)))) (status0 (process-status cli))) (accept-process-output cli 2) (let ((result (list status0 (process-status cli) (reverse msgs)))) (delete-process cli) (nelisp-socket-close lfd) result))' \
	  > target/standalone-reader-network-process-nowait-smoke-ok.el
	@printf '%s\n' \
	  '$(NELISP_PROCESS_ADAPTER_LOAD_1)' \
	  '$(NELISP_PROCESS_ADAPTER_LOAD_2)' \
	  '(let* (msgs (cli (make-network-process :name "cli" :host "127.0.0.1" :service 1 :nowait t :sentinel (lambda (_p m) (push m msgs)))) (status0 (process-status cli))) (accept-process-output cli $(if $(filter windows%,$(STANDALONE_GATE_TARGET)),3,2)) (let ((result (list status0 (process-status cli) (reverse msgs)))) (ignore-errors (delete-process cli)) result))' \
	  > target/standalone-reader-network-process-nowait-smoke-refused.el
	@printf '%s\n' \
	  '$(NELISP_PROCESS_ADAPTER_LOAD_1)' \
	  '$(NELISP_PROCESS_ADAPTER_LOAD_2)' \
	  '(let* (net-msgs sub-msgs (lfd (nelisp-socket-listen "127.0.0.1" 56011)) (net (make-network-process :name "net" :host "127.0.0.1" :service 56011 :nowait t :sentinel (lambda (_p m) (push m net-msgs)))) (sub (make-process :name "echo" :command $(PROCESS_ADAPTER_EXIT0_COMMAND) :sentinel (lambda (_p m) (push m sub-msgs))))) (accept-process-output nil 2) (accept-process-output nil 2) (let ((result (list (process-status net) (reverse net-msgs) (process-live-p sub) (reverse sub-msgs)))) (delete-process net) (nelisp-socket-close lfd) result))' \
	  > target/standalone-reader-network-process-nowait-smoke-mixed.el
	@ok_out="$$($(STANDALONE_BIN) --load target/standalone-reader-network-process-nowait-smoke-ok.el)"; \
	refused_out="$$($(STANDALONE_BIN) --load target/standalone-reader-network-process-nowait-smoke-refused.el)"; \
	mixed_out="$$($(STANDALONE_BIN) --load target/standalone-reader-network-process-nowait-smoke-mixed.el)"; \
	ok_expect="$$(printf '(connect open ("open\n"))')"; \
	refused_expect="$$(printf '(connect failed ("failed with code 111\n"))')"; \
	mixed_expect="$$(printf '(open ("open\n") nil ("finished\n"))')"; \
	if [ "$$ok_out" = "$$ok_expect" ] && \
	   [ "$$refused_out" = "$$refused_expect" ] && \
	   [ "$$mixed_out" = "$$mixed_expect" ]; then \
	  echo "[standalone-reader-network-process-nowait-smoke] PASS: ok(status0,status1,sentinel)=$$ok_out refused(status0,status1,sentinel)=$$refused_out mixed(net-status,net-sentinel,sub-live,sub-sentinel)=$$mixed_out"; \
	else \
	  echo "[standalone-reader-network-process-nowait-smoke] FAIL: ok=$$ok_out (want $$ok_expect) refused=$$refused_out (want $$refused_expect) mixed=$$mixed_out (want $$mixed_expect)"; \
	  exit 1; \
	fi

# Doc 194 P5 exit criterion: `:server t', auto-accept, `:log'.
# Against-the-bug: RED is the P0-P2 guard `standalone-reader-network-
# process-smoke''s own "refused" case used to assert for `:server t'
# (unconditional `error') -- updated alongside this target (same
# commit) to assert the OPPOSITE, GREEN claim (`process-status' reads
# `listen', no signal); this target is the dedicated positive proof
# P5's own exit criterion asks for.
#
# A real multi-client scenario: two clients connect to ONE `:server t'
# listener in the same poll window (both `make-network-process' calls
# happen before the FIRST `accept-process-output', so both connect
# attempts are already in flight/queued when the poll loop first looks
# -- the cooperative-concurrency case this doc's own P5 section flags:
# this substrate's poll loop is single-thread/cooperative, so the two
# accepts are serviced ONE PER POLL PASS, not literally simultaneously
# -- `accept-process-output' is called twice below for exactly this
# reason, and both children are confirmed live either way).  The live
# poll-set registry (this runtime's own `process-list' equivalent, Doc
# 194 S4 P5's own text) shows FIVE processes for two clients -- the
# listener, each CLIENT-side connection (`c1'/`c2', synchronous
# `make-network-process' calls, already `nelisp-process-adapter--live'
# members in their own right, Doc 194 P0), and each SERVER-side
# auto-accepted child (this phase's own addition) -- matching real Emacs
# 30.1's measured `process-list' shape exactly (this doc's own
# measurement while implementing this phase: `srv'/`c1'/`c2' plus one
# `srv <HOST:PORT>' child per client, five entries for two clients too),
# modulo the child NAME suffix (`<fd:N>', not `<HOST:PORT>' -- Phase 1's
# `nelisp-socket-accept' requests no peer address at all, S1.1, a
# substrate limitation this phase does not fix, recorded in
# `nelisp-process-adapter--drain-and-fire-network''s
# own comment).  Each child's `:filter' (inherited from the listener,
# `eq'-identical to it, matching the SAME real-Emacs measurement) fires
# independently as each client sends its OWN data -- proving the two
# connections are not cross-wired.  `:log' is called `(SERVER CHILD
# MESSAGE)' once per accept.  Deleting the SERVER transitions ONLY its
# own status to `closed' -- the already-accepted children are
# unaffected (measured against real Emacs 30.1: "connection procs
# outlive it", this doc's own P5 exit criterion text, confirmed rather
# than assumed) -- this smoke checks the children are still `open'
# immediately after `delete-process' on the server, before cleaning
# them up itself.
standalone-reader-network-process-server-smoke: standalone-reader
	@mkdir -p target
	@printf '%s\n' \
	  '$(NELISP_PROCESS_ADAPTER_LOAD_1)' \
	  '$(NELISP_PROCESS_ADAPTER_LOAD_2)' \
	  '(defun nps-name (p) (aref p 1))' \
	  '(let* (received log-calls (srv (make-network-process :name "srv" :server t :service 56020 :filter (lambda (p s) (push (cons (nps-name p) s) received)) :log (lambda (server client msg) (push (list (nps-name server) (nps-name client) msg) log-calls)))) (status0 (process-status srv)) (c1 (make-network-process :name "c1" :host "127.0.0.1" :service 56020)) (c2 (make-network-process :name "c2" :host "127.0.0.1" :service 56020))) (accept-process-output nil 1) (accept-process-output nil 1) (let* ((live (copy-sequence nelisp-process-adapter--live)) (children (seq-filter (lambda (p) (not (memq p (list c1 c2 srv)))) live)) (proc-count (length live))) (process-send-string c1 "hello-from-c1") (process-send-string c2 "hello-from-c2") (accept-process-output nil 1) (accept-process-output nil 1) (let* ((children-open (not (memq nil (mapcar (lambda (p) (eq (aref p 2) (quote open))) children)))) (filters-shared (not (memq nil (mapcar (lambda (p) (eq (process-get p :filter) (process-get srv :filter))) children)))) (recv-both (and (assoc-string "hello-from-c1" (mapcar (lambda (x) (cdr x)) received) t) (assoc-string "hello-from-c2" (mapcar (lambda (x) (cdr x)) received) t))) (server-status-before-delete (process-status srv))) (delete-process srv) (let* ((server-status-after (process-status srv)) (children-still-open (not (memq nil (mapcar (lambda (p) (eq (aref p 2) (quote open))) children))))) (delete-process c1) (delete-process c2) (list status0 proc-count (length children) children-open filters-shared (if recv-both t nil) (= (length log-calls) 2) server-status-before-delete server-status-after children-still-open)))))' \
	  > target/standalone-reader-network-process-server-smoke.el
	@out="$$($(STANDALONE_BIN) --load target/standalone-reader-network-process-server-smoke.el)"; \
	if [ "$$out" = "(listen 5 2 t t t t listen closed t)" ]; then \
	  echo "[standalone-reader-network-process-server-smoke] PASS: (server-status0,live-count,child-count,children-open,filters-shared,both-received,log-calls==2,server-status-before-delete,server-status-after-delete,children-still-open-after-server-delete)=$$out"; \
	else \
	  echo "[standalone-reader-network-process-server-smoke] FAIL: $$out"; \
	  exit 1; \
	fi

standalone-reader-nonblocking-socket-smoke: standalone-reader
	@mkdir -p target
	# Windows 11 measured WSAPoll refusal readiness at 2.03-2.05s (select was
	# identical).  The Windows readiness bound is therefore 3.0s, about 0.95s
	# of margin, while its poll timeout is 5s so a full timeout still fails the
	# bound loudly.  NOWAIT connect itself stays under 1.0s on every target.
	#
	# "Still fails loudly" was run, not asserted -- a loosened constant with
	# only an argument behind it is what this repo keeps getting burned by.
	# Injection: the windows `nl_socket_poll_impl' asks WSAPoll for no events
	# at all (`events' 0), so it can never report ready and must sit out its
	# whole timeout.  Clean: gate PASS, probe elapsed1=0.001 ready=t
	# elapsed2=2.040.  Injected: gate FAIL on BOTH cases
	# (connect-ok=(t t nil nil 0) connect-refused=(t t nil nil t)), probe
	# elapsed1=0.000 ready=nil elapsed2=5.009 -- past the 3.0s bound, which is
	# the thing being checked.  Restored: PASS again.  This is now the
	# windows-x86_64-scoped row in `tools/gate-mutations.txt'; target scoping keeps
	# the injection from sitting unreachable on Linux (failure mode 1 there).
	@printf '%s\n' \
	  '(let* ((start (float-time)) (lfd (nelisp-socket-listen "127.0.0.1" 55990 t)) (cfd (nelisp-socket-connect "127.0.0.1" 55990 t)) (elapsed1 (- (float-time) start)) (ready (nelisp-socket-poll cfd t 3000)) (elapsed2 (- (float-time) start)) (cerr (nelisp-socket-connect-error cfd))) (nelisp-socket-close cfd) (nelisp-socket-close lfd) (list (< elapsed1 1.0) (integerp cfd) ready (< elapsed2 1.0) cerr))' \
	  > target/standalone-reader-nonblocking-socket-smoke-connect-ok.el
	@printf '%s\n' \
	  '(let* ((start (float-time)) (cfd (nelisp-socket-connect "127.0.0.1" 1 t)) (elapsed1 (- (float-time) start)) (ready (nelisp-socket-poll cfd t $(if $(filter windows%,$(STANDALONE_GATE_TARGET)),5000,3000))) (elapsed2 (- (float-time) start)) (cerr (nelisp-socket-connect-error cfd))) (nelisp-socket-close cfd) (list (< elapsed1 1.0) (integerp cfd) ready (< elapsed2 $(if $(filter windows%,$(STANDALONE_GATE_TARGET)),3.0,1.0)) (/= cerr 0)))' \
	  > target/standalone-reader-nonblocking-socket-smoke-connect-refused.el
	@printf '%s\n' \
	  '(let* ((lfd (nelisp-socket-listen "127.0.0.1" 55991 t)) (empty (nelisp-socket-accept lfd t)) (cfd (nelisp-socket-connect "127.0.0.1" 55991)) (sfd (nelisp-socket-accept lfd t))) (nelisp-socket-close cfd) (nelisp-socket-close sfd) (nelisp-socket-close lfd) (list empty (integerp sfd) (>= sfd 0)))' \
	  > target/standalone-reader-nonblocking-socket-smoke-accept.el
	@ok_out="$$($(STANDALONE_BIN) --load target/standalone-reader-nonblocking-socket-smoke-connect-ok.el)"; \
	refused_out="$$($(STANDALONE_BIN) --load target/standalone-reader-nonblocking-socket-smoke-connect-refused.el)"; \
	accept_out="$$($(STANDALONE_BIN) --load target/standalone-reader-nonblocking-socket-smoke-accept.el)"; \
	if [ "$$ok_out" = "(t t t t 0)" ] && \
	   [ "$$refused_out" = "(t t t t t)" ] && \
	   [ "$$accept_out" = "(-1 t t)" ]; then \
	  echo "[standalone-reader-nonblocking-socket-smoke] PASS: connect-ok(bounded,fd,writable,bounded,err0)=$$ok_out connect-refused(bounded,fd,writable,bounded,err!=0)=$$refused_out accept(empty=-1,fd,fd>=0)=$$accept_out"; \
	else \
	  echo "[standalone-reader-nonblocking-socket-smoke] FAIL: connect-ok=$$ok_out connect-refused=$$refused_out accept=$$accept_out"; \
	  exit 1; \
	fi

# Doc 184 P3: the `--repl' blank-line idle pump. Retired from its
# original RED/GREEN split as of integration/wave6 phase 2A: this smoke
# used to prove `nelisp-async-core.el' ALONE left the baked-in no-op
# `nelisp--repl-idle-pump' in effect (no TICK) while ALSO loading
# nelisp-process-adapter.el upgraded it to a real bounded pump (TICK).
# Phase 2A wired both files' source into the default prelude
# (`nelisp-standalone--reader-repl-prelude-source'), so blank-Enter
# pumps a due timer on every standalone build now, with NOTHING
# `--load'ed -- the old RED case (no adapter loaded) now also prints
# TICK, which is the fix working, not a failure (confirmed by a real
# run: red_out=TICK). This target now asserts the default-bootstrap
# claim directly: NO explicit load, blank-Enter pumps (TICK); a
# `--load' batch run of the identical timer form still never reaches
# `nl_repl_loop' at all, matching Emacs's own batch-mode contract of
# not pumping outside an explicit wait -- that half is unaffected by
# the wiring and stays the real regression guard.
standalone-reader-repl-idle-pump-smoke: standalone-reader
	@mkdir -p target
	@printf '%s\n' \
	  '(run-at-time 0.05 nil (lambda () (princ "TICK")))' \
	  '(quote batch-no-pump)' \
	  > target/standalone-reader-repl-idle-pump-smoke-batch.el
	@noload_out="$$(printf '(run-at-time 0.05 nil (lambda () (princ "TICK")))\n\n\n\n\n' | timeout 5 $(STANDALONE_BIN) --repl --no-prompt --no-print 2>&1)"; \
	batch_out="$$($(STANDALONE_BIN) --load target/standalone-reader-repl-idle-pump-smoke-batch.el 2>&1)"; \
	case "$$noload_out" in \
	  *TICK*) noload_ok=1 ;; \
	  *) noload_ok=0 ;; \
	esac; \
	case "$$batch_out" in \
	  *TICK*) batch_ok=0 ;; \
	  *) batch_ok=1 ;; \
	esac; \
	if [ "$$noload_ok" = 1 ] && [ "$$batch_ok" = 1 ]; then \
	  echo "[standalone-reader-repl-idle-pump-smoke] PASS: default binary, NO --load of either new file -- REPL blank-Enter pumps a due timer (TICK), batch --eval of the same timer never reaches the pump at all -> noload=$$noload_out batch=$$batch_out (Doc 184 P3 ships in the default bootstrap, integration/wave6 phase 2A)"; \
	else \
	  echo "[standalone-reader-repl-idle-pump-smoke] FAIL: noload-repl-had-tick=$$([ $$noload_ok = 1 ] && echo YES || echo NO-BAD) batch-had-tick=$$([ $$batch_ok = 1 ] && echo no || echo YES-BAD)"; \
	  echo "  noload_out=$$noload_out"; \
	  echo "  batch_out=$$batch_out"; \
	  exit 1; \
	fi

# Fast focused loop for Doc 142 gate-6 REAL-RUNTIME in-process native exec.
# Builds/relinks target/nelisp, then runs the embedded `--neln-selftest'
# loader path against the REAL reader-linked `nelisp_aot_builtin_call1`.
# `nl_neln_demo_exec' is the real NeLN demo only on linux-x86_64; every
# other target gets the `125' placeholder
# (`nelisp-standalone--reader-neln-demo-source', scripts/nelisp-standalone-
# build.el).  The in-tree Emacs-side smoke has carried that distinction
# since it was written -- `nelisp-standalone--reader-neln-selftest-smoke'
# expects `(if (eq nelisp-standalone--target (quote linux-x86_64)) 42 125)'
# -- but this gate hardcoded 42, so it read as a failing runtime on every
# other target.  Same rule, stated the same way, in both places now.
standalone-reader-realrt-smoke: standalone-reader
	@mkdir -p target
	@stdout_file=target/standalone-reader-realrt-smoke.out; \
	rm -f "$$stdout_file"; \
	set +e; \
	$(STANDALONE_BIN) --neln-selftest >"$$stdout_file"; \
	rc=$$?; \
	set -e; \
	out="$$(cat "$$stdout_file")"; \
	want=$(if $(filter linux-x86_64,$(STANDALONE_GATE_TARGET)),42,125); \
	if [ "$$rc" -eq "$$want" ] && [ -z "$$out" ]; then \
	  echo "[standalone-reader-realrt-smoke] PASS: exit=$$rc (expected $$want for $(STANDALONE_GATE_TARGET)) stdout=<empty>"; \
	else \
	  echo "[standalone-reader-realrt-smoke] FAIL: exit=$$rc stdout=$$out (expected $$want for $(STANDALONE_GATE_TARGET))"; \
	  exit 1; \
	fi

# Doc 142 section 6.4: the general in-process loader.  Where
# `standalone-reader-realrt-smoke' runs ONE function whose bytes and extern
# addresses were baked into the reader at build time, this compiles a set of
# artifacts and has the reader read, map and call them at run time through
# `lisp/nelisp-native-load.el' -- interpreted elisp, no linker, no cc, no
# subprocess.  Covers both calling conventions, arity 0 through 6, and
# non-integer values in and out.
neln-loader-test: standalone-reader
	@mkdir -p target/neln-loader
	NELISP_ARTIFACT_DIR=$(CURDIR)/target/neln-loader \
	  $(EMACS) --batch -Q -L lisp -L src -L scripts \
	  --eval '(setq load-prefer-newer t)' \
	  -l nelisp-native-load-fixtures \
	  -f nelisp-native-load-fixtures-main
	@prelude=target/neln-loader/prelude.el; \
	{ echo '(load "$(CURDIR)/lisp/nelisp-native-load.el")'; \
	  echo '(defvar nelisp-native-load-driver-dir "$(CURDIR)/target/neln-loader")'; \
	  echo '(load "$(CURDIR)/test/nelisp-native-load-driver.el")'; \
	} > "$$prelude"; \
	./target/nelisp --load "$$prelude"

# The loader driver states each expected answer, so it only catches
# shapes someone thought of first.  This one computes the answer: the
# same source runs through the interpreter and as native code inside one
# reader process, and the two are compared.
aot-differential: standalone-reader
	@mkdir -p target/aot-differential
	NELISP_DIFF_DIR=$(CURDIR)/target/aot-differential \
	  $(EMACS) --batch -Q -L lisp -L src -L scripts \
	  --eval '(setq load-prefer-newer t)' \
	  -l nelisp-aot-differential-cases \
	  -f nelisp-aot-differential-main
	@echo '(princ "reader-ok\n")' > target/aot-differential/smoke.el; \
	if ! ./target/nelisp --load target/aot-differential/smoke.el 2>&1 | grep -q reader-ok; then \
	  echo "[diff] the reader does not run -- every case would 'crash' and say nothing about the cases"; \
	  echo "[diff] a compiler change can build a binary that dies on startup; check that first"; \
	  exit 1; \
	fi
	@count=$$(cat target/aot-differential/count.txt); size=1; \
	chunks=$$(( ($$count + $$size - 1) / $$size )); failed=0; \
	echo "[diff] $$count cases in $$chunks chunks of $$size"; \
	for chunk in $$(seq 0 $$(($$chunks - 1))); do \
	  prelude=target/aot-differential/prelude-$$chunk.el; \
	  { echo '(load "$(CURDIR)/lisp/nelisp-native-load.el")'; \
	    echo '(load "$(CURDIR)/target/aot-differential/cases.el")'; \
	    echo '(defvar nelisp-aot-differential-dir "$(CURDIR)/target/aot-differential")'; \
	    echo "(defvar nelisp-aot-differential-chunk $$chunk)"; \
	    echo "(defvar nelisp-aot-differential-chunk-size $$size)"; \
	    echo '(load "$(CURDIR)/test/nelisp-aot-differential-driver.el")'; \
	  } > "$$prelude"; \
	  out=$$(./target/nelisp --load "$$prelude" 2>&1); \
	  printf '%s\n' "$$out"; \
	  summary=$$(printf '%s\n' "$$out" | grep "^differential chunk $$chunk:"); \
	  if [ -z "$$summary" ]; then \
	    died=$$(printf '%s\n' "$$out" | grep '^RUN ' | tail -1); \
	    echo "[diff] CRASH $${died#RUN } -- the reader died running it"; \
	    failed=1; \
	  else \
	    case "$$summary" in *", 0 wrong,"*) : ;; *) failed=1 ;; esac; \
	  fi; \
	done; \
	test $$failed -eq 0

# Fast focused loop for REPL work.  Builds/relinks target/nelisp with the
# incremental unit cache, then runs only the REPL smoke used by the full reader
# test.
standalone-reader-repl-smoke:
	$(EMACS) --batch -Q -L lisp -L src -L scripts \
	  --eval '(setq load-prefer-newer t)' \
	  -l nelisp-standalone-build -f nelisp-standalone-reader-repl-test

# Fast focused loop for the reader-completeness / missing-file-unification /
# depth-guard defect class.  Builds/relinks target/nelisp with the
# incremental unit cache, then runs only the table-driven malformed-input
# smoke used by the full reader test.
standalone-reader-malformed-input-smoke:
	$(EMACS) --batch -Q -L lisp -L src -L scripts \
	  --eval '(setq load-prefer-newer t)' \
	  -l nelisp-standalone-build -f nelisp-standalone-reader-malformed-input-test

# Fast focused loop for Doc 180 Phase 1 (form-located errors): the
# uncaught-error printer names FILE, top-level form #N and a byte/line
# offset.  Builds/relinks target/nelisp with the incremental unit cache,
# then runs only the against-the-bug smoke used by the full reader test.
standalone-reader-form-location-smoke:
	$(EMACS) --batch -Q -L lisp -L src -L scripts \
	  --eval '(setq load-prefer-newer t)' \
	  -l nelisp-standalone-build -f nelisp-standalone-reader-form-location-test

# Fast focused loop for Doc 180 Phase 2 item 1 (the gc-context frame stack's
# pop/depth-cap desync).  Builds/relinks target/nelisp with the incremental
# unit cache, then runs only the against-the-bug smoke used by the full
# reader test.
standalone-reader-frame-stack-pop-desync-smoke:
	$(EMACS) --batch -Q -L lisp -L src -L scripts \
	  --eval '(setq load-prefer-newer t)' \
	  -l nelisp-standalone-build -f nelisp-standalone-reader-frame-stack-pop-desync-test

# Fast focused loop for Doc 180 Phase 2 item 3 (the bounded backtrace on an
# uncaught error).  Builds/relinks target/nelisp with the incremental unit
# cache, then runs only the against-the-bug smoke used by the full reader
# test.
standalone-reader-bounded-backtrace-smoke:
	$(EMACS) --batch -Q -L lisp -L src -L scripts \
	  --eval '(setq load-prefer-newer t)' \
	  -l nelisp-standalone-build -f nelisp-standalone-reader-bounded-backtrace-test
# Fast focused loop for the socket primitives (Doc 184 follow-on) and Task A's
# nelisp-unsupported-primitive fix.  Builds/relinks target/nelisp with the
# incremental unit cache, then runs only the loopback round-trip + two
# catchable-error negatives used by the full reader test.
standalone-reader-socket-smoke:
	$(EMACS) --batch -Q -L lisp -L src -L scripts \
	  --eval '(setq load-prefer-newer t)' \
	  -l nelisp-standalone-build -f nelisp-standalone-reader-socket-test

# Fast focused loop for the Doc 194 IPv6 phase (P7): AF_INET6 sockaddr_in6
# construction, IPv6 literal parsing (native raw primitives AND the
# separate pure-elisp parser), AAAA DNS resolution, and the
# make-network-process/open-network-stream :family 'ipv6 path -- ALL
# alongside a same-process IPv4 round trip proving the addition did not
# disturb the unchanged IPv4 path.  Builds/relinks target/nelisp with the
# incremental unit cache, then runs only this smoke.  Same shape as
# `standalone-reader-socket-smoke' immediately above (Doc 194 IPv6 phase's
# own precedent, not the shell-heredoc pattern the network-process-*
# smokes further down this file use).
standalone-reader-ipv6-socket-smoke:
	$(EMACS) --batch -Q -L lisp -L src -L scripts \
	  --eval '(setq load-prefer-newer t)' \
	  -l nelisp-standalone-build -f nelisp-standalone-reader-ipv6-socket-test

# Prelude-load breadth test (Wave-1 (A)+(B)).  Builds the reader binary, then
# runs it on  scripts/nelisp-stdlib-prelude.el  followed by a breadth test that
# exercises cond / dolist / nth / plist-get / backquote (all backed by the
# Wave-1 (B) breadth primitives), asserting exit == 42.  The prelude is just a
# loadable .el: the binary loads it then user code.  To use it by hand:
#   cat scripts/nelisp-stdlib-prelude.el yourfile.el > /tmp/prog.el
#   target/nelisp /tmp/prog.el   # exit = last form's value
standalone-reader-prelude-test:
	NELISP_STANDALONE_TARGET=$(STANDALONE_GATE_TARGET) $(EMACS) --batch -Q -L lisp -L src -L scripts \
	  --eval '(setq load-prefer-newer t)' \
	  -l nelisp-standalone-build -f nelisp-standalone-reader-prelude-test

# Zero-Rust standalone reader distribution.  Builds a short `bin/nelisp`
# (`bin/nelisp.exe` for windows-x86_64) tarball for the requested platform.
#   make standalone-tarball PLATFORM=linux-x86_64
#   make standalone-tarball PLATFORM=macos-aarch64
#   make standalone-tarball-verify PLATFORM=linux-x86_64
STANDALONE_VERSION ?= $(shell tr -d " \t\n\r" < $(CURDIR)/VERSION 2>/dev/null || echo v1.2.0)
standalone-tarball:
	@./tools/build-standalone-tarball.sh $(STANDALONE_VERSION) $(PLATFORM) --emacs "$(EMACS)"

standalone-tarball-verify:
	@./tools/verify-standalone-tarball.sh $(STANDALONE_VERSION) $(PLATFORM)

stage-d-v2-tarball:
	@./tools/build-bundled-tarball.sh $(RELEASE_VERSION) $(PLATFORM)

stage-d-v2-tarball-verify:
	@./tools/verify-bundled-tarball.sh $(RELEASE_VERSION) $(PLATFORM)
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
# CONCURRENTLY (fork(2) workers, each running the full AOT compiler, COW-
# isolated so no shared-state race), joined via a MAP_SHARED atomic counter.
standalone-parallel-compile-test:
	./tools/parallel-compile-test.sh

# Doc 140 chunked-arena GROWTH pressure test: build the standalone reader
# with an 8 MiB first chunk (< boot footprint) so allocation overflows it
# and MUST grow a second chunk; assert chunk-count > 1 and that a value
# escaping a `let' body survives the growth.  Proves pressure is handled by
# chunk growth, not a fixed-size reservation.
standalone-chunk-growth-test:
	@EMACS="$(EMACS)" ./tools/chunk-growth-test.sh

# Doc 152 Stage 4c/4d/5: the default-on mid-form collector must return wholly-
# free growth chunks to Linux and reach a steady RSS plateau without an
# explicit arm, while the checked poison-on-free run remains sound.  The
# implementation under test is pure Elisp AOT DSL; Python is only the host
# wait4(2)/ru_maxrss measurement driver.
.PHONY: standalone-midform-gc-bounded
standalone-midform-gc-bounded: standalone-reader
	@NELISP_STANDALONE_TARGET=$(STANDALONE_GATE_TARGET) $(EMACS) --batch -Q -L lisp -L src -L scripts -l nelisp-standalone-build \
	  --eval '(kill-emacs (if (nelisp-standalone--target-runnable-on-host-p) 0 3))' \
	  >/dev/null 2>&1; \
	host_rc=$$?; \
	if [ "$$host_rc" = 3 ]; then \
	  echo "GATE-SKIP target $(STANDALONE_GATE_TARGET) cannot run on this host"; \
	  exit 0; \
	fi; \
	if [ "$$host_rc" != 0 ]; then \
	  echo "standalone-midform-gc-bounded: target runnable predicate failed"; \
	  exit "$$host_rc"; \
	fi; \
	PYTHONDONTWRITEBYTECODE=1 python3 tools/nelisp-midform-gc-bounded.py ./target/nelisp


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

# Doc 171 G4: native artifact proof that transparent self-tail TCO
# keeps pace with the handwritten `nl-loop' form of the same algorithm.
bench-aot-tco:
	$(EMACS) --batch -Q -L lisp -L src -L bench -L packages/nl-prelude/src \
	  --eval '(setq load-prefer-newer t)' \
	  -l nelisp-aot-tco-bench \
	  -f nelisp-aot-tco-bench-batch

# Doc 187 P4: continuously cap the cost of opt-in checked arithmetic on a
# compiled tight `+' loop.  The harness also requires the checked and
# unchecked native-object hashes to differ, so a no-op flag fails
# structurally rather than depending on a timing delta.  Native AOT execution
# is Linux x86_64-only; other hosts print the gate contract's reasoned skip.
bench-aot-checked-arith:
	$(EMACS) --batch -Q -L lisp -L src -L bench \
	  --eval '(setq load-prefer-newer t)' \
	  -l nelisp-aot-checked-arith-bench \
	  -f nelisp-aot-checked-arith-bench-batch

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
