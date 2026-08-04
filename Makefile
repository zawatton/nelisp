.PHONY: test test-fast test-parallel test-one wasm-smoke wasm-runtime-image-smoke wasm-dtw-skeleton-smoke wasm-dtw-transpile wasm-dtw-compile wasm-dtw-smoke wasm-dtw-site wasm-dtw-site-smoke compile clean all bench gc-bench actor-bench soak soak-1h soak-full soak-worker \
        sqlite-module sqlite-module-clean \
        release-artifact release-checksum soak-blocker soak-post-ship \
        bench-actual bench-allocator bench-allocator-heavy \
        stage-d-tarball stage-d-v2-tarball stage-d-v2-tarball-verify \
        standalone-tarball standalone-tarball-verify \
        verify-elisp-fixtures \
        standalone-eval standalone-eval-clean standalone-eval-test standalone-eval-j \
        standalone-reader standalone-reader-test standalone-reader-load-smoke standalone-reader-fmt-smoke standalone-reader-prelude-equal-reload-smoke standalone-reader-declare-strip-smoke standalone-reader-nested-backquote-macro-smoke standalone-reader-derived-mode-shape-smoke standalone-reader-pcase-quote-literal-smoke standalone-reader-catch-throw-tag-smoke standalone-reader-cond-let-shape-smoke standalone-reader-ffi-smoke standalone-reader-tls-smoke standalone-reader-process-smoke standalone-reader-realrt-smoke standalone-reader-repl-smoke standalone-reader-prelude-test standalone-reader-intern-soft-smoke standalone-reader-intern-soft-loop-smoke standalone-selfhost-test standalone-selfhost-mt-test standalone-parallel-compile-test standalone-chunk-growth-test \
        standalone-reader-mod-float-smoke standalone-reader-match-data-smoke standalone-reader-current-time-smoke \
        nelisp-performance-gate nelisp-nelix-command-gate nelisp-native-artifact-gate nelisp-nelix-native-hot-gate \
        nelisp-nelix-operational-gate \
        nelisp-runtime-image-cache-gate nelisp-source-command-substrate-gate

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

# Fast TDD loop: same test load graph as `test' but skips `clean';
# use `test' as the clean verification gate before trusting results.
test-fast:
	$(EMACS) --batch -Q -L lisp -L src -L test -L bench \
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
	     :arch 'wasm32 :format 'wasm))"
	node tools/wasm-driver.mjs target/wasm-smoke/f.wasm f 42
	node tools/wasm-driver.mjs target/wasm-smoke/f-locals.wasm f 9

wasm-runtime-image-smoke:
	mkdir -p target/wasm-runtime-image
	HOME="$(CURDIR)" XDG_CONFIG_HOME="$(CURDIR)" $(EMACS) --batch -Q -L lisp -L src \
	  --eval '(setq load-prefer-newer t)' \
	  --eval "(progn \
	    (require 'nelisp-artifact) \
	    (compile-runtime-image \
	     '(\"compile-runtime-image\" \"--kind\" \"neln\" \
	       \"--target\" \"wasm32-wasi\" \
	       \"--input\" \"tools/wasm-runtime-image-p3c.nlri\" \
	       \"--output\" \"target/wasm-runtime-image/runtime-image.wasm\")))"
	node tools/wasm-driver.mjs target/wasm-runtime-image/runtime-image.wasm _start 3

wasm-dtw-skeleton-smoke:
	node tools/wasm-proofs/p4-run-all.mjs

wasm-dtw-transpile:
	mkdir -p target/wasm-dtw
	node tools/wasm-dtw-p4b/transpile-slice.mjs

wasm-dtw-compile: wasm-dtw-transpile
	HOME="$(CURDIR)" XDG_CONFIG_HOME="$(CURDIR)" $(EMACS) --batch -Q -L lisp -L src \
	  --eval '(setq load-prefer-newer t)' \
	  --eval "(progn \
	    (require 'nelisp-artifact) \
	    (compile-runtime-image \
	     '(\"compile-runtime-image\" \"--kind\" \"neln\" \
	       \"--target\" \"wasm32-wasi\" \
	       \"--input\" \"target/wasm-dtw/dtw-p4b.nlri\" \
	       \"--output\" \"target/wasm-dtw/dtw.wasm\")))"

wasm-dtw-smoke: wasm-dtw-compile
	node tools/wasm-dtw-p4b/smoke.mjs target/wasm-dtw/dtw.wasm

wasm-dtw-site: wasm-dtw-compile
	node tools/wasm-dtw-p4b/build-site.mjs

wasm-dtw-site-smoke: wasm-dtw-site
	node tools/wasm-dtw-p4b/site-smoke.mjs site/dtw

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
standalone-eval:
	$(EMACS) --batch -Q -L lisp -L src -L scripts \
	  --eval '(setq load-prefer-newer t)' \
	  -l nelisp-standalone-build -f nelisp-standalone-build

standalone-eval-test:
	$(EMACS) --batch -Q -L lisp -L src -L scripts \
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
	$(EMACS) --batch -Q -L lisp -L src -L scripts \
	  --eval '(setq load-prefer-newer t)' \
	  -l nelisp-standalone-build -f nelisp-standalone-build-reader

# fboundp-liar audit: every name in the reader's builtin fboundp list must
# have a dispatch arm (or be combiner-handled), so `fboundp' never lies the
# way `nelisp--syscall-readdir' did (2026-06-10).
reader-surface-audit:
	$(EMACS) --batch -Q -L lisp -L src -L scripts \
	  --eval '(setq load-prefer-newer t)' \
	  -l reader-surface-audit -f nelisp-reader-surface-audit

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

# Fast focused loop for CLI load work.  Builds/relinks target/nelisp using the
# incremental unit cache, then checks only `--load' output instead of running
# the full reader CLI/runtime-image/REPL smoke.
standalone-reader-load-smoke: standalone-reader
	@mkdir -p target
	@printf '%s\n' '(+ 40 3)' > target/standalone-reader-load-smoke.el
	@out="$$(./target/nelisp --load target/standalone-reader-load-smoke.el)"; \
	if [ "$$out" = "43" ]; then \
	  echo "[standalone-reader-load-smoke] PASS: --load -> $$out"; \
	else \
	  echo "[standalone-reader-load-smoke] FAIL: --load -> $$out"; \
	  exit 1; \
	fi

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
standalone-reader-intern-soft-smoke: standalone-reader
	@mkdir -p target
	@printf '%s\n' \
	  '(load "lisp/nelisp-stdlib-misc.el")' \
	  '(list (intern-soft "nelisp-doc163-fresh-a") (progn (intern "nelisp-doc163-fresh-a") (intern-soft "nelisp-doc163-fresh-a")) (intern-soft "nelisp-doc163-fresh-b") (intern-soft "nelisp-doc163-fresh-b"))' \
	  > target/standalone-reader-intern-soft-smoke.el
	@out="$$(ulimit -v 4194304; timeout 30 ./target/nelisp --load target/standalone-reader-intern-soft-smoke.el)"; \
	if [ "$$out" = "(nil nelisp-doc163-fresh-a nil nil)" ]; then \
	  echo "[standalone-reader-intern-soft-smoke] PASS: -> $$out"; \
	else \
	  echo "[standalone-reader-intern-soft-smoke] FAIL: -> $$out (expected (nil nelisp-doc163-fresh-a nil nil))"; \
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
	@out="$$(ulimit -v 4194304; timeout 30 ./target/nelisp --load target/standalone-reader-intern-soft-loop-smoke.el)"; \
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
	@out="$$(./target/nelisp --load target/standalone-reader-fmt-smoke.el)"; \
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
	@out="$$(./target/nelisp --load target/standalone-reader-prelude-equal-reload-smoke.el)"; \
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
	@out="$$(./target/nelisp --repl --no-prompt < target/standalone-reader-declare-strip-smoke.el | tail -1)"; \
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
	@out="$$(./target/nelisp --repl --no-prompt < target/standalone-reader-nested-backquote-macro-smoke.el | tail -1)"; \
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
	@out="$$(./target/nelisp --load target/standalone-reader-derived-mode-shape-smoke.el)"; \
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
	@out="$$(./target/nelisp --load target/standalone-reader-pcase-quote-literal-smoke.el)"; \
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
	@out="$$(./target/nelisp --load target/standalone-reader-catch-throw-tag-smoke.el)"; \
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
	@out="$$(./target/nelisp --load target/standalone-reader-cond-let-shape-smoke.el)"; \
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
	  '(list (= (mod 5.5 2) 1.5) (= (mod -5.5 2) 0.5) (= (mod 5.5 -2) -0.5) (= (mod -5.5 -2) -1.5) (= (mod 5 2.0) 1.0) (= (mod -5 2.0) 1.0) (= (mod 5 -2.0) -1.0) (= (mod -5 -2.0) -1.0) (= (mod 5.5 2.5) 0.5) (= (mod -5.5 2.5) 2.0) (= (mod 5.5 -2.5) -2.0) (= (mod -5.5 -2.5) -0.5) (= (mod 7 -3) -2) (= (mod -7 3) 2) (= (mod 7 3) 1) (= (mod -7 -3) -1) (= (mod 10 3) 1) (floatp (mod 5.5 0.0)) (floatp (mod 5.0 0)) (eq (condition-case nil (progn (mod 5 0) (quote no-error)) (error (quote caught-error))) (quote caught-error)))' \
	  > target/standalone-reader-mod-float-smoke.el
	@out="$$(./target/nelisp --load target/standalone-reader-mod-float-smoke.el)"; \
	if [ "$$out" = "(t t t t t t t t t t t t t t t t t t t t)" ]; then \
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
	@out="$$(./target/nelisp --load target/standalone-reader-match-data-smoke.el)"; \
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
# reconstructs the same whole-second count `(floor (float-time))' gives,
# and that USEC/PSEC are in-range.
standalone-reader-current-time-smoke: standalone-reader
	@mkdir -p target
	@printf '%s\n' \
	  '(let* ((tm (current-time)) (hi (nth 0 tm)) (lo (nth 1 tm)) (us (nth 2 tm)) (ps (nth 3 tm))) (list (= (length tm) 4) (= (+ (* hi 65536) lo) (floor (float-time))) (and (>= us 0) (< us 1000000)) (= ps 0)))' \
	  > target/standalone-reader-current-time-smoke.el
	@out="$$(./target/nelisp --load target/standalone-reader-current-time-smoke.el)"; \
	if [ "$$out" = "(t t t t)" ]; then \
	  echo "[standalone-reader-current-time-smoke] PASS: -> $$out"; \
	else \
	  echo "[standalone-reader-current-time-smoke] FAIL: -> $$out"; \
	  exit 1; \
	fi

# Phase 47.D Step C / D1 / F1: runtime FFI smoke.  Builds a DYNAMICALLY linked
# reader (NELISP_READER_DYNAMIC=1) that imports libc + GnuTLS + FreeType and
# exposes them via the `nl-ffi-call' dispatcher, then asserts the calls route
# through the linker-generated PLT stubs + ld.so-resolved GOT slots into the real
# shared libraries.  Covers: libc int->int (toupper), libm double->double f64
# marshalling (sqrt/pow/ldexp(f64+i64)/hypot via XMM args + xmm0 return), GnuTLS
# const char* return (D1: gnutls_check_version), FreeType pointer-out-params (F1:
# FT_Init_FreeType + FT_Library_Version).  Self-contained (does NOT depend on the default static
# `standalone-reader').  GnuTLS/FreeType assertions are version-prefix checks so
# they survive minor library bumps.
standalone-reader-ffi-smoke:
	@mkdir -p target
	@NELISP_READER_DYNAMIC=1 $(EMACS) --batch -Q -L lisp -L src -L scripts \
	  --eval '(setq load-prefer-newer t)' \
	  -l nelisp-standalone-build -f nelisp-standalone-build-reader
	@chmod +x target/nelisp
	@printf '%s\n' '(nl-ffi-call "toupper" 97)' > target/standalone-reader-ffi-smoke.el
	@out="$$(./target/nelisp --load target/standalone-reader-ffi-smoke.el)"; \
	if [ "$$out" = "65" ]; then \
	  echo "[ffi-smoke libc] PASS: (nl-ffi-call \"toupper\" 97) -> $$out"; \
	else \
	  echo "[ffi-smoke libc] FAIL: -> $$out (expected 65)"; exit 1; \
	fi
	@: 'f64 FFI: double args/return through XMM.  The reader top-level printer'
	@: 'renders any float (even a literal) as #<object>, so assert on numeric'
	@: 'equality (= -> t) rather than the printed form.'
	@printf '%s\n' '(= (nl-ffi-call "sqrt" 4.0) 2.0)' > target/standalone-reader-ffi-f64.el
	@out="$$(./target/nelisp --load target/standalone-reader-ffi-f64.el)"; \
	if [ "$$out" = "t" ]; then \
	  echo "[ffi-smoke f64 libm] PASS: (= (nl-ffi-call \"sqrt\" 4.0) 2.0) -> $$out"; \
	else \
	  echo "[ffi-smoke f64 libm] FAIL: -> $$out (expected t)"; exit 1; \
	fi
	@printf '%s\n' '(list (= (nl-ffi-call "pow" 2.0 10.0) 1024.0) (= (nl-ffi-call "ldexp" 1.5 3) 12.0) (= (nl-ffi-call "hypot" 3.0 4.0) 5.0))' > target/standalone-reader-ffi-f64b.el
	@out="$$(./target/nelisp --load target/standalone-reader-ffi-f64b.el)"; \
	if [ "$$out" = "(t t t)" ]; then \
	  echo "[ffi-smoke f64 mixed] PASS: pow(2,10)=1024 / ldexp(1.5,3 :: f64+i64)=12 / hypot(3,4)=5 -> $$out"; \
	else \
	  echo "[ffi-smoke f64 mixed] FAIL: -> $$out (expected (t t t))"; exit 1; \
	fi
	@printf '%s\n' '(let ((p (nl-ffi-call "gnutls_check_version" 0))) (if (= p 0) "NULL" (unibyte-string (ptr-read-u8 p 0) (ptr-read-u8 p 1))))' > target/standalone-reader-ffi-d1.el
	@out="$$(./target/nelisp --load target/standalone-reader-ffi-d1.el)"; \
	case "$$out" in \
	  '"'[0-9].'"') echo "[ffi-smoke D1 gnutls] PASS: gnutls_check_version -> $$out (X.)";; \
	  *) echo "[ffi-smoke D1 gnutls] FAIL: -> $$out (expected \"<digit>.\")"; exit 1;; \
	esac
	@printf '%s\n' '(let* ((s (alloc-bytes 8 8)) (rc (nl-ffi-call "FT_Init_FreeType" s)) (lib (ptr-read-u64 s 0)) (mj (alloc-bytes 4 4)) (mn (alloc-bytes 4 4)) (pt (alloc-bytes 4 4))) (nl-ffi-call "FT_Library_Version" lib mj mn pt) (let ((r (list rc (ptr-read-u32 mj 0)))) (nl-ffi-call "FT_Done_FreeType" lib) r))' > target/standalone-reader-ffi-f1.el
	@out="$$(./target/nelisp --load target/standalone-reader-ffi-f1.el)"; \
	case "$$out" in \
	  '(0 '[0-9]*')') echo "[ffi-smoke F1 freetype] PASS: FT_Init+Version -> $$out (rc=0, major)";; \
	  *) echo "[ffi-smoke F1 freetype] FAIL: -> $$out (expected (0 <major>))"; exit 1;; \
	esac
	@font=/usr/share/fonts/truetype/dejavu/DejaVuSans.ttf; \
	if [ ! -f "$$font" ]; then \
	  echo "[ffi-smoke F2 glyph] SKIP: $$font not installed"; \
	else \
	  printf '%s\n' '(let* ((libp (alloc-bytes 8 8))) (nl-ffi-call "FT_Init_FreeType" libp) (let* ((lib (ptr-read-u64 libp 0)) (path "'"$$font"'") (pl (length path)) (pb (alloc-bytes 256 1)) (i 0)) (while (< i pl) (ptr-write-u8 pb i (aref path i)) (setq i (1+ i))) (ptr-write-u8 pb pl 0) (let* ((fp (alloc-bytes 8 8)) (nf (nl-ffi-call "FT_New_Face" lib pb 0 fp)) (face (ptr-read-u64 fp 0))) (nl-ffi-call "FT_Set_Pixel_Sizes" face 0 48) (let* ((gi (nl-ffi-call "FT_Get_Char_Index" face 65)) (ap (alloc-bytes 8 8)) (gr (nl-ffi-call "FT_Get_Advance" face gi 0 ap)) (adv (ptr-read-u64 ap 0))) (nl-ffi-call "FT_Done_Face" face) (nl-ffi-call "FT_Done_FreeType" lib) (list nf gi gr adv)))))' > target/standalone-reader-ffi-f2.el; \
	  out="$$(./target/nelisp --load target/standalone-reader-ffi-f2.el 2>/dev/null)"; \
	  case "$$out" in \
	    '(0 '[1-9]*' 0 '[1-9]*')') echo "[ffi-smoke F2 glyph] PASS: FT_New_Face+Get_Advance('A') -> $$out (newface rc, gindex, adv rc, 16.16 advance)";; \
	    *) echo "[ffi-smoke F2 glyph] FAIL: -> $$out (expected (0 <gindex> 0 <advance>))"; exit 1;; \
	  esac; \
	  printf '%s\n' '(let* ((libp (alloc-bytes 8 8))) (nl-ffi-call "FT_Init_FreeType" libp) (let* ((lib (ptr-read-u64 libp 0)) (path "'"$$font"'") (pl (length path)) (pb (alloc-bytes 256 1)) (i 0)) (while (< i pl) (ptr-write-u8 pb i (aref path i)) (setq i (1+ i))) (ptr-write-u8 pb pl 0) (let* ((fp (alloc-bytes 8 8)) (nf (nl-ffi-call "FT_New_Face" lib pb 0 fp)) (face (ptr-read-u64 fp 0))) (nl-ffi-call "FT_Set_Pixel_Sizes" face 0 48) (let* ((gi (nl-ffi-call "FT_Get_Char_Index" face 65)) (lg (nl-ffi-call "FT_Load_Glyph" face gi 0)) (slot (ptr-read-u64 face 152)) (rg (nl-ffi-call "FT_Render_Glyph" slot 0)) (rows (ptr-read-u32 slot 152)) (width (ptr-read-u32 slot 156))) (nl-ffi-call "FT_Done_Face" face) (nl-ffi-call "FT_Done_FreeType" lib) (list lg rg rows width)))))' > target/standalone-reader-ffi-f3.el; \
	  out="$$(./target/nelisp --load target/standalone-reader-ffi-f3.el 2>/dev/null)"; \
	  case "$$out" in \
	    '(0 0 '[1-9]*' '[1-9]*')') echo "[ffi-smoke F3 bitmap] PASS: FT_Load_Glyph+FT_Render_Glyph('A') -> $$out (load rc, render rc, bitmap rows, width)";; \
	    *) echo "[ffi-smoke F3 bitmap] FAIL: -> $$out (expected (0 0 <rows> <width>))"; exit 1;; \
	  esac; \
	  printf '%s\n' '(let* ((libp (alloc-bytes 8 8))) (nl-ffi-call "FT_Init_FreeType" libp) (let* ((lib (ptr-read-u64 libp 0)) (path "'"$$font"'") (pl (length path)) (pb (alloc-bytes 256 1)) (i 0)) (while (< i pl) (ptr-write-u8 pb i (aref path i)) (setq i (1+ i))) (ptr-write-u8 pb pl 0) (let* ((fp (alloc-bytes 8 8)) (nf (nl-ffi-call "FT_New_Face" lib pb 0 fp)) (face (ptr-read-u64 fp 0)) (mat (alloc-bytes 32 8)) (vec (alloc-bytes 16 8))) (nl-ffi-call "FT_Set_Pixel_Sizes" face 0 48) (ptr-write-u64 mat 0 131072) (ptr-write-u64 mat 8 0) (ptr-write-u64 mat 16 0) (ptr-write-u64 mat 24 131072) (ptr-write-u64 vec 0 0) (ptr-write-u64 vec 8 0) (nl-ffi-call "FT_Set_Transform" face mat vec) (let* ((gi (nl-ffi-call "FT_Get_Char_Index" face 65)) (lg (nl-ffi-call "FT_Load_Glyph" face gi 0)) (slot (ptr-read-u64 face 152)) (rg (nl-ffi-call "FT_Render_Glyph" slot 0)) (w (ptr-read-u32 slot 156))) (nl-ffi-call "FT_Done_Face" face) (nl-ffi-call "FT_Done_FreeType" lib) w))))' > target/standalone-reader-ffi-f4.el; \
	  out="$$(./target/nelisp --load target/standalone-reader-ffi-f4.el 2>/dev/null)"; \
	  if [ "$$out" -ge 55 ] 2>/dev/null; then \
	    echo "[ffi-smoke F4 transform] PASS: FT_Set_Transform(2x via FT_Matrix/FT_Vector) -> 2x-glyph width $$out px (vs ~33 untransformed)"; \
	  else \
	    echo "[ffi-smoke F4 transform] FAIL: -> $$out (expected 2x width >= 55)"; exit 1; \
	  fi; \
	fi
	@echo "[standalone-reader-ffi-smoke] PASS: libc + libm(f64) + GnuTLS(D1) + FreeType(F1/F2/F3/F4) via nl-ffi-call"

# Phase 47.D D2: REAL TLS 1.3 handshake from the pure-elisp reader.  Opens a raw
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
standalone-reader-tls-smoke:
	@mkdir -p target
	@if ! timeout 6 bash -c 'exec 3<>/dev/tcp/1.1.1.1/443' 2>/dev/null; then \
	  echo "[tls-smoke D2] SKIP: no egress to 1.1.1.1:443"; exit 0; \
	fi; \
	NELISP_READER_DYNAMIC=1 $(EMACS) --batch -Q -L lisp -L src -L scripts \
	  --eval '(setq load-prefer-newer t)' \
	  -l nelisp-standalone-build -f nelisp-standalone-build-reader; \
	chmod +x target/nelisp; \
	printf '%s\n' '(let* ((fd (syscall-direct 41 2 1 0 0 0 0)) (sa (alloc-bytes 16 8)) (i 0) (host "one.one.one.one") (hl (length host)) (hbuf (alloc-bytes 32 1)) (j 0) (req "GET / HTTP/1.1\r\nHost: one.one.one.one\r\nConnection: close\r\n\r\n") (rl (length req)) (rbuf (alloc-bytes 256 1)) (k 0)) (while (< j hl) (ptr-write-u8 hbuf j (aref host j)) (setq j (1+ j))) (while (< k rl) (ptr-write-u8 rbuf k (aref req k)) (setq k (1+ k))) (while (< i 16) (ptr-write-u8 sa i 0) (setq i (1+ i))) (ptr-write-u8 sa 0 2) (ptr-write-u8 sa 2 1) (ptr-write-u8 sa 3 187) (ptr-write-u8 sa 4 1) (ptr-write-u8 sa 5 1) (ptr-write-u8 sa 6 1) (ptr-write-u8 sa 7 1) (let ((crc (syscall-direct 42 fd sa 16 0 0 0)) (credp (alloc-bytes 8 8)) (sessp (alloc-bytes 8 8))) (nl-ffi-call "gnutls_global_init") (nl-ffi-call "gnutls_certificate_allocate_credentials" credp) (nl-ffi-call "gnutls_init" sessp 2) (let* ((cred (ptr-read-u64 credp 0)) (sess (ptr-read-u64 sessp 0))) (nl-ffi-call "gnutls_server_name_set" sess 1 hbuf hl) (nl-ffi-call "gnutls_set_default_priority" sess) (nl-ffi-call "gnutls_credentials_set" sess 1 cred) (nl-ffi-call "gnutls_transport_set_int2" sess fd fd) (let* ((hs (nl-ffi-call "gnutls_handshake" sess)) (ver (nl-ffi-call "gnutls_protocol_get_version" sess)) (np (nl-ffi-call "gnutls_protocol_get_name" ver)) (nm (if (= np 0) "?" (unibyte-string (ptr-read-u8 np 0) (ptr-read-u8 np 1) (ptr-read-u8 np 2) (ptr-read-u8 np 3)))) (sent (nl-ffi-call "gnutls_record_send" sess rbuf rl)) (resp (alloc-bytes 512 1)) (g1 (nl-ffi-call "gnutls_record_recv" sess resp 511)) (got (nl-ffi-call "gnutls_record_recv" sess resp 511)) (st (if (> got 0) (unibyte-string (ptr-read-u8 resp 0) (ptr-read-u8 resp 1) (ptr-read-u8 resp 2) (ptr-read-u8 resp 3)) "?"))) (nl-ffi-call "gnutls_bye" sess 0) (nl-ffi-call "gnutls_deinit" sess) (nl-ffi-call "gnutls_certificate_free_credentials" cred) (nl-ffi-call "gnutls_global_deinit") (syscall-direct 3 fd 0 0 0 0 0) (list hs nm sent st)))))' > target/standalone-reader-tls-smoke.el; \
	out="$$(timeout 30 ./target/nelisp --load target/standalone-reader-tls-smoke.el 2>/dev/null)"; \
	case "$$out" in \
	  '(0 "TLS'*'"HTTP")') echo "[tls-smoke D2+D3] PASS: real handshake + HTTPS GET -> $$out (handshake, proto, bytes-sent, response)";; \
	  *) echo "[tls-smoke D2+D3] FAIL: -> $$out (expected (0 \"TLS..\" <sent> \"HTTP\"))"; exit 1;; \
	esac

# Runtime smoke for the reader's process substrate (call-process /
# start-process / pipe read, scripts/nelisp-standalone-build.el).  The host ERT
# only checks the emitted C structure, NOT real fork/execve/wait4 behaviour, so
# this exercises the freestanding binary against actual subprocesses.
# POSIX-only (Windows builds emit -1 stubs, covered by the target ERT).
standalone-reader-process-smoke: standalone-reader
	@mkdir -p target
	@printf '%s\n' '(nelisp-process-call-process "/bin/sh" nil nil nil "-c" "exit 7")' > target/standalone-reader-process-smoke-cp.el
	@printf '%s\n' '(let* ((p (nelisp-process-start "/bin/sh" "-c" "printf process-smoke-ok"))) (nelisp-process-wait p) (nelisp-process-read-output p 64))' > target/standalone-reader-process-smoke-async.el
	@printf '%s\n' '(let* ((p (nelisp-process-start "/bin/cat")) (w (nelisp-process-write p "cat-roundtrip"))) (nelisp-process-close-stdin p) (nelisp-process-wait p) (let* ((out (nelisp-process-read-output p 64)) (ev (nelisp-process-poll p)) (ready (aref ev 0)) (exited (aref ev 1)) (code (aref ev 2))) (list w out ready exited code)))' > target/standalone-reader-process-smoke-cat.el
	@printf '%s\n' '(let* ((p (nelisp-process-start "/bin/sh" "-c" "sleep 1; printf sleepy")) (ev0 (nelisp-process-poll p)) (r0 (aref ev0 0)) (e0 (aref ev0 1))) (nelisp-process-wait p) (let* ((ev1 (nelisp-process-poll p)) (r1 (aref ev1 0)) (e1 (aref ev1 1)) (out (nelisp-process-read-output p 64))) (list r0 e0 r1 e1 out)))' > target/standalone-reader-process-smoke-poll.el
	@set +e; ./target/nelisp target/standalone-reader-process-smoke-cp.el; cp_rc=$$?; set -e; \
	out="$$(./target/nelisp --load target/standalone-reader-process-smoke-async.el)"; \
	cat_out="$$(./target/nelisp --load target/standalone-reader-process-smoke-cat.el)"; \
	poll_out="$$(./target/nelisp --load target/standalone-reader-process-smoke-poll.el)"; \
	if [ "$$cp_rc" = "7" ] && [ "$$out" = '"process-smoke-ok"' ] && [ "$$cat_out" = '(13 "cat-roundtrip" 1 1 0)' ] && [ "$$poll_out" = '(0 0 1 1 "sleepy")' ]; then \
	  echo "[standalone-reader-process-smoke] PASS: call-process exit=$$cp_rc, read-output -> $$out, cat -> $$cat_out, poll -> $$poll_out"; \
	else \
	  echo "[standalone-reader-process-smoke] FAIL: call-process exit=$$cp_rc, read-output -> $$out, cat -> $$cat_out, poll -> $$poll_out"; \
	  exit 1; \
	fi

# Fast focused loop for Doc 142 gate-6 REAL-RUNTIME in-process native exec.
# Builds/relinks target/nelisp, then runs the embedded `--neln-selftest'
# loader path against the REAL reader-linked `nelisp_aot_builtin_call1`.
standalone-reader-realrt-smoke: standalone-reader
	@mkdir -p target
	@stdout_file=target/standalone-reader-realrt-smoke.out; \
	rm -f "$$stdout_file"; \
	set +e; \
	./target/nelisp --neln-selftest >"$$stdout_file"; \
	rc=$$?; \
	set -e; \
	out="$$(cat "$$stdout_file")"; \
	if [ "$$rc" -eq 42 ] && [ -z "$$out" ]; then \
	  echo "[standalone-reader-realrt-smoke] PASS: exit=$$rc stdout=<empty>"; \
	else \
	  echo "[standalone-reader-realrt-smoke] FAIL: exit=$$rc stdout=$$out"; \
	  exit 1; \
	fi

# Fast focused loop for REPL work.  Builds/relinks target/nelisp with the
# incremental unit cache, then runs only the REPL smoke used by the full reader
# test.
standalone-reader-repl-smoke:
	$(EMACS) --batch -Q -L lisp -L src -L scripts \
	  --eval '(setq load-prefer-newer t)' \
	  -l nelisp-standalone-build -f nelisp-standalone-reader-repl-test

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
STANDALONE_VERSION ?= v0.6.0
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
