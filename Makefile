.PHONY: test compile clean all bench gc-bench actor-bench soak

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
