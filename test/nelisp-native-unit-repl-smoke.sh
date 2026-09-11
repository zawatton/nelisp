#!/bin/sh
# WS-F CI wiring: this smoke is registered as the `native-unit-repl-smoke'
# gate (Makefile target of the same name, tools/ai/gates.expected entry,
# Linux-only CI step run through `tools/ai/nelisp-ai.sh gate NAME -- CMD').
# It needs the opt-in Linux x86_64 `runtime-reload-reader' build
# (`make runtime-reload-reader'); every skip path below prints an explicit
# `GATE-SKIP <reason>' line rather than a silent or fake pass -- see
# tools/ai/README.md's report contract and AI.md rule 1 ("a gate that
# executed zero cases is not green").
#
# All numbered assertions live inside ONE persistent standalone REPL
# process (`tools/ai/nelisp-ai.sh repl'), started once below and fed the
# whole fixture in a single `load'.  Each one is tallied by the fixture's
# own `nelisp-smoke--check' counter and reported back as a real
# `GATE-COUNT checked=N findings=M' line -- never a hard-coded count.
#
# REPL errors do not necessarily produce a nonzero process exit (AI.md
# says this explicitly): besides the exit status, this script also checks
# a completion marker AND unexpected stderr before trusting a PASS.
set -eu

root=$(CDPATH= cd -- "$(dirname "$0")/.." && pwd)
bin=${NELISP_BIN:-$root/target/nelisp-runtime-reload}

CHECKED=0
REPORTED=0
dir=""
cleanup() {
    rc=$?
    if [ -n "$dir" ] && [ "${NELISP_SMOKE_KEEP:-0}" != 1 ]; then
        rm -rf "$dir"
    fi
    if [ "$REPORTED" -eq 0 ]; then
        if [ "$rc" -eq 0 ]; then
            printf 'GATE-COUNT checked=%s findings=0\n' "$CHECKED"
        else
            printf 'GATE-COUNT checked=%s findings=1\n' "$CHECKED"
        fi
    fi
}
trap cleanup EXIT

if [ ! -x "$bin" ]; then
    REPORTED=1
    echo "GATE-SKIP runtime binary is unavailable: $bin"
    exit 0
fi
if ! command -v timeout >/dev/null 2>&1; then
    REPORTED=1
    echo 'GATE-SKIP timeout command is unavailable'
    exit 0
fi
if [ "$(uname -s)" != Linux ] || [ "$(uname -m)" != x86_64 ]; then
    REPORTED=1
    echo 'GATE-SKIP native raw units require Linux x86_64'
    exit 0
fi

mkdir -p "$root/target/tmp"
dir=$(mktemp -d "$root/target/tmp/nelisp-native-unit-repl-smoke.XXXXXX")
echo "artifacts=$dir"
sha=$(sha256sum "$bin" | awk '{print $1}')
echo "binary-sha256=$sha"

# --- fixture sources -------------------------------------------------

cat >"$dir/source-a.el" <<'EOF'
(defun helper (x) (+ x 1))
(defun publicscore (x) (* (helper x) 2))
EOF
cat >"$dir/source-b.el" <<'EOF'
(defun privatehelper (x) (+ x 10))
(defun publicscore (x) (* (privatehelper x) 2))
EOF
cat >"$dir/source-c.el" <<'EOF'
(defun privatehelper (x) (+ x 20))
(defun publicscore (x) (* (privatehelper x) 3))
EOF
cat >"$dir/source-d.el" <<'EOF'
(defun privatehelper (x) (+ x 30))
(defun publicscore (x) (* (privatehelper x) 3))
EOF
cat >"$dir/source-arity.el" <<'EOF'
(defun privatehelper (x) (+ x 10))
(defun publicscore (x y) (* (+ (privatehelper x) y) 3))
EOF
cat >"$dir/source-multi-1.el" <<'EOF'
(defun expa (x) (+ x 1))
(defun expb (x) (+ x 2))
EOF
cat >"$dir/source-multi-2.el" <<'EOF'
(defun expa (x) (+ x 101))
(defun expb (x) (+ x 202))
EOF
cat >"$dir/source-six.el" <<'EOF'
(defun sumsix (a b c d e f) (+ a b c d e f))
EOF
cat >"$dir/source-seven.el" <<'EOF'
(defun sumseven (a b c d e f g) (+ a b c d e f g))
EOF
cat >"$dir/source-badbuild.el" <<'EOF'
(defun sumsix (a b c d e f) (+ a b c d e f))
(message "not a defun, this must fail the raw compiler")
EOF
# Ten successive generations of the same single-export unit (requirement
# 4: >= 3 successive publications; requirement 6: memory over repeated
# replacement).  Each body differs so the observed behaviour differs too.
i=1
while [ "$i" -le 10 ]; do
    printf '(defun loopfn (x) (+ x %d))\n' "$((i * 100))" >"$dir/source-loop-$i.el"
    i=$((i + 1))
done

# Compile immutable candidates used by the CAS check, the multi-export
# atomicity check and the consecutive-replacement loop. The callers (source
# A/B) and the six/seven-arg/bad-build sources are compiled by
# `nelisp-native-unit-rebuild-and-reload' itself, once its stable gate
# address is known, so they are NOT precompiled here.
precompile_names='("source-c" "source-d" "source-arity" "source-multi-1" "source-multi-2" "source-loop-1" "source-loop-2" "source-loop-3" "source-loop-4" "source-loop-5" "source-loop-6" "source-loop-7" "source-loop-8" "source-loop-9" "source-loop-10")'
NELISP_SHA="$sha" NELISP_SMOKE_DIR="$dir" \
  timeout "${NELISP_COMPILE_TIMEOUT:-90}s" emacs --batch -Q -L "$root/lisp" -L "$root/src" -L "$root/scripts" \
  --eval '(setq load-prefer-newer t)' \
  --eval "(progn
            (require (quote nelisp-native-load))
            (dolist (name (quote $precompile_names))
              (let ((source (expand-file-name (concat name \".el\") (getenv \"NELISP_SMOKE_DIR\")))
                    (artifact (expand-file-name (concat name \".nelr\") (getenv \"NELISP_SMOKE_DIR\"))))
                (nelisp-native-load-raw-compile-file source artifact nil \"replaceable-native-unit\" (getenv \"NELISP_SHA\")))))" \
  >/dev/null

export NELISP_SMOKE_ROOT="$root" NELISP_SMOKE_SHA="$sha"
cat >"$dir/fixture.el" <<'EOF'
(progn
  (require 'nelisp-native-unit-development)
  (unless (and (fboundp 'ptr-call) (fboundp 'syscall-direct)
               (nelisp-native-load--raw-supported-p))
    (nelisp--write-stdout-bytes "SKIP: native raw runtime is unavailable\n")
    (kill-emacs 0))

  (setq dir (getenv "NELISP_NATIVE_SMOKE_DIR"))
  (setq root (getenv "NELISP_SMOKE_ROOT"))

  ;; Every assertion below goes through this one counter. A failed check
  ;; signals immediately (never silently continues), and the running
  ;; total is re-printed on every success so a mid-run failure still
  ;; leaves the last-known-good count in this process's stdout for the
  ;; shell wrapper to report.
  (setq nelisp-smoke--checks 0)
  (defun nelisp-smoke--check (label ok)
    (if ok
        (progn
          (setq nelisp-smoke--checks (1+ nelisp-smoke--checks))
          (nelisp--write-stdout-bytes (format "SMOKE-CHECKS %d\n" nelisp-smoke--checks)))
      (error "smoke check failed: %s" label)))

  (defun nelisp-smoke--rss-bytes ()
    "Resident set size in bytes, read from /proc/self/statm.

SECONDARY signal only: native-unit reload maps memory through direct
`syscall-direct' mmap calls outside the tracked GC/allocator counters, so
this cannot by itself distinguish deliberate reservation from a leak.
The PRIMARY signal for requirement 6 is `nelisp-native-unit-resources',
whose :retained-bytes/:reclaimed-bytes are the runtime's own explicit
accounting of what a repeated replacement keeps mapped and why."
    (with-temp-buffer
      (insert-file-contents-literally "/proc/self/statm")
      (let* ((fields (split-string (buffer-string)))
             (resident-pages (string-to-number (nth 1 fields))))
        (* resident-pages 4096))))

  ;; ------------------------------------------------------------------
  ;; Requirement 2: a caller already running the old generation.
  ;; nativecaller is compiled ONCE against source A's stable gate address.
  ;; ------------------------------------------------------------------
  (setq retained-state (list "日本語" [17 29]))
  (setq source-a-result
        (nelisp-native-unit-rebuild-and-reload
         (expand-file-name "source-a.el" dir) nil '("publicscore") root))
  (nelisp-smoke--check "source-a-published"
                       (eq (plist-get source-a-result :status) 'published))
  (setq unit-id (plist-get source-a-result :unit-id))
  (setq stable-gate (nelisp-native-unit-address unit-id "publicscore"))
  (let ((caller-source (expand-file-name "nativecaller.el" dir)))
    (with-temp-file caller-source
      (insert (format "(defun nativecaller (x) (+ 100 (call-ptr %d x)))\n" stable-gate)))
    (setq caller-result
          (nelisp-native-unit-rebuild-and-reload caller-source nil '("nativecaller") root))
    (nelisp-smoke--check "caller-published"
                         (eq (plist-get caller-result :status) 'published))
    (setq caller-unit (plist-get caller-result :unit-id)))
  ;; This call runs entirely BEFORE source B is even built below: it
  ;; exercises the caller against the generation published at the time it
  ;; runs and must complete correctly under it.
  (setq old-generation-call-result (nelisp-native-unit-call caller-unit "nativecaller" '(5)))
  (nelisp-smoke--check "old-generation-call-completes-correctly"
                       (= old-generation-call-result 112))
  (nelisp--write-stdout-bytes
   (format "NATIVE_UNIT_OLD_GENERATION_RESULT %d\n" old-generation-call-result))

  (setq source-b-result
        (nelisp-native-unit-rebuild-and-reload
         (expand-file-name "source-b.el" dir) unit-id '("publicscore") root))
  (nelisp-smoke--check "source-b-published"
                       (eq (plist-get source-b-result :status) 'published))
  (nelisp-smoke--check "stable-gate-address-unchanged"
                       (= stable-gate (nelisp-native-unit-address unit-id "publicscore")))
  ;; Same stable entry, no recompilation of the caller: this call now
  ;; observes the new generation.
  (setq new-generation-call-result (nelisp-native-unit-call caller-unit "nativecaller" '(5)))
  (nelisp-smoke--check "new-generation-call-observes-new-code"
                       (= new-generation-call-result 130))
  (nelisp--write-stdout-bytes
   (format "NATIVE_UNIT_NEW_GENERATION_RESULT %d\n" new-generation-call-result))
  (garbage-collect)
  (nelisp-smoke--check "retained-lisp-state-survives-gc-and-reload"
                       (equal retained-state '("日本語" [17 29])))
  (nelisp--write-stdout-bytes "NATIVE_UNIT_OLD_THEN_NEW_112_130\n")

  ;; ------------------------------------------------------------------
  ;; Requirement 5a: a refused (stale) candidate leaves the unit answering
  ;; with the previously published generation and behaviour, and
  ;; `nelisp-native-unit-code-info' still names that older identity, not
  ;; the rejected candidate's.
  ;; ------------------------------------------------------------------
  (setq candidate-c
        (nelisp-native-unit-stage (expand-file-name "source-c.nelr" dir) unit-id '("publicscore")))
  (setq candidate-d
        (nelisp-native-unit-stage (expand-file-name "source-d.nelr" dir) unit-id '("publicscore")))
  (nelisp-smoke--check "candidate-c-staged" (eq (plist-get candidate-c :status) 'staged))
  (nelisp-smoke--check "candidate-d-staged" (eq (plist-get candidate-d :status) 'staged))
  (setq publish-c (nelisp-native-unit-publish (plist-get candidate-c :candidate-id)))
  (nelisp-smoke--check "publish-c-published" (eq (plist-get publish-c :status) 'published))
  (nelisp-smoke--check "publish-c-generation-3" (= (plist-get publish-c :generation) 3))
  (setq code-info-after-c (nelisp-native-unit-code-info unit-id "publicscore"))
  (setq publish-d (nelisp-native-unit-publish (plist-get candidate-d :candidate-id)))
  (nelisp-smoke--check "publish-d-rejected-stale-cas"
                       (and (eq (plist-get publish-d :status) 'rejected)
                            (string-match-p "CAS rejected" (plist-get publish-d :reason))))
  (setq code-info-after-d-refusal (nelisp-native-unit-code-info unit-id "publicscore"))
  (nelisp-smoke--check "refused-publish-leaves-code-info-identity-untouched"
                       (and (equal (plist-get code-info-after-c :source-sha256)
                                   (plist-get code-info-after-d-refusal :source-sha256))
                            (= (plist-get code-info-after-c :generation)
                               (plist-get code-info-after-d-refusal :generation))))
  (setq caller-after-cas (nelisp-native-unit-call caller-unit "nativecaller" '(5)))
  (nelisp-smoke--check "caller-still-observes-c-after-cas" (= caller-after-cas 175))
  (nelisp-smoke--check "generation-preserved-after-stale-rejection"
                       (= (plist-get (nelisp-native-unit-status unit-id) :generation) 3))
  (nelisp--write-stdout-bytes "NATIVE_UNIT_CAS_REJECTED_STATE_PRESERVED\n")

  (setq arity-result
        (nelisp-native-unit-rebuild-and-reload
         (expand-file-name "source-arity.el" dir) unit-id '("publicscore") root))
  (nelisp-smoke--check "arity-change-rejected-at-stage"
                       (and (eq (plist-get arity-result :status) 'rejected)
                            (eq (plist-get arity-result :phase) :stage)
                            (string-match-p "arities" (plist-get arity-result :reason))))
  (nelisp-smoke--check "caller-still-175-after-contract-rejection"
                       (= (nelisp-native-unit-call caller-unit "nativecaller" '(5)) 175))
  (nelisp--write-stdout-bytes "NATIVE_UNIT_ARITY_CONTRACT_REJECTED\n")

  ;; ------------------------------------------------------------------
  ;; Requirement 1: multiple exports switch atomically.
  ;; ------------------------------------------------------------------
  (setq multi-stage-1
        (nelisp-native-unit-stage (expand-file-name "source-multi-1.nelr" dir) nil '("expa" "expb")))
  (nelisp-smoke--check "multi-gen1-staged" (eq (plist-get multi-stage-1 :status) 'staged))
  (setq multi-publish-1 (nelisp-native-unit-publish (plist-get multi-stage-1 :candidate-id)))
  (nelisp-smoke--check "multi-gen1-published" (eq (plist-get multi-publish-1 :status) 'published))
  (setq multi-unit (plist-get multi-publish-1 :unit-id))
  (nelisp-smoke--check "multi-gen1-baseline-a" (= (nelisp-native-unit-call multi-unit "expa" '(0)) 1))
  (nelisp-smoke--check "multi-gen1-baseline-b" (= (nelisp-native-unit-call multi-unit "expb" '(0)) 2))
  (setq multi-stage-2
        (nelisp-native-unit-stage (expand-file-name "source-multi-2.nelr" dir) multi-unit '("expa" "expb")))
  (nelisp-smoke--check "multi-gen2-staged" (eq (plist-get multi-stage-2 :status) 'staged))
  (setq multi-publish-2 (nelisp-native-unit-publish (plist-get multi-stage-2 :candidate-id)))
  (nelisp-smoke--check "multi-gen2-published" (eq (plist-get multi-publish-2 :status) 'published))
  ;; Both exports are read independently, AFTER the single table-pointer
  ;; swap completed: neither may still show generation-1 behaviour.
  (setq multi-a2 (nelisp-native-unit-call multi-unit "expa" '(0)))
  (setq multi-b2 (nelisp-native-unit-call multi-unit "expb" '(0)))
  (setq multi-status-2 (nelisp-native-unit-status multi-unit))
  (nelisp-smoke--check "multi-gen2-a-atomic" (= multi-a2 101))
  (nelisp-smoke--check "multi-gen2-b-atomic" (= multi-b2 202))
  (nelisp-smoke--check "multi-gen2-status-generation-matches"
                       (= (plist-get multi-status-2 :generation) 2))
  (nelisp--write-stdout-bytes
   (format "NATIVE_UNIT_MULTI_EXPORT_ATOMIC a=%d b=%d generation=%d\n"
           multi-a2 multi-b2 (plist-get multi-status-2 :generation)))

  ;; ------------------------------------------------------------------
  ;; Coordinator addendum 1: a discarded/refused candidate is reclaimed,
  ;; via `nelisp-native-unit-resources'.
  ;; ------------------------------------------------------------------
  (setq resources-before-discard (nelisp-native-unit-resources))
  (setq discard-candidate
        (nelisp-native-unit-stage (expand-file-name "source-multi-1.nelr" dir) multi-unit '("expa" "expb")))
  (nelisp-smoke--check "discard-candidate-staged" (eq (plist-get discard-candidate :status) 'staged))
  (setq resources-after-stage (nelisp-native-unit-resources))
  (nelisp-smoke--check "candidates-count-increased-after-stage"
                       (= (plist-get resources-after-stage :candidates)
                          (1+ (plist-get resources-before-discard :candidates))))
  (nelisp-native-unit-discard (plist-get discard-candidate :candidate-id))
  (setq resources-after-discard (nelisp-native-unit-resources))
  (nelisp-smoke--check "candidates-count-back-down-after-discard"
                       (= (plist-get resources-after-discard :candidates)
                          (plist-get resources-before-discard :candidates)))
  (nelisp-smoke--check "reclaimed-tables-increased-after-discard"
                       (= (plist-get resources-after-discard :reclaimed-tables)
                          (1+ (plist-get resources-after-stage :reclaimed-tables))))
  (nelisp-smoke--check "reclaimed-bytes-increased-after-discard"
                       (>= (- (plist-get resources-after-discard :reclaimed-bytes)
                              (plist-get resources-after-stage :reclaimed-bytes))
                           4096))
  (nelisp--write-stdout-bytes "NATIVE_UNIT_DISCARD_RECLAIMED\n")

  ;; ------------------------------------------------------------------
  ;; Requirement 3: argument boundaries. Declared limit is six integer
  ;; arguments under the raw-v1 SysV ABI.
  ;; ------------------------------------------------------------------
  (setq six-result
        (nelisp-native-unit-rebuild-and-reload
         (expand-file-name "source-six.el" dir) nil '("sumsix") root))
  (nelisp-smoke--check "six-arg-export-published" (eq (plist-get six-result :status) 'published))
  (setq six-unit (plist-get six-result :unit-id))
  (setq six-call-result (nelisp-native-unit-call six-unit "sumsix" '(1 2 3 4 5 6)))
  (nelisp-smoke--check "six-arg-export-call-correct" (= six-call-result 21))
  (nelisp--write-stdout-bytes (format "NATIVE_UNIT_SIX_ARG_OK %d\n" six-call-result))

  (setq seven-result
        (nelisp-native-unit-rebuild-and-reload
         (expand-file-name "source-seven.el" dir) nil '("sumseven") root))
  (nelisp-smoke--check "seven-arg-export-refused-at-compile"
                       (and (eq (plist-get seven-result :status) 'rejected)
                            (eq (plist-get seven-result :phase) :compile)))
  (nelisp--write-stdout-bytes "NATIVE_UNIT_SEVEN_ARG_REFUSED\n")

  (setq arity-mismatch-error
        (condition-case err
            (progn (nelisp-native-unit-call six-unit "sumsix" '(1 2 3 4 5)) 'no-error)
          (error (error-message-string err))))
  (nelisp-smoke--check "call-time-arity-mismatch-refused"
                       (and (stringp arity-mismatch-error)
                            (string-match-p "arity" arity-mismatch-error)))
  (nelisp--write-stdout-bytes "NATIVE_UNIT_ARITY_MISMATCH_REFUSED\n")

  ;; ------------------------------------------------------------------
  ;; Requirement 5b: state is preserved after a failed BUILD (as opposed
  ;; to 5a's refused-but-compiled candidate). source-badbuild.el's second
  ;; top-level form is not a `defun' at all, so the raw compiler rejects
  ;; the whole source before ever reaching stage/publish.
  ;; ------------------------------------------------------------------
  (setq six-status-before-failed-build (nelisp-native-unit-status six-unit))
  (setq failed-build-result
        (nelisp-native-unit-rebuild-and-reload
         (expand-file-name "source-badbuild.el" dir) six-unit '("sumsix") root))
  (nelisp-smoke--check "failed-build-rejected-at-compile-phase"
                       (and (eq (plist-get failed-build-result :status) 'rejected)
                            (eq (plist-get failed-build-result :phase) :compile)))
  (setq six-status-after-failed-build (nelisp-native-unit-status six-unit))
  (nelisp-smoke--check "state-preserved-after-failed-build-generation"
                       (equal (plist-get six-status-before-failed-build :generation)
                              (plist-get six-status-after-failed-build :generation)))
  (setq six-call-after-failed-build (nelisp-native-unit-call six-unit "sumsix" '(1 2 3 4 5 6)))
  (nelisp-smoke--check "state-preserved-after-failed-build-call"
                       (= six-call-after-failed-build 21))
  (nelisp--write-stdout-bytes "NATIVE_UNIT_STATE_PRESERVED_AFTER_FAILED_BUILD\n")

  ;; ------------------------------------------------------------------
  ;; Requirements 4 and 6, plus coordinator addenda 2 and 3: at least
  ;; three (here, ten) successive publications on one unit, each
  ;; generation +1 and each behaviour different; the runtime's own
  ;; :retired/:retained-bytes accounting across them (primary signal for
  ;; requirement 6); RSS as a labelled secondary signal; code-info's
  ;; :source-current flip after an unpublished on-disk edit; and
  ;; `nelisp-native-unit-reclaim' honestly refusing to unmap generations
  ;; it cannot prove are unreachable.
  ;; ------------------------------------------------------------------
  (setq loop-rss-before (nelisp-smoke--rss-bytes))
  (setq loop-resources-before (nelisp-native-unit-resources))
  (setq loop-unit nil)
  (let ((i 1) (n 10))
    (while (<= i n)
      (let* ((artifact (expand-file-name (format "source-loop-%d.nelr" i) dir))
             (staged (nelisp-native-unit-stage artifact loop-unit '("loopfn"))))
        (nelisp-smoke--check (format "loop-generation-%d-staged" i)
                             (eq (plist-get staged :status) 'staged))
        (let ((published (nelisp-native-unit-publish (plist-get staged :candidate-id))))
          (nelisp-smoke--check (format "loop-generation-%d-published" i)
                               (eq (plist-get published :status) 'published))
          (nelisp-smoke--check (format "loop-generation-%d-increments-by-one" i)
                               (= (plist-get published :generation) i))
          (setq loop-unit (plist-get published :unit-id))
          (let ((value (nelisp-native-unit-call loop-unit "loopfn" '(0))))
            (nelisp-smoke--check (format "loop-generation-%d-behaviour-changed" i)
                                 (= value (* i 100))))))
      (setq i (1+ i))))
  (setq loop-rss-after (nelisp-smoke--rss-bytes))
  (setq loop-resources-after (nelisp-native-unit-resources))

  (nelisp-smoke--check "loop-candidates-count-unchanged"
                       (= (plist-get loop-resources-after :candidates)
                          (plist-get loop-resources-before :candidates)))
  (setq loop-retired-delta (- (plist-get loop-resources-after :retired)
                              (plist-get loop-resources-before :retired)))
  (nelisp-smoke--check "loop-retired-count-is-generations-minus-one"
                       (= loop-retired-delta 9))
  (setq loop-retained-delta (- (plist-get loop-resources-after :retained-bytes)
                               (plist-get loop-resources-before :retained-bytes)))
  ;; Every retired generation keeps its own generation-table page mapped
  ;; (nelisp-native-unit--table-bytes, 4096 today) plus its artifact's
  ;; mapped code -- so the per-retirement cost is a fixed, page-granular
  ;; figure, never below one table page. Bounded above at 16 table pages
  ;; (64 KiB) per retirement: generous headroom over the actually observed
  ;; figure (measured 8192 B/generation for these one-line bodies against
  ;; this build), while still catching a regression that leaked megabytes
  ;; per republish instead of one code page.
  (nelisp-smoke--check "loop-retained-bytes-is-exact-multiple-of-generations"
                       (= (mod loop-retained-delta 9) 0))
  (setq loop-retained-per-generation (/ loop-retained-delta 9))
  (nelisp-smoke--check "loop-retained-bytes-per-generation-bounded"
                       (and (>= loop-retained-per-generation 4096)
                            (<= loop-retained-per-generation (* 16 4096))))
  (nelisp--write-stdout-bytes
   (format "NATIVE_UNIT_RETAINED_BYTES retired=%d growth=%d per_generation=%d\n"
           loop-retired-delta loop-retained-delta loop-retained-per-generation))

  (setq loop-rss-growth (- loop-rss-after loop-rss-before))
  ;; SECONDARY signal (see `nelisp-smoke--rss-bytes'): 100 MiB over ten
  ;; replacements is roughly 1000x the table-page-level growth the PRIMARY
  ;; :retained-bytes figure above already measured exactly, so this bound
  ;; is here only to catch a gross, unrelated blow-up (e.g. a whole extra
  ;; heap image), not to police the expected page-granular growth --
  ;; that is what :retained-bytes above is for.
  (nelisp-smoke--check "loop-rss-growth-bounded-secondary-signal"
                       (< loop-rss-growth (* 100 1024 1024)))
  (nelisp--write-stdout-bytes
   (format "NATIVE_UNIT_RSS_SECONDARY before=%d after=%d growth=%d\n"
           loop-rss-before loop-rss-after loop-rss-growth))

  ;; code-info :source-current, using the loop's own last generation
  ;; (its recorded :source is this fixture's own file on disk, unlike the
  ;; rebuild-and-reload callers above, which record an internal snapshot
  ;; path instead).
  (setq loop-source-path (expand-file-name "source-loop-10.el" dir))
  (setq loop-code-info-1 (nelisp-native-unit-code-info loop-unit "loopfn"))
  (nelisp-smoke--check "code-info-names-published-source-and-is-current"
                       (and (equal (plist-get loop-code-info-1 :source) loop-source-path)
                            (eq (plist-get loop-code-info-1 :source-current) t)
                            (equal (plist-get loop-code-info-1 :export-arity) 1)
                            (integerp (plist-get loop-code-info-1 :export-address))))
  (with-temp-file loop-source-path
    (insert "(defun loopfn (x) (+ x 999))\n"))
  (setq loop-code-info-2 (nelisp-native-unit-code-info loop-unit "loopfn"))
  (nelisp-smoke--check "code-info-source-current-flips-after-unpublished-edit"
                       (null (plist-get loop-code-info-2 :source-current)))
  (nelisp-smoke--check "call-unaffected-by-unpublished-edit"
                       (= (nelisp-native-unit-call loop-unit "loopfn" '(0)) 1000))
  (nelisp--write-stdout-bytes "NATIVE_UNIT_CODE_INFO_SOURCE_CURRENT_FLIPS\n")

  (setq loop-reclaim-result (nelisp-native-unit-reclaim loop-unit))
  (nelisp-smoke--check "reclaim-releases-nothing-it-cannot-prove-unreachable"
                       (null (plist-get loop-reclaim-result :released)))
  (nelisp-smoke--check "reclaim-refuses-exactly-the-retired-generations"
                       (= (length (plist-get loop-reclaim-result :refused)) 9))
  (nelisp--write-stdout-bytes "NATIVE_UNIT_RECLAIM_HONEST_REFUSAL\n")

  (with-temp-file (expand-file-name "proof.el" dir)
    (prin1 (list :binary (getenv "NELISP_SMOKE_SHA") :first source-a-result
                 :second source-b-result :cas publish-c :stale publish-d
                 :arity arity-result :multi multi-status-2 :six six-result
                 :seven seven-result :loop-retired loop-retired-delta
                 :loop-retained loop-retained-delta :gate stable-gate
                 :retained retained-state :caller-generation
                 (nelisp-native-unit-status caller-unit))
           (current-buffer)))

  (nelisp--write-stdout-bytes "NATIVE_UNIT_REPL_DONE\n"))
EOF

export NELISP_NATIVE_SMOKE_DIR="$dir"
printf '%s\n' '(load (expand-file-name "fixture.el" (getenv "NELISP_NATIVE_SMOKE_DIR")))' '(exit)' >"$dir/input.el"
set +e
timeout "${NELISP_SMOKE_TIMEOUT:-180}s" env NELISP_BIN="$bin" \
  "$root/tools/ai/nelisp-ai.sh" repl --no-prompt <"$dir/input.el" \
  >"$dir/out" 2>"$dir/err"
status=$?
set -e

checked=$(grep -E '^SMOKE-CHECKS ' "$dir/out" | tail -1 | awk '{print $2}')
[ -n "${checked:-}" ] || checked=0
CHECKED=$checked

if grep -q '^SKIP:' "$dir/out" && [ ! -s "$dir/err" ]; then
    cat "$dir/out"
    REPORTED=1
    echo 'GATE-SKIP native raw runtime is unavailable inside the running binary'
    exit 0
fi
if [ "$status" -ne 0 ]; then
    cat "$dir/err" >&2
    exit "$status"
fi

# Two deliberate calls above (the seven-argument export and the bad
# build) each go through a real host `emacs --batch' compiler subprocess
# that is EXPECTED to fail, and this substrate's `call-process' does not
# fully separate that child's own diagnostic output from this process's
# inherited stderr. Filter exactly that known, benign shape -- a blank
# line, `debug-early-backtrace...done', and the compiler's own rejection
# message -- and treat anything else in stderr as unexpected.
if [ -s "$dir/err" ]; then
    unexpected_err=$(grep -v '^nelisp-native-load: unsupported raw top-level form:' "$dir/err" \
        | grep -v '^debug-early-backtrace\.\.\.done$' \
        | grep -v '^[[:space:]]*$' || true)
    if [ -n "$unexpected_err" ]; then
        echo 'nelisp-native-unit-repl-smoke: unexpected stderr:' >&2
        printf '%s\n' "$unexpected_err" >&2
        exit 1
    fi
fi

grep -Fxq 'NATIVE_UNIT_OLD_THEN_NEW_112_130' "$dir/out"
grep -Fxq 'NATIVE_UNIT_CAS_REJECTED_STATE_PRESERVED' "$dir/out"
grep -Fxq 'NATIVE_UNIT_ARITY_CONTRACT_REJECTED' "$dir/out"
grep -q '^NATIVE_UNIT_MULTI_EXPORT_ATOMIC ' "$dir/out"
grep -Fxq 'NATIVE_UNIT_DISCARD_RECLAIMED' "$dir/out"
grep -q '^NATIVE_UNIT_SIX_ARG_OK ' "$dir/out"
grep -Fxq 'NATIVE_UNIT_SEVEN_ARG_REFUSED' "$dir/out"
grep -Fxq 'NATIVE_UNIT_ARITY_MISMATCH_REFUSED' "$dir/out"
grep -Fxq 'NATIVE_UNIT_STATE_PRESERVED_AFTER_FAILED_BUILD' "$dir/out"
grep -q '^NATIVE_UNIT_RETAINED_BYTES ' "$dir/out"
grep -q '^NATIVE_UNIT_RSS_SECONDARY ' "$dir/out"
grep -Fxq 'NATIVE_UNIT_CODE_INFO_SOURCE_CURRENT_FLIPS' "$dir/out"
grep -Fxq 'NATIVE_UNIT_RECLAIM_HONEST_REFUSAL' "$dir/out"
grep -Fxq 'NATIVE_UNIT_REPL_DONE' "$dir/out"

if [ "$checked" -le 0 ] 2>/dev/null; then
    echo 'nelisp-native-unit-repl-smoke: zero checks executed' >&2
    exit 1
fi

echo 'nelisp-native-unit-repl-smoke: PASS'
