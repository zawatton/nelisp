#!/bin/sh
# S7.7 slice 4: corpus-wide, non-per-fixture crash gate.
#
# test/nelisp-eln-crash-general-smoke.sh (S7.7 slice 1) exercises
# `nelisp-eln-registration--containment-boundary' through mocked Lisp-level
# non-local exits on one self-emitted fixture; test/nelisp-eln-same-artifact-smoke.sh
# (S7.6) exercises one pinned cleanup-failure injection on one self-emitted
# fixture. Both are per-fixture. This gate instead attempts a genuine
# `load' -- through the same after-load wrapper as the latter script -- of
# every real .eln artifact this repository has on hand: the GNU-compiled
# increment/decrement/chain fixtures, the 19 faithful vendor artifacts
# surveyed in S6, and this process's own self-emitted fixture, plus a
# deliberately corrupted copy of each (a flipped byte in .text, a
# truncated file, and a wrong ABI hash in metadata).
#
# One process per (artifact, check) pair -- never more than one artifact's
# real content in a process -- so that no artifact's admission state (an
# owner successfully rooted, or fboundp already claiming a name) or a
# crash can leak into another artifact's or another check's result. A
# clean rollback after a genuine artifact is admitted, followed by a
# second attempt at a corrupted copy of the SAME artifact in the SAME
# process, would trivially "reject" the corrupted copy merely because its
# name is already bound -- masking whatever the corruption itself would
# have done. Isolating every check into its own process avoids that trap.
#
# What proves "no crash" is each child's own exit code, not what it
# printed: the driver (test/nelisp-eln-crash-corpus-driver.el) always
# exits via an explicit `kill-emacs' with 0 (pass) or 1 (a Lisp-level
# rejection or a detected inconsistency -- a fail, but not a crash); any
# other exit code came from a signal or an abort outside Lisp's control,
# and that artifact/check is reported as CRASH, never silently folded
# into a plain FAIL.
set -eu

script_dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
repo=${NELISP_ROOT:-$(CDPATH= cd -- "$script_dir/.." && pwd)}
binary=${NELISP_BIN:-$repo/target/nelisp}
emacs_bin=${EMACS_BIN:-emacs}
shared_lisp=${NELISP_SHARED_LISP:-$repo/lisp}
ffi_root=${NELISP_ELN_CRASH_CORPUS_FFI_ROOT:-$repo}
driver=$repo/test/nelisp-eln-crash-corpus-driver.el
batch_driver=$repo/test/nelisp-eln-crash-corpus-batch-driver.el
cache_root=${XDG_CACHE_HOME:-$HOME/.cache}
log_root=${NELISP_ELN_CRASH_CORPUS_LOG_ROOT:-$cache_root/tmp/crash-gate}
mkdir -p "$log_root"
run_dir=$(mktemp -d "$log_root/corpus-gate.XXXXXX")
keep_artifacts=${NELISP_ELN_KEEP_ARTIFACTS:-0}
timeout_secs=${NELISP_ELN_CRASH_CORPUS_TIMEOUT:-60}

# Bounded worker pool for step 7's (artifact, check) matrix: every check
# already runs in its own process (see the file-level commentary above --
# this is a correctness requirement, not just a performance choice), so
# running several of those processes at once is safe as long as no two
# concurrent checks ever share a mutable path. Every corrupted-copy path
# and every check's stdout/stderr/result path is already namespaced by
# "$label.$check_kind" (see make_corrupt_* and the $worker script below),
# which
# stays unique per (artifact, check) regardless of how many run at once;
# nothing here is shared across labels or check kinds.
ncpu=$(nproc 2>/dev/null || getconf _NPROCESSORS_ONLN 2>/dev/null || echo 4)
case $ncpu in ''|*[!0-9]*) ncpu=4 ;; esac
jobs_default=$((ncpu / 2))
[ "$jobs_default" -ge 1 ] || jobs_default=1
[ "$jobs_default" -le 16 ] || jobs_default=16
case ${NELISP_ELN_GATE_JOBS:-} in
  ''|*[!0-9]*) jobs=$jobs_default ;;
  *) jobs=$NELISP_ELN_GATE_JOBS ;;
esac
[ "$jobs" -ge 1 ] || jobs=1
echo "worker pool: $jobs concurrent check(s) (NELISP_ELN_GATE_JOBS=${NELISP_ELN_GATE_JOBS:-unset}, nproc=$ncpu)" >&2

# S7.7.4: how many "safe-batch" processes step 7's `normal'/`corrupt-truncate'
# checks (proven never to reach `nl-ffi--dlopen' -- see
# test/nelisp-eln-crash-corpus-batch-driver.el's file-level commentary for
# the evidence) are folded into. Each is one process handling several
# checks sequentially, instead of one process per check, so this trades
# batch-internal parallelism for fewer process-start/module-load overheads.
# The still-isolated corrupt-text/corrupt-abi checks (one process each)
# always dominate the pool's item count, so fewer/larger batches -- which
# cuts the pool's TOTAL item count the most -- measured faster on this box
# than more/smaller ones every time tried (a 4-vs-6-vs-8-vs-12 sweep, S7.7.4
# investigation notes): quartering the pool width leaves enough batches to
# still overlap several with the isolated checks without spending an extra
# pool slot (and an extra process-start) on splitting the batch set finer
# than that pays for.
safe_chunks_default=$((jobs / 4))
[ "$safe_chunks_default" -ge 1 ] || safe_chunks_default=1
case ${NELISP_ELN_GATE_SAFE_CHUNKS:-} in
  ''|*[!0-9]*) safe_chunks=$safe_chunks_default ;;
  *) safe_chunks=$NELISP_ELN_GATE_SAFE_CHUNKS ;;
esac
[ "$safe_chunks" -ge 1 ] || safe_chunks=1

# Cross-run duration measurements for step 7b's longest-processing-time-
# first dispatch order. Lives under $log_root, NOT $run_dir, so it
# survives past this run's cleanup and each later run schedules from the
# previous run's real numbers -- there is nothing to measure a check's
# own duration by before it has ever run once.
duration_cache=${NELISP_ELN_CRASH_CORPUS_DURATIONS:-$log_root/durations.tsv}
[ -f "$duration_cache" ] || : >"$duration_cache"

cleanup() {
  status=$?
  if [ "$status" -eq 0 ] && [ "$keep_artifacts" != 1 ]; then
    rm -rf "$run_dir"
  else
    echo "LOG_DIR=$run_dir" >&2
  fi
}
trap cleanup EXIT HUP INT TERM

if [ ! -x "$binary" ]; then
  echo "NELISP_BIN is not executable: $binary" >&2
  exit 2
fi
. "$repo/test/lib/nelisp-boot-args.sh"
nl_cold_image_setup "$binary" || exit 1
if [ ! -r "$driver" ]; then
  echo "missing driver: $driver" >&2
  exit 2
fi
if [ ! -r "$batch_driver" ]; then
  echo "missing batch driver: $batch_driver" >&2
  exit 2
fi
if ! command -v "$emacs_bin" >/dev/null 2>&1; then
  echo "EMACS_BIN is not executable: $emacs_bin" >&2
  exit 2
fi
for tool in objcopy python3 truncate timeout sha256sum xargs; do
  if ! command -v "$tool" >/dev/null 2>&1; then
    echo "required tool not found: $tool" >&2
    exit 2
  fi
done

cd "$repo"

results=$run_dir/results.tsv
: >"$results"

# Every output row -- whether decided immediately (SKIP, a corruption
# helper's own make-error, source-mutated) or produced by the $worker script
# dispatched later into the worker pool -- is recorded as one line in its
# own "$run_dir/<label>.<check_kind>.result" file. `order' lists those
# paths in the exact sequence step 9's report must see, decided entirely
# during step 7's sequential per-label pass (the same order the old
# strictly-sequential version of this script produced its rows in);
# `queue' lists the $worker invocations step 7 deferred into the pool.
# Concatenating `order' after the pool drains reproduces that sequence
# regardless of which check happened to finish first.
order=$run_dir/order.tsv
queue=$run_dir/queue.tsv
: >"$order"
: >"$queue"

# S7.7.4: `normal' and `corrupt-truncate' checks -- proven never to reach
# `nl-ffi--dlopen' (see test/nelisp-eln-crash-corpus-batch-driver.el) --
# are collected here instead of $queue, then folded into $safe_chunks
# batch-worker processes after step 7's per-artifact loop. Each line is
# the exact same "label\tcheck_kind\tartifact\texpect" shape $queue rows
# have always had; only the dispatch granularity differs.
safe_list=$run_dir/safe.tsv
: >"$safe_list"

# Write ROW (already-decided: SKIP / make-error / source-mutated) as the
# next line of $results, in $order's sequence.
emit_row() {
  label=$1
  check_kind=$2
  status=$3
  rc=$4
  resultfile=$run_dir/$label.$check_kind.result
  printf '%s\t%s\t%s\t%s\n' "$label" "$check_kind" "$status" "$rc" >"$resultfile"
  printf '%s\n' "$resultfile" >>"$order"
}

# Reserve label/check_kind's position in $order now, and queue the actual
# check for the worker pool to execute later. artifact/expect are exactly
# the $worker script's own positional arguments (besides label/check_kind).
enqueue_check() {
  label=$1
  check_kind=$2
  artifact=$3
  expect=$4
  resultfile=$run_dir/$label.$check_kind.result
  printf '%s\n' "$resultfile" >>"$order"
  printf '%s\t%s\t%s\t%s\n' "$label" "$check_kind" "$artifact" "$expect" >>"$queue"
}

# Like enqueue_check, but for the batchable (`normal'/`corrupt-truncate')
# checks: label/check_kind's position in $order is reserved exactly the
# same way (so step 9's report and total/fail counts cannot tell a
# batched result apart from an isolated one), but the actual check goes
# to $safe_list, not $queue -- a later batch-worker process, not a
# dedicated one-check process, will eventually write this resultfile.
queue_safe_check() {
  label=$1
  check_kind=$2
  artifact=$3
  expect=$4
  resultfile=$run_dir/$label.$check_kind.result
  printf '%s\n' "$resultfile" >>"$order"
  printf '%s\t%s\t%s\t%s\n' "$label" "$check_kind" "$artifact" "$expect" >>"$safe_list"
}

# --- 1. Load wrapper: same generation as test/nelisp-eln-same-artifact-smoke.sh ---
# Evaluates nelisp-standalone--core-bytecode-src and
# nelisp-standalone--after-load-runtime-src from scripts/nelisp-standalone-build.el
# with -L scripts -L lisp, so that plain `(load "foo.eln")' transparently
# routes through `nelisp-eln-registration-load' the same way a genuine
# consumer's `require'/`load' chain would encounter it.
provided_load_wrapper=${NELISP_ELN_LOAD_WRAPPER:-}
load_wrapper=${provided_load_wrapper:-$run_dir/generated-wrapper.el}
if [ -z "$provided_load_wrapper" ]; then
  if ! "$emacs_bin" --batch -Q -L scripts -L lisp --eval \
      '(progn (defvar nelisp-standalone--repo-root (file-name-as-directory default-directory)) (dolist (name (list "nelisp-standalone--core-bytecode-src" "nelisp-standalone--after-load-runtime-src")) (with-temp-buffer (insert-file-contents "scripts/nelisp-standalone-build.el") (goto-char (point-min)) (unless (search-forward (concat "(defun " name) nil t) (error "source generator not found: %s" name)) (goto-char (match-beginning 0)) (eval (read (current-buffer))))) (princ (nelisp-standalone--after-load-runtime-src)))' \
      >"$load_wrapper" 2>"$run_dir/wrapper.stderr"; then
    cat "$run_dir/wrapper.stderr" >&2
    exit 1
  fi
fi
if [ ! -r "$load_wrapper" ]; then
  echo "ELN load wrapper is not readable: $load_wrapper" >&2
  exit 2
fi

# NOTE on an approach tried and discarded here: baking the wrapper plus
# nelisp-eln-registration(-objects) into one `dump-runtime-image' /
# `eval-runtime-image' replay (so the worker pool below would replay a
# recorded recipe instead of each of the 100 workers re-parsing source)
# measured faster in isolation (~6.3s/replay vs ~7.5-10s/worker,
# low-load, single- and 8-way-concurrent) but, driven through this
# script at the real pool width (16-way, 101 checks) on this shared box,
# came out SLOWER on both metrics against a same-repo, same-box, back-
# to-back pristine-script control: 1140 CPU-s / 186s wall vs the
# control's 872 CPU-s / 75s wall (total=101 fail=0 both times -- the
# regression is pure cost, not correctness). `eval-runtime-image'
# apparently does not amortize the way a baked image's near-instant
# `dump' return time suggests; whatever it does at replay time scales
# worse under contention than plain source `load', not better. Left out
# of this file; see the handoff note for the measurements and the
# control run's log paths.
#
# --- 2. Discover the pinned ABI hash from the binary under test ---
# Read from the running binary rather than hardcoded, so a future ABI bump
# does not silently corrupt the wrong bytes in step 5 below.
abi_hash=$("$binary" ${NL_COLD_IMAGE_PATH:+--cold-load-from "$NL_COLD_IMAGE_PATH"} -L "$shared_lisp" --eval \
    '(progn (require (quote nelisp-eln-abi)) (princ (format "ABI_HASH=%s\n" (plist-get nelisp-eln-abi-gnu-31-1-x86_64 :producer-abi-hash))))' \
    2>"$run_dir/abi.stderr" | sed -n '1p' | sed 's/^ABI_HASH=//')
if [ -z "$abi_hash" ]; then
  cat "$run_dir/abi.stderr" >&2
  echo "could not determine the pinned ABI hash from $binary" >&2
  exit 1
fi
abi_replacement=$(printf '%s' "$abi_hash" | sed 's/./0/g')

# --- 3. Emit this process's own self-emitted fixture ---
# Same recipe as test/nelisp-eln-crash-general-smoke.sh's fixture.
self_fixture=$run_dir/self-emitted-fixture.eln
export NELISP_ELN_CRASH_CORPUS_SELF_FIXTURE=$self_fixture
if ! "$binary" ${NL_COLD_IMAGE_PATH:+--cold-load-from "$NL_COLD_IMAGE_PATH"} -L "$shared_lisp" --eval \
    '(progn (require (quote nelisp-eln-emitter)) (let ((ir (nelisp-aot-compiler--parse-stmt (quote (defun nelisp-eln-crash-corpus-self-emitted-fixture () 71)) nil nil nil))) (nelisp-eln-emitter-write-ir ir (getenv "NELISP_ELN_CRASH_CORPUS_SELF_FIXTURE"))) (princ "NELISP-ELN-CRASH-CORPUS-SELF-EMIT-PASS\n"))' \
    >"$run_dir/self-emit.stdout" 2>"$run_dir/self-emit.stderr"; then
  cat "$run_dir/self-emit.stdout"
  cat "$run_dir/self-emit.stderr" >&2
  exit 1
fi
if [ -s "$run_dir/self-emit.stderr" ] || \
   ! grep -Fx 'NELISP-ELN-CRASH-CORPUS-SELF-EMIT-PASS' "$run_dir/self-emit.stdout" >/dev/null; then
  cat "$run_dir/self-emit.stdout"
  cat "$run_dir/self-emit.stderr" >&2
  echo "NeLisp did not emit the corpus self-fixture cleanly" >&2
  exit 1
fi

# --- 4. Build the corpus: one "label<TAB>path" row per artifact ---
# Every default is a cache-relative discovery, never a bare hardcoded
# machine path, and every one is overridable. A source that is not found
# is SKIPped, not fatal, so this gate stays useful while a sibling lane's
# artifact has not been produced yet.
find_one_eln() {
  dir=$1
  [ -d "$dir" ] || return 1
  find "$dir" -maxdepth 1 -name '*.eln' -print 2>/dev/null | sort | head -n 1
}

increment_dir=${NELISP_ELN_CRASH_CORPUS_INCREMENT_DIR:-$cache_root/tmp/eln-gnu-arithmetic-probe/overlay/eln/31.1-$abi_hash}
decrement_dir=${NELISP_ELN_CRASH_CORPUS_DECREMENT_DIR:-$cache_root/tmp/eln-gnu-decrement-sonnet/overlay/eln/31.1-$abi_hash}
chain_dir=${NELISP_ELN_CRASH_CORPUS_CHAIN_DIR:-$cache_root/tmp/s46-chain-artifact/overlay/eln/31.1-$abi_hash}
vendor_root=${NELISP_ELN_CRASH_CORPUS_VENDOR_ROOT:-$cache_root/tmp/s6-survey-lex}

increment_src=${NELISP_ELN_CRASH_CORPUS_INCREMENT:-$(find_one_eln "$increment_dir" || true)}
decrement_src=${NELISP_ELN_CRASH_CORPUS_DECREMENT:-$(find_one_eln "$decrement_dir" || true)}
chain_src=${NELISP_ELN_CRASH_CORPUS_CHAIN:-$(find_one_eln "$chain_dir" || true)}

corpus_list=$run_dir/corpus.tsv
: >"$corpus_list"
printf 'gnu-increment\t%s\n' "${increment_src:-}" >>"$corpus_list"
printf 'gnu-decrement\t%s\n' "${decrement_src:-}" >>"$corpus_list"
printf 'gnu-chain\t%s\n' "${chain_src:-}" >>"$corpus_list"
printf 'self-emitted-fixture\t%s\n' "$self_fixture" >>"$corpus_list"
if [ -d "$vendor_root" ]; then
  find "$vendor_root" -path "*/overlay/eln/31.1-$abi_hash/*.eln" \
      ! -name '*DYNAMIC-BINDING-DO-NOT-USE*' -print 2>/dev/null | sort >"$run_dir/vendor-paths.txt" || true
  while IFS= read -r vendor_path; do
    [ -n "$vendor_path" ] || continue
    vendor_name=$(basename "$vendor_path" .eln)
    printf 'vendor-%s\t%s\n' "$vendor_name" "$vendor_path" >>"$corpus_list"
  done <"$run_dir/vendor-paths.txt"
fi

# S1.3/S5.8's gnu-identity.eln (and any sibling artifact this same
# investigation directory produced, such as scalar-boundary.eln) never
# lived under any of the three named dirs above or under vendor_root's
# `overlay/eln/31.1-<hash>/' layout, so 93/93 on an earlier run missed it
# entirely and a regression in this file went undetected until a
# same-artifact ledger check caught it separately.  Named explicitly here,
# plus a generic catch-all below, so a future genuine artifact that lands
# in yet another `eln-gnu-*' directory cannot go unnoticed the same way.
identity_dir=${NELISP_ELN_CRASH_CORPUS_IDENTITY_DIR:-$cache_root/tmp/eln-gnu-identity-investigation}
if [ -d "$identity_dir" ]; then
  find "$identity_dir" -maxdepth 1 -name '*.eln' -print 2>/dev/null | sort \
      >"$run_dir/identity-paths.txt" || true
  while IFS= read -r identity_path; do
    [ -n "$identity_path" ] || continue
    identity_name=$(basename "$identity_path" .eln)
    printf 'identity-%s\t%s\n' "$identity_name" "$identity_path" >>"$corpus_list"
  done <"$run_dir/identity-paths.txt"
fi

# Generic catch-all: any genuine `.eln' under any other `eln-gnu-*'
# top-level cache directory this run has not already named above.
# increment_dir/decrement_dir are deep subdirectories of their own
# top-level `eln-gnu-*' dir (.../overlay/eln/31.1-<hash>), not that dir
# itself, so exclusion compares TOP-LEVEL ancestors, never the deep
# path -- comparing the deep paths directly (an earlier version of this
# block did) silently never matched anything and duplicated both into
# the corpus under `catchall-' labels too.  This is deliberately broad --
# a missed artifact here is exactly the class of gap that let
# gnu-identity.eln slip through.
top_level_under_cache_tmp() {
  # Print the single path component directly under "$cache_root/tmp" that
  # DIR (any absolute path at or below it) descends from, or nothing if
  # DIR is not under "$cache_root/tmp" at all.
  dir=$1
  case $dir in
    "$cache_root/tmp"/*)
      rest=${dir#"$cache_root/tmp/"}
      printf '%s\n' "${rest%%/*}"
      ;;
  esac
}
: >"$run_dir/named-dirs.txt"
for named in "$increment_dir" "$decrement_dir" "$identity_dir"; do
  [ -n "$named" ] || continue
  resolved_named=$(CDPATH= cd -- "$named" 2>/dev/null && pwd) || continue
  top_level_under_cache_tmp "$resolved_named" >>"$run_dir/named-dirs.txt"
done
if [ -d "$cache_root/tmp" ]; then
  find "$cache_root/tmp" -maxdepth 1 -type d -name 'eln-gnu-*' -print 2>/dev/null \
      | sort >"$run_dir/catchall-dirs.txt" || true
  while IFS= read -r catchall_dir; do
    [ -n "$catchall_dir" ] || continue
    grep -Fxq "$(basename "$catchall_dir")" "$run_dir/named-dirs.txt" && continue
    find "$catchall_dir" -name '*.eln' -print 2>/dev/null | sort \
        >"$run_dir/catchall-paths.txt" || true
    while IFS= read -r catchall_path; do
      [ -n "$catchall_path" ] || continue
      catchall_name=$(basename "$catchall_dir")-$(basename "$catchall_path" .eln)
      printf 'catchall-%s\t%s\n' "$catchall_name" "$catchall_path" >>"$corpus_list"
    done <"$run_dir/catchall-paths.txt"
  done <"$run_dir/catchall-dirs.txt"
fi

vendor_count=$(grep -c '^vendor-' "$corpus_list" || true)
artifact_count=$(wc -l <"$corpus_list")
echo "corpus: $artifact_count artifacts (expected 19 vendor artifacts, found ${vendor_count:-0})" >&2

# --- 5. Corruption helpers: same-length in-place edits, ELF layout untouched ---

make_corrupt_text() {
  # A single flipped byte in the middle of .text, via objcopy dump/update
  # so no file-offset arithmetic on the ELF is needed by this script.
  src=$1
  dst=$2
  cp "$src" "$dst" || return 1
  chmod u+w "$dst" || return 1
  textbin=$dst.text.bin
  objcopy --dump-section .text="$textbin" "$dst" || return 1
  python3 - "$textbin" <<'PY' || return 1
import sys
path = sys.argv[1]
with open(path, "r+b") as f:
    data = bytearray(f.read())
    if not data:
        sys.exit("flip: empty .text extract: %s" % path)
    mid = len(data) // 2
    data[mid] ^= 0xFF
    f.seek(0)
    f.write(data)
PY
  objcopy --update-section .text="$textbin" "$dst" || return 1
  rm -f "$textbin"
}

make_corrupt_truncate() {
  # Cut the file to 60% of its original size (floor 16 bytes).
  src=$1
  dst=$2
  cp "$src" "$dst" || return 1
  chmod u+w "$dst" || return 1
  size=$(wc -c <"$src") || return 1
  new_size=$((size * 6 / 10))
  [ "$new_size" -ge 16 ] || new_size=16
  truncate -s "$new_size" "$dst" || return 1
}

make_corrupt_abi() {
  # Overwrite the metadata blob's ASCII ABI-hash text with a same-length,
  # definitely-wrong string, derived from the binary's own pinned hash
  # (step 2), never hardcoded.
  src=$1
  dst=$2
  cp "$src" "$dst" || return 1
  chmod u+w "$dst" || return 1
  python3 - "$dst" "$abi_hash" "$abi_replacement" <<'PY' || return 1
import sys
path, needle, replacement = sys.argv[1], sys.argv[2].encode(), sys.argv[3].encode()
if len(needle) != len(replacement):
    sys.exit("patch: needle/replacement length mismatch")
with open(path, "r+b") as f:
    data = f.read()
    count = data.count(needle)
    if count != 1:
        sys.exit("patch: expected exactly one ABI hash occurrence in %s, found %d"
                  % (path, count))
    idx = data.find(needle)
    f.seek(idx)
    f.write(replacement)
PY
}

# --- 6. run-one-check.sh: one process, one (artifact, check) pair ---

# Generated once; invoked by step 7b's `xargs -P' pool, one fresh `sh'
# process per (label, check_kind) pair. POSIX sh functions cannot be
# handed to a child the way bash's `export -f' hands bash functions to
# one, so the check's logic lives in this file instead of a shell
# function this pool could not dispatch to. Context ($binary, $repo, the
# generated wrapper's path, etc.) travels via the environment exported
# right before the pool starts (see step 7b) -- $RUN_DIR et al below are
# those same exports, read back by each worker process.
#
# Every path this writes -- out/err/resultfile/durationfile -- is
# namespaced by "$label.$check_kind", so distinct (label, check_kind)
# pairs never share a path even when their processes overlap in time.
worker=$run_dir/run-one-check.sh
cat >"$worker" <<'WORKER_EOF'
#!/bin/sh
set -eu
label=$1
check_kind=$2
artifact=$3
expect=$4
out=$RUN_DIR/$label.$check_kind.stdout
err=$RUN_DIR/$label.$check_kind.stderr
resultfile=$RUN_DIR/$label.$check_kind.result
durationfile=$RUN_DIR/$label.$check_kind.duration
rc=0
start=$(date +%s.%N)
NELISP_ELN_CRASH_CORPUS_JOB=load \
NELISP_ELN_CRASH_CORPUS_ARTIFACT=$artifact \
NELISP_ELN_CRASH_CORPUS_LABEL=$label.$check_kind \
NELISP_ELN_CRASH_CORPUS_EXPECT=$expect \
  timeout "$TIMEOUT_SECS" "$BINARY" ${NL_COLD_IMAGE_PATH:+--cold-load-from "$NL_COLD_IMAGE_PATH"} \
    -L "$REPO/lisp" -L "$SHARED_LISP" -L "$FFI_ROOT/packages/nl-ffi/src" \
    --load "$LOAD_WRAPPER" --load "$DRIVER" \
    >"$out" 2>"$err" || rc=$?
finish=$(date +%s.%N)
case $rc in
  0) status=PASS ;;
  1) status=FAIL ;;
  124) status=TIMEOUT ;;
  *) status=CRASH ;;
esac
printf '%s\t%s\t%s\t%s\n' "$label" "$check_kind" "$status" "$rc" >"$resultfile"
LC_ALL=C awk -v a="$start" -v b="$finish" 'BEGIN { printf "%.3f\n", (b - a) }' >"$durationfile"
if [ "$status" != PASS ]; then
  echo "NOT-PASS $label/$check_kind rc=$rc status=$status" >&2
fi
WORKER_EOF
chmod +x "$worker"

# batch_worker: one process for a whole $safe_list chunk (several
# `normal'/`corrupt-truncate' checks -- see test/nelisp-eln-crash-corpus-batch-driver.el
# for why only those two kinds are safe to share a process). Unlike
# $worker, the per-check result files are written by the Elisp driver
# itself, one by one, as each check is decided -- not by this shell
# wrapper from a single overall exit code, since one exit code cannot
# speak for several independent checks. This wrapper's own job is the
# backstop: after the driver process returns (cleanly, by timeout, or by
# crashing outright), any of THIS chunk's checks that still has no
# resultfile never got decided, and is filled in here, correctly
# labelled, with this process's own observed status -- so a crash deep in
# a batch is reported exactly like a crash in an isolated check would be
# (see $worker above), just attributed to every not-yet-decided check in
# the same chunk rather than to one.
batch_worker=$run_dir/run-safe-batch.sh
cat >"$batch_worker" <<'BATCH_WORKER_EOF'
#!/bin/sh
set -eu
label=$1
chunkfile=$2
out=$RUN_DIR/$label.stdout
err=$RUN_DIR/$label.stderr
durationfile=$RUN_DIR/$label.safe-batch.duration
rc=0
lines=$(wc -l <"$chunkfile" 2>/dev/null || echo 1)
case $lines in ''|*[!0-9]*) lines=1 ;; esac
[ "$lines" -ge 1 ] || lines=1
budget=$((TIMEOUT_SECS * lines))
[ "$budget" -ge "$TIMEOUT_SECS" ] || budget=$TIMEOUT_SECS
start=$(date +%s.%N)
NELISP_ELN_CRASH_CORPUS_BATCH_FILE=$chunkfile \
  timeout "$budget" "$BINARY" ${NL_COLD_IMAGE_PATH:+--cold-load-from "$NL_COLD_IMAGE_PATH"} \
    -L "$REPO/lisp" -L "$SHARED_LISP" -L "$FFI_ROOT/packages/nl-ffi/src" \
    --load "$LOAD_WRAPPER" --load "$BATCH_DRIVER" \
    >"$out" 2>"$err" || rc=$?
finish=$(date +%s.%N)
case $rc in
  0) batch_status=PASS ;;
  1) batch_status=FAIL ;;
  124) batch_status=TIMEOUT ;;
  *) batch_status=CRASH ;;
esac
LC_ALL=C awk -v a="$start" -v b="$finish" 'BEGIN { printf "%.3f\n", (b - a) }' >"$durationfile"
while IFS="$(printf '\t')" read -r c_label c_check_kind _ _; do
  [ -n "${c_label:-}" ] || continue
  resultfile=$RUN_DIR/$c_label.$c_check_kind.result
  if [ ! -r "$resultfile" ]; then
    printf '%s\t%s\t%s\t%s\n' "$c_label" "$c_check_kind" "$batch_status" "$rc" >"$resultfile"
  fi
done <"$chunkfile"
if [ "$batch_status" != PASS ]; then
  echo "NOT-PASS batch $label rc=$rc status=$batch_status (chunk=$chunkfile)" >&2
fi
BATCH_WORKER_EOF
chmod +x "$batch_worker"

# --- 7. One pass per artifact: normal load, then the three corruptions ---

# This pass stays sequential: per-label work here is generation (cp,
# objcopy, truncate, a small python edit) and a sha256 integrity check,
# all cheap next to a check's own $binary invocation, and every
# corrupted-copy path it writes ($text_copy/$trunc_copy/$abi_copy, plus
# make_corrupt_text's own "$dst.text.bin") is namespaced by "$label",
# unique across the whole corpus (verified: no two corpus_list rows ever
# share a label) -- so nothing generated here can collide with another
# label's files even though the checks this pass only enqueues will
# later execute concurrently. Only the checks themselves -- each its own
# process, each already namespaced by "$label.$check_kind" -- are
# deferred into the worker pool below.
while read -r label src; do
  [ -n "${label:-}" ] || continue
  if [ -z "${src:-}" ] || [ ! -r "$src" ]; then
    echo "SKIP $label: source not readable: ${src:-<empty>}" >&2
    emit_row "$label" "source" "SKIP" "-"
    continue
  fi
  before=$(sha256sum "$src" | cut -d ' ' -f1)

  # normal: a genuine artifact, proven never to reach dlopen except by
  # legitimately admitting (never a corrupted-code crash risk) -- batched.
  queue_safe_check "$label" normal "$src" any

  text_copy=$run_dir/$label.corrupt-text.eln
  if make_corrupt_text "$src" "$text_copy" 2>"$run_dir/$label.corrupt-text.make.stderr"; then
    # corrupt-text: traced actually reaching `nl-ffi--dlopen' on at least
    # one real corpus artifact whose .text exceeds the CRT-stub template's
    # own byte-exact preopen-validated prefix (192 bytes) -- stays isolated.
    enqueue_check "$label" corrupt-text "$text_copy" reject
  else
    echo "could not build corrupt-text copy for $label" >&2
    emit_row "$label" "corrupt-text" "FAIL" "make-error"
  fi

  trunc_copy=$run_dir/$label.corrupt-truncate.eln
  if make_corrupt_truncate "$src" "$trunc_copy" 2>"$run_dir/$label.corrupt-truncate.make.stderr"; then
    # corrupt-truncate: traced as never reaching `nl-ffi--dlopen' on any
    # probed artifact -- ELF section/symbol-table parsing in
    # nelisp-eln-system-loader--file-symbols runs, and fails, before the
    # preopen validator and dlopen even see it -- batched.
    queue_safe_check "$label" corrupt-truncate "$trunc_copy" reject
  else
    echo "could not build corrupt-truncate copy for $label" >&2
    emit_row "$label" "corrupt-truncate" "FAIL" "make-error"
  fi

  # corrupt-abi: the ABI-hash text `make_corrupt_abi' overwrites lives
  # outside every section the preopen validator authenticates (.init/.plt/
  # .plt.got/.fini/CRT-stub/.dynamic); traced actually reaching
  # `nl-ffi--dlopen' on real corpus artifacts -- stays isolated.
  abi_copy=$run_dir/$label.corrupt-abi.eln
  if make_corrupt_abi "$src" "$abi_copy" 2>"$run_dir/$label.corrupt-abi.make.stderr"; then
    enqueue_check "$label" corrupt-abi "$abi_copy" reject
  else
    echo "could not build corrupt-abi copy for $label" >&2
    emit_row "$label" "corrupt-abi" "FAIL" "make-error"
  fi

  after=$(sha256sum "$src" | cut -d ' ' -f1)
  if [ "$before" != "$after" ]; then
    echo "a check mutated the source artifact: $label ($src)" >&2
    emit_row "$label" "source-mutated" "FAIL" "-"
  fi
done <"$corpus_list"

# --- 7a. Partition $safe_list into $safe_chunks batch-worker jobs ---
#
# Round-robin, not size-based: every artifact in this corpus is one
# GNU-31.1-toolchain-compiled single-function .eln in the same ~16-22KB
# range (verified across the whole corpus during the S7.7.4 investigation),
# so per-check cost is already close to uniform and a simple `NR % chunks'
# split balances the chunks about as well as anything fancier would.
# Each resulting chunk becomes exactly one line appended to $queue --
# check_kind "safe-batch" is never a real check kind (those are normal/
# corrupt-text/corrupt-truncate/corrupt-abi), so step 7b's xargs dispatch
# below can tell a batch dispatch apart from an isolated one on sight.
# Only non-empty chunks are queued: a cold corpus (every source missing)
# must not spawn pointless empty-chunk processes.
if [ -s "$safe_list" ]; then
  safe_count=$(wc -l <"$safe_list")
  chunks=$safe_chunks
  [ "$chunks" -le "$safe_count" ] || chunks=$safe_count
  [ "$chunks" -ge 1 ] || chunks=1
  awk -v chunks="$chunks" -v prefix="$run_dir/safe-batch-" '
    { file = prefix ((NR - 1) % chunks) ".tsv"; print > file }
  ' "$safe_list"
  i=0
  while [ "$i" -lt "$chunks" ]; do
    chunkfile=$run_dir/safe-batch-$i.tsv
    if [ -s "$chunkfile" ]; then
      printf 'safe-batch-%s\tsafe-batch\t%s\t-\n' "$i" "$chunkfile" >>"$queue"
    fi
    i=$((i + 1))
  done
  echo "safe batch: $safe_count check(s) across $chunks process(es) (NELISP_ELN_GATE_SAFE_CHUNKS=${NELISP_ELN_GATE_SAFE_CHUNKS:-unset})" >&2
fi

# --- 7b. Order $queue longest-first, then drain it with `xargs -P' ---
#
# Longest-processing-time-first (LPT): sorted by each (label, check_kind)
# pair's most recently measured duration, read from $duration_cache (this
# run's own checks have not run yet, so a previous run's numbers are the
# only measurement there is to schedule by). A pair never measured before
# sorts first (the 1e9 sentinel below) -- unmeasured is treated as "assume
# slow", the safe default on a cold cache -- and ties break by original
# queue order for determinism. This only picks DISPATCH order: 7c always
# replays rows from $order, in the sequence step 7 decided, never from
# completion order, so results/ordering/totals do not depend on it. LPT
# matters because the 4 checks queued back-to-back for one label (which,
# unlike small vendor fixtures, may be a much bigger GNU increment/
# decrement/chain artifact) would otherwise land together and, run
# fastest-or-arbitrary-first, leave one slow straggler to finish alone
# after every other check is already done -- LPT starts the slow ones
# immediately, overlapping them with everything else.
#
# Concurrency is `xargs -P "$jobs"' itself, not a shell loop: xargs is the
# one process managing the pool (fork/wait4 in C), so a finished check's
# slot is reused the instant it exits -- no poll tick, no idle slack at a
# batch boundary the way a batch-and-`wait' loop has (measured slower: a
# find+wc poll loop cost MORE than batch-and-wait's own slack, and plain
# batch-and-wait leaves stragglers idling the rest of their batch). No two
# queued checks ever share a mutable path (see $worker's own commentary),
# so running them concurrently is safe regardless of dispatch order.
queue_ordered=$run_dir/queue-ordered.tsv
# `FILENAME == durfile', not the usual `NR == FNR' two-file idiom: NR==FNR
# is only true for the first file's lines, but on a cold cache
# $duration_cache is legitimately empty, and NR==FNR then stays true for
# $queue's own first lines too (FNR resets per file, but with zero lines
# consumed from an empty first file, NR and FNR run in lockstep from the
# very first line of the second file onward) -- routing all of $queue into
# the "build the map" branch and leaving $queue_ordered empty. Comparing
# FILENAME instead identifies which file a record came from directly, so
# an empty $duration_cache (every run's first one) is handled the same as
# a populated one.
LC_ALL=C awk -F'\t' -v durfile="$duration_cache" '
  FILENAME == durfile { duration[$1 SUBSEP $2] = $3; next }
  {
    idx++
    key = duration[$1 SUBSEP $2]
    if (key == "") key = 1e9
    printf "%.6f\t%d\t%s\t%s\t%s\t%s\n", key, idx, $1, $2, $3, $4
  }
' "$duration_cache" "$queue" \
  | LC_ALL=C sort -t "$(printf '\t')" -k1,1nr -k2,2n \
  | cut -f3- >"$queue_ordered"

export NL_COLD_IMAGE_PATH RUN_DIR=$run_dir BINARY=$binary REPO=$repo SHARED_LISP=$shared_lisp \
       FFI_ROOT=$ffi_root LOAD_WRAPPER=$load_wrapper DRIVER=$driver \
       BATCH_DRIVER=$batch_driver TIMEOUT_SECS=$timeout_secs WORKER=$worker \
       BATCH_WORKER=$batch_worker
# check_kind "safe-batch" (never a real check kind) routes to
# $BATCH_WORKER instead of $WORKER; see step 7a above. Both branches are
# still just one `exec' per queue line -- xargs's own fork/wait4 pool
# still owns all the concurrency, unchanged from before this dispatch
# gained a second worker script.
xargs -d '\n' -P "$jobs" -I{} sh -c '
  line=$1
  IFS="$(printf "\t")"
  set -- $line
  if [ "$2" = "safe-batch" ]; then
    exec "$BATCH_WORKER" "$1" "$3"
  else
    exec "$WORKER" "$1" "$2" "$3" "$4"
  fi
' _ {} <"$queue_ordered"

# Fold this run's real measurements back into $duration_cache so the next
# run's LPT order is sorted by them instead of the sentinel.
new_durations=$run_dir/new-durations.tsv
: >"$new_durations"
while IFS="$(printf '\t')" read -r q_label q_check_kind _ _; do
  [ -n "${q_label:-}" ] || continue
  durationfile=$run_dir/$q_label.$q_check_kind.duration
  [ -r "$durationfile" ] || continue
  printf '%s\t%s\t%s\n' "$q_label" "$q_check_kind" "$(cat "$durationfile")" >>"$new_durations"
done <"$queue"
merged_durations=$run_dir/durations-merged.tsv
# FILENAME, not `FNR == NR' (see queue_ordered's own commentary above for
# why): $new_durations is normally non-empty, but a run where every check
# was SKIPped would leave it empty, and FNR==NR would then read every
# line of $duration_cache as though it were "new" -- harmless here (each
# ends up printed exactly once either way), but not by design.
awk -F'\t' -v newfile="$new_durations" '
  FILENAME == newfile { new[$1 SUBSEP $2] = $0; next }
  !(($1 SUBSEP $2) in new) { print }
  END { for (k in new) print new[k] }
' "$new_durations" "$duration_cache" >"$merged_durations"
cp "$merged_durations" "$duration_cache"

# --- 7c. Merge: replay $order into $results in its recorded sequence ---
while IFS= read -r resultfile; do
  [ -n "${resultfile:-}" ] || continue
  if [ ! -r "$resultfile" ]; then
    echo "missing result file (a queued check never wrote it): $resultfile" >&2
    printf '%s\t%s\t%s\t%s\n' "unknown" "unknown" "FAIL" "missing-result" >>"$results"
    continue
  fi
  cat "$resultfile" >>"$results"
done <"$order"

# --- 8. Negative control: a gate variant that skips the reporter assertion ---
# must be shown to MISS an injected inconsistency that the real (asserting)
# gate correctly DETECTS. Two processes, neither touching a real artifact.
#
# Neither process ever calls `load' on anything -- the injected
# inconsistency is built directly out of Lisp vectors
# (nelisp-eln-crash-corpus--inject-inconsistency) -- so, unlike every
# other check in this gate, these two have no need for the ELN load
# wrapper's `load'-routing hooks at all, only for the two `require's
# that define nelisp-eln-registration--owner-size/--owner-marker/
# --owners/--pending-cleanups and the reporter itself. Plain `-L' plus
# `require' (confirmed: same CORPUS_RESULT/exit-code pair as the
# wrapper-loaded form) skips that dead weight for both processes.
neg_detect_rc=0
NELISP_ELN_CRASH_CORPUS_JOB=negative-control \
NELISP_ELN_CRASH_CORPUS_SKIP_REPORTER=0 \
  timeout "$timeout_secs" "$binary" ${NL_COLD_IMAGE_PATH:+--cold-load-from "$NL_COLD_IMAGE_PATH"} \
    -L "$repo/lisp" -L "$shared_lisp" -L "$ffi_root/packages/nl-ffi/src" \
    --load "$driver" \
    >"$run_dir/negative-control-assert.stdout" 2>"$run_dir/negative-control-assert.stderr" || neg_detect_rc=$?

neg_skip_rc=0
NELISP_ELN_CRASH_CORPUS_JOB=negative-control \
NELISP_ELN_CRASH_CORPUS_SKIP_REPORTER=1 \
  timeout "$timeout_secs" "$binary" ${NL_COLD_IMAGE_PATH:+--cold-load-from "$NL_COLD_IMAGE_PATH"} \
    -L "$repo/lisp" -L "$shared_lisp" -L "$ffi_root/packages/nl-ffi/src" \
    --load "$driver" \
    >"$run_dir/negative-control-skip.stdout" 2>"$run_dir/negative-control-skip.stderr" || neg_skip_rc=$?

if [ "$neg_detect_rc" -eq 1 ] && \
   grep -Fxq 'CORPUS_RESULT negative-control-assert=DETECTED' "$run_dir/negative-control-assert.stdout" && \
   [ "$neg_skip_rc" -eq 0 ] && \
   grep -Fxq 'CORPUS_RESULT negative-control-skip=PASS-BUT-UNSOUND' "$run_dir/negative-control-skip.stdout"; then
  neg_status=PASS
  echo "negative control: the reporter assertion catches the injected inconsistency (exit $neg_detect_rc); a gate variant that skips it misses it (exit $neg_skip_rc)." >&2
else
  neg_status=FAIL
  echo "negative control FAILED: assert-mode rc=$neg_detect_rc skip-mode rc=$neg_skip_rc" >&2
fi
printf 'negative-control\tassert-vs-skip\t%s\t%s/%s\n' "$neg_status" "$neg_detect_rc" "$neg_skip_rc" >>"$results"

# --- 9. Report: the detailed table, one consolidated line per artifact, and a summary ---

echo ""
echo "=== detailed results ==="
awk -F'\t' '{printf "%-28s %-16s %-8s rc=%s\n", $1, $2, $3, $4}' "$results"

echo ""
echo "=== one line per artifact ==="
awk -F'\t' '
  { st[$1, $2] = $3 }
  !seen[$1]++ { order[++n] = $1 }
  END {
    for (i = 1; i <= n; i++) {
      label = order[i]
      printf "%-28s normal=%-5s corrupt-text=%-5s corrupt-truncate=%-5s corrupt-abi=%-5s\n", \
        label, st[label,"normal"], st[label,"corrupt-text"], \
        st[label,"corrupt-truncate"], st[label,"corrupt-abi"]
    }
  }' "$results"

total=$(wc -l <"$results")
fail_count=$(awk -F'\t' '$3 != "PASS" && $3 != "SKIP" {c++} END{print c+0}' "$results")
echo ""
echo "NELISP-ELN-CRASH-CORPUS-GATE total=$total fail=$fail_count"
if [ "$fail_count" -gt 0 ]; then
  exit 1
fi
echo "NELISP-ELN-CRASH-CORPUS-GATE-PASS"
