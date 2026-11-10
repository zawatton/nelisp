#!/usr/bin/env bash
# Inject a known defect, require the named gate to go RED, restore.
#
# A gate is a claim that a class of defect cannot land.  The claim is only
# worth what it has been tested against, and testing a checker means giving
# it something to catch.  Three checks were green while seeing nothing on
# 2026-08-19; each would have been caught by one row of gate-mutations.txt.
set -u
cd "$(dirname "$0")/.." || exit 1
# "Text file busy" happens when the previous gate's subprocess still holds
# ./target/nelisp; it clears in well under a second, so one retry is enough
# and a second failure is a real build error.
# Which standalone target the rebuilds and the gates must use.  Restated on
# every `make' below as a COMMAND-LINE variable rather than left to the
# environment, for the reason tools/nelisp-reader-smokes.sh records at
# length: on at least one host this repository is developed on, `make' is a
# Cygwin build running MSYS2's `/bin/sh' and NOTHING crosses that boundary,
# so an environment-only target silently becomes the default one.  Here
# that would rebuild and run the LINUX binary while the row's gate expects
# the Windows one -- every gate would fail for the wrong reason, and this
# harness reads "the gate failed" as "the row is lethal", i.e. it would
# report every row as proved without any of them having been exercised.
MUTATION_TARGET_ARG=""
MUTATION_TARGET=${NELISP_STANDALONE_TARGET:-}
if [ -z "$MUTATION_TARGET" ]; then
  # Do not inherit Make's platform default implicitly.  On this Windows host
  # the harness runs under Git Bash while `make' runs an MSYS2 recipe shell;
  # relying on OS crossing that boundary selected linux-x86_64 in one process
  # and windows-x86_64 in another.  Focused reader gates then linked
  # target/nelisp but Windows process lookup ran the pre-existing
  # target/nelisp.exe, so two mutations repeatedly tested the wrong binary.
  case "$(uname -s 2>/dev/null || true)" in
    MINGW*|MSYS*|CYGWIN*) MUTATION_TARGET=windows-x86_64 ;;
    *) MUTATION_TARGET=linux-x86_64 ;;
  esac
fi
# Always restate the resolved target as a make COMMAND-LINE variable.  The
# environment-only spelling is known not to cross this repository's
# Cygwin/MSYS2 make boundary reliably.
MUTATION_TARGET_ARG="NELISP_STANDALONE_TARGET=$MUTATION_TARGET"
printf 'gate-mutation: target %s\n' "$MUTATION_TARGET"

# The nl-num mutation is bounded below the harness's hard 90-second ceiling.
# A shorter value is useful for exercising the timeout classification without
# waiting for the full diagnostic budget; values above the ceiling are
# rejected so a caller cannot accidentally reintroduce an unbounded sweep.
nl_num_mutation_timeout=${NELISP_GATE_MUTATION_TIMEOUT:-90}
case "$nl_num_mutation_timeout" in
  ''|*[!0-9]*)
    echo "gate-mutation: FAIL (NELISP_GATE_MUTATION_TIMEOUT must be an integer in 1..90)"
    exit 1 ;;
esac
if [ "$nl_num_mutation_timeout" -lt 1 ] || [ "$nl_num_mutation_timeout" -gt 90 ]; then
  echo "gate-mutation: FAIL (NELISP_GATE_MUTATION_TIMEOUT must be an integer in 1..90)"
  exit 1
fi

# Two further budgets, validated the same loud way.  A timeout is never
# evidence that a gate is lethal: it files the row as a named failure.
#   NELISP_GATE_MUTATION_ROW_TIMEOUT    each single gate invocation (s)
#   NELISP_GATE_MUTATION_BUILD_TIMEOUT  each `make standalone-reader' attempt
#                                       (s); a mutation in a widely used
#                                       source can recompile most units, so
#                                       builds are budgeted apart from gates
#   NELISP_GATE_MUTATION_SLOW_ROW       rows at least this slow get a line
require_positive_int() {
  case "$2" in
    ''|*[!0-9]*) echo "gate-mutation: FAIL ($1 must be a positive integer, got '$2')"; exit 1 ;;
  esac
  if [ "${#2}" -gt 9 ] || [ "$2" -lt 1 ]; then
    echo "gate-mutation: FAIL ($1 must be a positive integer, got '$2')"
    exit 1
  fi
}
row_timeout=${NELISP_GATE_MUTATION_ROW_TIMEOUT:-1200}
build_timeout=${NELISP_GATE_MUTATION_BUILD_TIMEOUT:-2700}
slow_row=${NELISP_GATE_MUTATION_SLOW_ROW:-60}
require_positive_int NELISP_GATE_MUTATION_ROW_TIMEOUT "$row_timeout"
require_positive_int NELISP_GATE_MUTATION_BUILD_TIMEOUT "$build_timeout"
require_positive_int NELISP_GATE_MUTATION_SLOW_ROW "$slow_row"
# Shard-budget ESTIMATE for a rebuild row (a row that rebuilds the default
# binary around its injection).  The default is the hard worst case with the
# default timeouts (build_timeout 2700 + row_timeout 1200 = 3900 s), so a row
# that starts always ends inside the shard budget.  A measured 3300 s (largest
# rebuild-row total 2379 s on run 42578356513 plus the 38.5% spread of ffi-dsl;
# tools/ai/ci-speed-evidence/s7-rebuild-row-times.txt) was tried on run
# 42596869405: there it admitted rebuild rows late in two shards that then hit
# the 1200 s row timeout after about 2840 s (fewer rows ran on that run, 43 of
# 120 against 71, mostly because its runners were slower).  Override with
# NELISP_GATE_MUTATION_REBUILD_ESTIMATE; row_worst_case never returns more
# than build_timeout + row_timeout.
rebuild_row_estimate=${NELISP_GATE_MUTATION_REBUILD_ESTIMATE:-3900}
require_positive_int NELISP_GATE_MUTATION_REBUILD_ESTIMATE "$rebuild_row_estimate"
# Optional budget for the whole shard (seconds since this harness started).
# Unset means no budget.  Each row is judged on its own estimate against
# the time left: a row that does not fit is reported NOT RUN (and fails the
# shard), while a later, cheaper row that does fit is still run.
shard_budget=${NELISP_GATE_MUTATION_SHARD_BUDGET:-}
if [ -n "$shard_budget" ]; then
  require_positive_int NELISP_GATE_MUTATION_SHARD_BUDGET "$shard_budget"
fi
not_run=0; selected_total=0; assigned=0

# A timeout(1) exit of 124 (TERM) or 137 (the --kill-after KILL took the
# whole group, `timeout' included) only counts as a budget timeout when the
# budget was really used up, so a gate that exits 124 by itself is not
# mislabelled.  Sets rebuild_timed_out; a timed-out attempt is not retried.
#
# timeout(1) exits as soon as its direct child has, so a descendant that
# ignores TERM would outlive --kill-after.  timeout puts itself in a process
# group of its own (its pid is the group id), so the job runs in the
# background and a timeout is followed by a KILL of that whole group.
bounded_pid=""
wait_bounded() {
  local pid="$1" rc
  bounded_pid="$pid"
  wait "$pid"
  rc=$?
  if [ "$rc" -eq 124 ] || [ "$rc" -eq 137 ]; then
    kill -s KILL -- "-$pid" 2>/dev/null || true
  fi
  bounded_pid=""
  return "$rc"
}
rebuild_timed_out=0
rebuild_seq=0
# Set only around an injected build (see unit_scratch_make).  It is passed
# through `env' on that one command and never exported: the variable is part
# of the stamp key, so a leak would make every later status read `key changed'.
# The injected build also gets NELISP_VERSION=<baseline version>: the build
# embeds `git describe --dirty' (scripts/nelisp-version.el), the baseline comes
# from a clean checkout and every injected build runs in a dirty tree, so
# without it the injected set would always differ and the injection check could
# never fire.  nelisp-build-source-revision (HEAD) is not affected by a working
# tree edit.  Like the cache variable, it is passed through `env' on that one
# command only.
baseline_version=""
rebuild_cache_dir=""
# Print the end of a failed attempt's build log under a header that names
# the row, so a red rebuild is diagnosable from the CI log alone.
show_rebuild_tail() {
  local context="$1" blog="$2"
  echo "gate-mutation: rebuild log tail ($context; log $blog)"
  if [ -s "$blog" ]; then
    tail -40 "$blog" | sed 's/^/    /'
  else
    echo "    (build produced no output)"
  fi
}
# Each attempt's build output goes to its own file under
# target/gate-mutation-logs/ (redirected into the file, not through a pipe,
# so $! and the exit code stay those of timeout).  Nothing extra is printed
# for a successful build.  $1 optionally names the context for the header.
rebuild_checked() {
  local attempt rc t0 context blog safe
  context="${1:-gate ${gate:-<none>}, file ${file:-<none>}}"
  rebuild_timed_out=0
  mkdir -p target/gate-mutation-logs
  safe=$(printf '%s' "${gate:-warmup}" | tr -c 'A-Za-z0-9._-' '_')
  rebuild_seq=$((rebuild_seq + 1))
  for attempt in 1 2; do
    t0=$SECONDS
    blog="target/gate-mutation-logs/rebuild-$rebuild_seq-$safe-attempt$attempt.log"
    if [ -n "$rebuild_cache_dir" ]; then
      env "NELISP_STANDALONE_UNIT_CACHE_DIR=$rebuild_cache_dir" "NELISP_VERSION=$baseline_version" \
        timeout --kill-after=10s "$build_timeout" make standalone-reader $MUTATION_TARGET_ARG >"$blog" 2>&1 &
    else
      timeout --kill-after=10s "$build_timeout" make standalone-reader $MUTATION_TARGET_ARG >"$blog" 2>&1 &
    fi
    wait_bounded $!
    rc=$?
    [ "$rc" -eq 0 ] && return 0
    if { [ "$rc" -eq 124 ] || [ "$rc" -eq 137 ]; } && [ $((SECONDS - t0)) -ge "$build_timeout" ]; then
      rebuild_timed_out=1
      show_rebuild_tail "$context, TIMEOUT after ${build_timeout}s, attempt $attempt" "$blog"
      return 1
    fi
    [ "$attempt" = 1 ] && sleep 2
  done
  show_rebuild_tail "$context, failed twice, last exit $rc" "$blog"
  return 1
}

# Set once a rebuild timed out: the binary may be partial or built from
# injected source.  The standalone build deletes its stamp before building, so
# a later `make standalone-reader' rebuilds, but a gate whose prerequisite is
# only "binary absent" would happily run the partial file.  The next row
# therefore starts with a clean rebuild (see the row loop).
binary_dirty=0

report_harness_log() {
  local label="$1" log="$2"
  echo "gate-mutation: $label"
  if [ -s "$log" ]; then
    tail -80 "$log"
  else
    echo "  (command produced no output)"
  fi
}

# Does this row need the binary rebuilt around it?  True when the file it
# mutates is BAKED INTO the binary at build time, which is most of
# scripts/ and lisp/: the gates below all carry the conditional
# prerequisite `$(if $(wildcard target/nelisp target/nelisp.exe),,
# standalone-reader)', i.e. build only when the binary is ABSENT, never
# when the source is newer.  Without a rebuild the row runs the old
# artifact and reads as PASS on code that is no longer there.
#
# This predicate exists because the same list used to be written out three
# times -- once for the rebuild WITH the injection, once on the skip path,
# once for the rebuild after RESTORING it -- and when Doc 201 added four
# gates to it, only the first copy was updated.  The visible consequence
# was not a false PASS but a false FAILURE later: the row rebuilt with its
# injection, restored the source, skipped the second rebuild, and left the
# tree holding a binary built from injected source, so the next unrelated
# run of that gate went red with nothing wrong in the tree.  One list.
gate_needs_rebuild() {
  case "$1" in
    emacs-parity) return 0 ;;
    # Drives target/nelisp and its Makefile target only builds when the
    # binary is ABSENT, so without this the row would test the OLD binary
    # and report STAYED GREEN against an injection that never reached it.
    nelisp-sexp-clone-bind-smoke) return 0 ;;
    standalone-reader-buffer-smoke) return 0 ;;
    standalone-reader-bignum-smoke) return 0 ;;
    standalone-reader-test) return 0 ;;
    standalone-reader-form-location-smoke) return 0 ;;
    standalone-reader-frame-stack-pop-desync-smoke) return 0 ;;
    standalone-reader-socket-smoke) return 0 ;;
    precise-root-coverage) return 0 ;;
    standalone-reader-ipv6-socket-smoke) return 0 ;;
    standalone-reader-ffi-smoke) return 0 ;;
    # ffi-dsl's own recipe rebuilds the dynamic reader unconditionally,
    # the same way standalone-reader-ffi-smoke above does -- listed here
    # for the same reason: a stale binary from a DIFFERENT gate earlier in
    # the same sweep must not stand in for the "before" baseline.
    ffi-dsl) return 0 ;;
    standalone-reader-host-parity-smoke) return 0 ;;
    standalone-reader-winpath-smoke) return 0 ;;
    standalone-reader-defvar-alloc-smoke) return 0 ;;
    standalone-reader-alloc-bytes-int-root-smoke) return 0 ;;
    standalone-reader-fileattrs-smoke) return 0 ;;
    standalone-reader-splitstring-perf-smoke) return 0 ;;
    standalone-reader-regexp-lead-filter-smoke) return 0 ;;
    standalone-reader-print-large-sexp-smoke) return 0 ;;
    nelisp-thread-standalone-smoke) return 0 ;;
    nelisp-thread-allocating-standalone-smoke) return 0 ;;
    nelisp-thread-mirror-guard-standalone-smoke) return 0 ;;
    standalone-midform-gc-bounded) return 0 ;;
    standalone-reader-nonblocking-socket-smoke) return 0 ;;
    standalone-reader-tls-smoke) return 0 ;;
    # Both carry the unconditional prerequisite `standalone-reader', so an
    # injection into their package source (nelisp-process-adapter.el) was
    # rebuilt INSIDE the 1200 s row budget and timed out (CI run 42748394272,
    # shard 2).  Rebuilding first, under the build budget, leaves the
    # prerequisite a stamp hit.
    standalone-reader-dns-smoke) return 0 ;;
    standalone-reader-network-process-nowait-smoke) return 0 ;;
    # These two run the shared target/nelisp, which their Makefile targets
    # build only when it is absent.  native-symbol-test rebuilds its own
    # fixture on every run.
    special-variables) return 0 ;;
    symbol-identity) return 0 ;;
    # Different reason from every entry above: this row's own mutated file
    # is `Makefile', never baked into `target/nelisp'.  What needs the
    # rebuild is `binary-size-ratchet''s OWN freshness check (2026-09-17),
    # which refuses to judge a `target/nelisp' older than any `.el' under
    # lisp/src/scripts.  `mutation_restore' uses `cp' with no `-p' (line
    # ~159) and every OTHER row's `sed -i' (line ~427) both stamp the
    # current time on the file they touch, so after the first `.el'-
    # mutating row anywhere earlier in a sweep, every source file under
    # those three directories is newer than whatever `target/nelisp' was
    # sitting in `target/' when the sweep started.  Without this arm this
    # row's own gate then reports its own GATE-SKIP for the rest of the
    # sweep, `run_gate' sees no findings, and the skip-vs-fail branch below
    # (same skip reason on the restored clean tree) files it as "SKIP (gate
    # not runnable on this host)" -- a full sweep goes green having tested
    # this row exactly once, by accident, and only if it happens to sit
    # before every `.el'-touching row in the table.  Confirmed by hand
    # 2026-09-17: with an unrelated `.el' deliberately left newer than
    # `target/nelisp' (the state every later row in a real sweep leaves
    # behind), `NELISP_GATE_MUTATION_ONLY=binary-size-ratchet
    # tools/nelisp-gate-mutation.sh' reported `skipped=1' for this row
    # without this arm, `passed=1 skipped=0' with it -- see
    # target/ai/ratchet-freshness-report.md for the full transcript.  Two
    # rebuilds per sweep (before the mutated run, again after restoring so
    # the NEXT row does not inherit a Makefile-mutated binary) at ~25s each
    # with a warm unit cache -- ~50s added to a sweep that already runs
    # ~2547s, for a row this gate cannot otherwise be trusted to reach.
    binary-size-ratchet) return 0 ;;
  esac
  # Doc 200: an `ert-full' row is only binary-sensitive when it mutates the
  # standalone build script itself.
  [ "$1:$2" = "ert-full:scripts/nelisp-standalone-build.el" ]
}

# Gates whose recipe builds its OWN variant binary (target/nelisp-dyn,
# target/nelisp-chunk8m) from the current sources and runs only that.  Their
# gate run is a build, so it gets the build budget, not the row budget.  They
# also rewrite the fixed-path companions of the default build, so the default
# binary is restored from the baseline copy after them.  One list; a test
# parses it.  `standalone-reader-tls-smoke' is the table's name, the -linux
# spelling is the Makefile target it dispatches to.
gate_builds_variant() {
  case "$1" in
    standalone-chunk-growth-test) return 0 ;;
    standalone-reader-ffi-smoke) return 0 ;;
    ffi-dsl) return 0 ;;
    standalone-reader-tls-smoke) return 0 ;;
    standalone-reader-tls-smoke-linux) return 0 ;;
  esac
  return 1
}

# Gates whose own recipe compiles native units in the gate run itself (not a
# variant binary the harness can skip rebuilding for): `standalone-reader-test'
# compiles and runs its reader fixtures, `standalone-eval-test' compiles the
# reader units with EVAL-P into target/nelisp-standalone-eval.  An injection
# into a toolchain file (scripts/nelisp-standalone-build.el, the reader parser)
# misses every unit cache entry, so that compile is a cold one: both rows hit
# the 1200 s row budget on CI run 42748394272 (shard 3) before judging
# anything.  They run under the build budget instead.
gate_run_compiles() {
  case "$1" in
    standalone-reader-test) return 0 ;;
    standalone-eval-test) return 0 ;;
  esac
  return 1
}

# A rebuild only changes anything when the mutated file is a build input.  The
# lists are read from the stamp tool, which owns them; copying them here would
# let the two drift.
stamp_tool=tools/nelisp-standalone-reader-build.sh
build_key_dirs=$(sed -n 's/^KEY_DIRS="\(.*\)"$/\1/p' "$stamp_tool" 2>/dev/null)
build_key_files=$(sed -n 's/^KEY_FILES="\(.*\)"$/\1/p' "$stamp_tool" 2>/dev/null)
if [ -z "$build_key_dirs" ] || [ -z "$build_key_files" ]; then
  echo "gate-mutation: FAIL (cannot read KEY_DIRS/KEY_FILES from $stamp_tool; build inputs are unknown)"
  exit 1
fi
file_is_build_input() {
  local f="$1" d k
  for d in $build_key_dirs; do
    case "$f" in "$d"/*) return 0 ;; esac
  done
  for k in $build_key_files; do
    [ "$f" = "$k" ] && return 0
  done
  return 1
}
# Does this row rebuild around its injection?
# A variant-building gate builds AND runs its own binary (target/nelisp-dyn,
# target/nelisp-chunk8m) and never executes target/nelisp (Makefile:3607-3611
# ffi-dsl, 3406-3532 standalone-reader-ffi-smoke which runs STANDALONE_FFI_BIN =
# the dyn binary on Linux, 3714-3735 tls-smoke-linux; tools/chunk-growth-test.sh
# runs only VARIANT_OUT), so a default rebuild around it is wasted work.  Its
# clean binary is still restored afterwards: the variant build rewrites the
# fixed-path companions of the default build.
row_rebuilds() {
  gate_needs_rebuild "$1" "$2" && file_is_build_input "$2" && ! gate_builds_variant "$1"
}
# The "injection reached the build output" check applies only to source files
# under the stamp's KEY_DIRS.  A KEY_FILE (Makefile, the parity .tsv, the opcode
# .json, the stamp tool) is part of the stamp key but need not change the output.
file_in_key_dirs() {
  local d
  for d in $build_key_dirs; do
    case "$1" in "$d"/*) return 0 ;; esac
  done
  return 1
}

# The set of files one clean build produces.  The single definition is the
# upload list of job `build' in .github/workflows/ci.yml; a test compares the
# two.  An entry ending in `/' is a directory.
# BASELINE-SET-BEGIN
baseline_set='
target/nelisp
target/nelisp.cold
target/nelisp.build-stamp
target/nelisp.build-stamp.inputs
target/nelisp.version.json
target/nelisp.native-startup.el
target/nelisp.buffer-local-bytecode.tsv
target/nelisp.variable-alias-bytecode.tsv
target/nelisp-artifact-runtime.el
target/nelisp-artifact-runtime.el.nelc
target/nelisp-artifact-runtime.el.nelc.enable
target/nelisp-artifact-runtime.el.nelc.manifest.el
target/nelisp-core-bytecode-report.tsv
target/nelisp-prelude-bytecode-report.tsv
target/nelisp-regexp-bytecode-report.tsv
target/nelisp-vendor-bytecode-report.tsv
target/nelisp-boot-image/
'
# BASELINE-SET-END
baseline_dir=target/gate-mutation-baseline
baseline_ready=0
baseline_bin_sha=""
baseline_set_sha=""

# sha256 of a file, of a directory's file listing, or `absent'.
sha_of() {
  if [ -d "$1" ]; then
    (cd "$1" && find . -type f | LC_ALL=C sort | xargs -r sha256sum | sha256sum | cut -d' ' -f1)
  elif [ -f "$1" ]; then
    sha256sum "$1" | cut -d' ' -f1
  else
    echo absent
  fi
}

# One hash over the whole set except the stamp and its inputs sidecar (binary,
# .cold, companions, reports): "the injection reached the build output" means
# any of it changed.  The sidecar is derived from the key, so counting it would
# make that trivially true.
set_sha() {
  local f
  for f in $baseline_set; do
    case "$f" in target/nelisp.build-stamp|target/nelisp.build-stamp.inputs) continue ;; esac
    printf '%s %s\n' "$f" "$(sha_of "${f%/}")"
  done | sha256sum | cut -d' ' -f1
}

# Real copies (no hardlinks) of the clean build set, with their hashes.  A
# missing file is recorded as absent and must be absent after a restore.
baseline_save() {
  local f p
  rm -rf "$baseline_dir"
  mkdir -p "$baseline_dir" || return 1
  : >"$baseline_dir/manifest.txt"
  for f in $baseline_set; do
    p=${f%/}
    if [ -e "$p" ]; then
      mkdir -p "$baseline_dir/$(dirname "$p")" || return 1
      cp -R "$p" "$baseline_dir/$p" || return 1
    fi
    printf '%s %s\n' "$p" "$(sha_of "$p")" >>"$baseline_dir/manifest.txt"
  done
  baseline_bin_sha=$(sha_of target/nelisp)
  baseline_set_sha=$(set_sha)
  baseline_version=$(sed -n 's/.*"version" *: *"\([^"]*\)".*/\1/p' "$baseline_dir/target/nelisp.version.json" 2>/dev/null | head -1)
  baseline_ready=1
}

# Copy the baseline set back with PLAIN cp, after the source was restored, so
# every file is newer than every source (binary-size-ratchet refuses a binary
# older than a source .el).  Derived files go last: the binary, then the stamp
# inputs sidecar, then the stamp.
baseline_restore() {
  local p
  for p in $(printf '%s\n' $baseline_set | sed 's:/$::' |
             grep -v -e '^target/nelisp$' -e '^target/nelisp.build-stamp$' \
                     -e '^target/nelisp.build-stamp.inputs$') \
           target/nelisp target/nelisp.build-stamp.inputs target/nelisp.build-stamp; do
    rm -rf "$p"
    if [ -e "$baseline_dir/$p" ]; then
      cp -R "$baseline_dir/$p" "$p" || return 1
    fi
  done
}

# Put the clean binary back after a row.  Restore by copy, then ask the stamp
# tool whether the tree is up to date; if not, say so loudly and rebuild.
# Never continues silently.
binary_may_be_dirty=0
restore_clean_binary() {
  local ctx="$1" st why
  rebuild_timed_out=0
  if [ "$baseline_ready" = 1 ] && baseline_restore; then
    st=$(timeout 300 make -s standalone-reader-status $MUTATION_TARGET_ARG 2>&1)
    case "$st" in
      *"up to date"*)
        if [ "$(sha_of target/nelisp)" = "$baseline_bin_sha" ]; then
          binary_may_be_dirty=0
          return 0
        fi
        why="restored binary differs from the baseline" ;;
      *) why=$(printf '%s\n' "$st" | tail -1) ;;
    esac
  else
    why="no usable baseline copy"
  fi
  echo "gate-mutation: RESTORE-BY-COPY NOT CONFIRMED ($ctx): ${why:-no status output}; falling back to a full rebuild"
  if rebuild_checked "$ctx (fallback rebuild)"; then
    binary_may_be_dirty=0
    return 0
  fi
  return 1
}

# A private copy of the unit cache for one injected build.  Hardlinks are safe
# because the build writes a temp name and renames it over the entry and
# prunes with unlink (scripts/nelisp-standalone-build.el, unit cache write
# path); it never writes into an existing entry.  The copy is removed right
# after the build, by the EXIT trap, and on INT/TERM.
unit_scratch=""
unit_scratch_make() {
  mkdir -p target/gate-mutation-units || return 1
  unit_scratch="target/gate-mutation-units/row-$row_index"
  rm -rf "$unit_scratch"
  if [ -d target/standalone-units ]; then
    if ! cp -al target/standalone-units "$unit_scratch" 2>/dev/null; then
      echo "gate-mutation: hardlink copy of the unit cache failed; using a full copy (slower)"
      rm -rf "$unit_scratch"
      cp -a target/standalone-units "$unit_scratch" || return 1
    fi
  else
    mkdir -p "$unit_scratch" || return 1
  fi
  rebuild_cache_dir="$(cd "$unit_scratch" && pwd)"
  # The build also writes target/nelisp-vendor-bytecode/ (scripts/nelisp-
  # standalone-build.el:49943 out-dir, 50006 with-temp-file; the path is fixed,
  # not redirectable).  Swap in a hardlink copy for the injected build only.
  if [ -d target/nelisp-vendor-bytecode ]; then
    vendor_orig="target/gate-mutation-units/vendor-orig-$row_index"
    rm -rf "$vendor_orig"
    mv target/nelisp-vendor-bytecode "$vendor_orig" || return 1
    cp -al "$vendor_orig" target/nelisp-vendor-bytecode ||
      cp -a "$vendor_orig" target/nelisp-vendor-bytecode || return 1
  fi
}
vendor_orig=""
unit_scratch_drop() {
  rebuild_cache_dir=""
  if [ -n "$vendor_orig" ] && [ -d "$vendor_orig" ]; then
    rm -rf target/nelisp-vendor-bytecode
    mv "$vendor_orig" target/nelisp-vendor-bytecode
  fi
  vendor_orig=""
  [ -n "$unit_scratch" ] && rm -rf "$unit_scratch"
  unit_scratch=""
  rmdir target/gate-mutation-units 2>/dev/null || true
}

# Seconds one row is expected to use, for the shard budget.  A non-variant
# rebuild row is charged `rebuild_row_estimate' (measured, see above) capped at
# its hard limit build_timeout + row_timeout; every other row is charged its
# hard limit.  This is a PRE-CHECK, not a hard guarantee: it ignores the second build attempt, the
# restore fallback rebuild and the clean-baseline check, which the 30 minutes
# between the 90 minute budget and the job's timeout-minutes 120 absorb.
# (Doubling BUILD would equal the whole budget, so no rebuild row would start.)
row_worst_case() {
  local g="$1" f="$2" w=0
  case "$g" in
    nl-num-standalone-smoke) echo $((3 * nl_num_mutation_timeout)); return ;;
    nelisp-thread-allocating-standalone-smoke) echo 30; return ;;
  esac
  if row_rebuilds "$g" "$f"; then w=$build_timeout; fi
  if gate_builds_variant "$g" || gate_run_compiles "$g"; then
    w=$((w + build_timeout))
  else
    w=$((w + row_timeout))
  fi
  if row_rebuilds "$g" "$f" && ! gate_builds_variant "$g" &&
     [ "$rebuild_row_estimate" -lt "$w" ]; then w=$rebuild_row_estimate; fi
  echo "$w"
}

# `ert-full' rows run a real (deliberately red) `nelisp-ai.sh test', which
# writes a gate report into `NELISP_GATE_DIR' (default `target/gates').
# Left alone that would overwrite the tree's real `ert-full' report with a
# broken one on every `gate-mutation' run.  Scratch it into a throwaway
# directory instead; nothing else reads this one.
mutation_gate_dir="$(mktemp -d)"

# Per-row timing.  finish_row runs when the next row starts (and from the
# summary), so every row's line is printed as soon as the row is over; the
# summary is also printed from the EXIT trap, so a failed or interrupted
# sweep still says where the time went.
timing_file="$mutation_gate_dir/row-times"
timing_started=0; timing_printed=0; warmup_secs=0
row_total=0; row_index=0; row_open=0; row_started=0; row_timed_out=0
row_gate=""; row_file=""
start_row() {
  row_open=1; row_started=$SECONDS; row_timed_out=0; row_gate="$1"; row_file="$2"
}
finish_row() {
  [ "$row_open" = 1 ] || return 0
  row_open=0
  local secs=$((SECONDS - row_started)) tag=""
  printf '%s|%s|%s\n' "$secs" "$row_gate" "$row_file" >>"$timing_file"
  if [ "$row_timed_out" = 1 ]; then tag=TIMEOUT
  elif [ "$secs" -ge "$slow_row" ]; then tag=slow
  fi
  if [ -n "$tag" ]; then
    printf 'gate-mutation: row %s/%s %s file=%s %ss [%s]\n' \
      "$row_index" "$row_total" "$row_gate" "$row_file" "$secs" "$tag"
  fi
}
print_timing_summary() {
  [ "$timing_started" = 1 ] && [ "$timing_printed" = 0 ] || return 0
  timing_printed=1
  finish_row
  echo "gate-mutation: slowest rows"
  if [ -s "$timing_file" ]; then
    LC_ALL=C sort -t'|' -k1,1nr "$timing_file" | head -10 |
      while IFS='|' read -r t_secs t_gate t_file; do
        printf '  %6ss  %s  file=%s\n' "$t_secs" "$t_gate" "$t_file"
      done
  fi
  printf 'gate-mutation: total wall %ss, warm-up %ss, not run %s\n' "$SECONDS" "$warmup_secs" "${not_run:-0}"
}

# A row is inject -> run -> restore, and until this trap existed the restore
# only happened on the paths that reach it.  Kill the sweep between the
# `sed -i' and the restore and the injected defect stays in the working tree,
# where the next thing to read that file believes it.  That has happened
# repeatedly: an accept4 syscall number left at 289, a network process left
# reporting `closed' instead of `open' -- the latter surviving long enough to
# be mistaken for a regression in an unrelated change and costing half an hour
# to attribute.  One of those leftovers was staged, so `git diff' did not show
# it either.
#
# The row's backup is a `mktemp' file that outlives a kill, so its existence
# is the signal: if it is still there when the shell leaves, the row did not
# reach its own restore and this puts the file back.  No restore site needs to
# cooperate -- they each `rm -f' the backup, which makes this a no-op.
mutation_active_file=""
mutation_active_backup=""
mutation_restore() {
  if cp "$mutation_active_backup" "$mutation_active_file" 2>/dev/null &&
     cmp -s "$mutation_active_backup" "$mutation_active_file"; then
    return 0
  fi
  printf 'gate-mutation: COULD NOT RESTORE %s -- original retained at %s; stopping before another mutation\n' \
    "$mutation_active_file" "$mutation_active_backup" >&2
  return 1
}

mutation_cleanup() {
  # An interrupt can arrive while a bounded gate or build is still running.
  if [ -n "${bounded_pid:-}" ]; then
    kill -s KILL -- "-$bounded_pid" 2>/dev/null || true
    bounded_pid=""
  fi
  if [ -n "${mutation_active_backup:-}" ] && [ -f "$mutation_active_backup" ]; then
    if mutation_restore; then
      printf 'gate-mutation: restored %s (left mid-row)\n' "$mutation_active_file" >&2
      rm -f "$mutation_active_backup"
    fi
  fi
  mutation_active_file=""; mutation_active_backup=""
  # Scratch unit caches and the private vendor-bytecode swap go too.
  unit_scratch_drop
  rm -rf target/gate-mutation-units
  # An injected build or a variant gate may have left the default build set
  # changed.  Copy the baseline back (plain cp, no status run); do it only when
  # a row actually touched the set, so a clean finish costs nothing.
  if [ "${binary_may_be_dirty:-0}" = 1 ] && [ "${baseline_ready:-0}" = 1 ] && [ -d "$baseline_dir" ]; then
    baseline_restore && binary_may_be_dirty=0
  fi
  print_timing_summary
  rm -rf "$mutation_gate_dir"
}
trap 'mutation_cleanup' EXIT
trap 'mutation_cleanup; exit 130' INT
trap 'mutation_cleanup; exit 143' TERM
trap 'mutation_cleanup; exit 129' HUP

# Run the gate for the current row, on whatever content `$file' currently
# holds, capturing its combined output to `$2'.  `ert-full' is not a raw
# `make' target -- `nelisp-ai.sh test' owns it, wrapping ERT's own batch
# runner -- so it routes there instead of `make "$gate"'.  The corrupted-
# helper row (source: `src/nelisp-eval.el') is scoped to the one test file
# that calls the helper directly, via `NELISP_GATE_MUTATION_TEST_FILES' --
# see the comment on `test_files()' in `tools/ai/nelisp-ai.sh' for why the
# full suite is too slow to carry here.  The empty-glob row corrupts
# `test_files()' ITSELF (source: `tools/ai/nelisp-ai.sh'), so it must run
# UNSCOPED: scoping would bypass the very line the row exists to test.
run_gate_budget=$row_timeout
run_gate() {
  local g="$1" f="$2" log="$3" generic=1 rc t0=$SECONDS
  local budget=$row_timeout
  # A gate that builds its own variant runs under the build budget.
  if gate_builds_variant "$g" || gate_run_compiles "$g"; then budget=$build_timeout; fi
  run_gate_budget=$budget
  local bounded=(timeout --kill-after=10s "$budget")
  run_gate_timeout=0
  if [ "$g" = "ert-full" ]; then
    if [ "$f" = "tools/ai/nelisp-ai.sh" ]; then
      NELISP_GATE_DIR="$mutation_gate_dir" \
        "${bounded[@]}" tools/ai/nelisp-ai.sh test >"$log" 2>&1 &
      wait_bounded $!
    else
      # Which test file a source-file mutation gets scoped to (speed
      # only -- see the block comment above): `src/nelisp-eval.el' is
      # exercised by `test/nelisp-eval-test.el' (the pre-existing row);
      # docs/design/185-cl-generic-subset.org's mutation rows target
      # `lisp/nelisp-cl-macros.el', exercised by
      # `test/nelisp-cl-generic-test.el'.  Any other file falls back to
      # the original single-file scope, unchanged.
      local scoped_test=test/nelisp-eval-test.el
      case "$f" in
        lisp/nelisp-cl-macros.el) scoped_test=test/nelisp-cl-generic-test.el ;;
        packages/nl-num/src/*.el) scoped_test=packages/nl-num/test/nl-num-test.el ;;
        lisp/nelisp-aot-compiler.el|lisp/nelisp-cc-jit-type-of.el|lisp/nelisp-cc-sexp-clone-into.el|scripts/nelisp-standalone-build.el)
          scoped_test=test/nelisp-doc200-unibyte-repr-test.el ;;
      esac
      # Doc 200 probes the production GNU-31.1 compiler dialect.  Keep all
      # other ERT and make gates on the host, including Emacs parity.
      local scoped_emacs="${EMACS:-emacs}"
      if [ "$scoped_test" = test/nelisp-doc200-unibyte-repr-test.el ]; then
        scoped_emacs="${NELISP_BUILD_EMACS:-$scoped_emacs}"
      fi
      EMACS="$scoped_emacs" NELISP_GATE_DIR="$mutation_gate_dir" \
        NELISP_GATE_MUTATION_TEST_FILES="$scoped_test" \
        "${bounded[@]}" tools/ai/nelisp-ai.sh test >"$log" 2>&1 &
      wait_bounded $!
    fi
  elif [ "$g" = "nelisp-thread-allocating-standalone-smoke" ]; then
    # The no-GC mutation can invalidate another worker's private frame before
    # it reaches the completion increment.  That missed-root manifestation is
    # an intentionally red hang, so bound the mutation run; the clean smoke
    # completes in well under one second on the same binary.
    generic=0
    timeout 30 make "$g" $MUTATION_TARGET_ARG >"$log" 2>&1
  elif [ "$g" = "nl-num-standalone-smoke" ]; then
    # A malformed nl-num implementation can enter an unbounded numeric loop
    # while the mutation is present.  Keep this diagnostic row bounded, but
    # let the caller distinguish the timeout (124) from an ordinary red gate.
    # The clean baseline is run separately before and after the mutation; a
    # timeout alone is never accepted as proof that the gate went red.
    # This harness is non-interactive.  Let timeout manage the subprocess
    # tree and force-kill anything that ignores TERM after five seconds.
    generic=0
    timeout --kill-after=5s "$nl_num_mutation_timeout" make "$g" $MUTATION_TARGET_ARG >"$log" 2>&1
  else
    "${bounded[@]}" make "$g" $MUTATION_TARGET_ARG >"$log" 2>&1 &
    wait_bounded $!
  fi
  rc=$?
  # Only the generic branches use the row budget; the two special rows above
  # keep their own timeouts and their own classification.
  if [ "$generic" = 1 ] && { [ "$rc" -eq 124 ] || [ "$rc" -eq 137 ]; } &&
     [ $((SECONDS - t0)) -ge "$budget" ]; then
    run_gate_timeout=1
  fi
  return "$rc"
}

rows=$(grep -v '^#' tools/gate-mutations.txt | grep -v '^[[:space:]]*$')

# Rows historically had exactly four fields.  A fifth is now an optional
# standalone target scope; validate the whole table before selection or
# mutation so a typo cannot silently widen/narrow a row, even when a filter
# would otherwise hide that row from this run.
row_number=0
while IFS= read -r row; do
  row_number=$((row_number+1))
  field_count=1
  field_tail=$row
  while [ "${field_tail#*|}" != "$field_tail" ]; do
    field_count=$((field_count+1))
    field_tail=${field_tail#*|}
  done
  case "$field_count" in
    4|5) ;;
    *) echo "gate-mutation: FAIL (row $row_number has $field_count fields; expected GATE|FILE|SED|WHAT[|TARGET])"; exit 1 ;;
  esac
  IFS='|' read -r row_gate row_file row_expr row_what row_scope <<< "$row"
  if [ -z "$row_gate" ] || [ -z "$row_file" ] || [ -z "$row_expr" ] || [ -z "$row_what" ]; then
    echo "gate-mutation: FAIL (row $row_number has an empty required field)"
    exit 1
  fi
  case "${row_scope:-}" in
    ""|linux-x86_64|linux-aarch64|windows-x86_64|windows-aarch64|macos-x86_64|macos-aarch64) ;;
    *) echo "gate-mutation: FAIL (row $row_number has malformed target scope '$row_scope')"; exit 1 ;;
  esac
done <<< "$rows"

# Row selection.  Authoring a row requires proving it is REACHED -- inject it,
# watch the gate go RED, restore, watch it go GREEN -- and doing that against
# the whole file costs a full sweep (several rebuilds) for one new row.  Three
# separate rows have shipped unreachable or non-lethal in this repo's history,
# each discovered by a CI round rather than at authoring time, so make the
# per-row check cheap enough that there is no excuse to skip it:
#
#   NELISP_GATE_MUTATION_ONLY=<gate-name>   only rows for that gate
#   NELISP_GATE_MUTATION_GREP=<substring>   only rows matching anywhere
#
# `make gate-mutation-verify GATE=<name>' is the front door for the first.
only=${NELISP_GATE_MUTATION_ONLY:-}
if [ -n "$only" ]; then
  rows=$(printf '%s\n' "$rows" | awk -F'|' -v g="$only" '$1==g')
  [ -z "$rows" ] && { echo "gate-mutation: FAIL (no row for gate '$only')"; exit 1; }
  echo "gate-mutation: restricted to gate '$only'"
fi
# Changed-only selection.  A full sweep is 2547 seconds -- measured on the
# ubuntu/29.4 lane of run 32931012524, where it was 42.5 of check-tier's
# 43.6 minutes, i.e. essentially the whole tier.  Most of that proves rows
# whose file nobody touched.
#
# `NELISP_GATE_MUTATION_BASE=<ref>' restricts the sweep to rows whose
# mutated file differs from <ref>.  This is a PRE-MERGE signal, not a
# replacement for the sweep: a row can also stop working because the GATE
# moved rather than the mutated file, and no diff of the mutated file will
# show that.  So the harness itself, the row table, the Makefile that
# dispatches the gates, and the tier runner all force the full sweep when
# they change -- and CI still runs the whole thing on the integration
# branch, where the guarantee has to hold.
base=${NELISP_GATE_MUTATION_BASE:-}
if [ -n "$base" ]; then
  if ! changed=$(git diff --name-only "$base" 2>/dev/null); then
    echo "gate-mutation: FAIL (cannot diff against '$base'; refusing to"
    echo "  narrow the sweep on a base I could not read -- that would run"
    echo "  zero rows and report success)"
    exit 1
  fi
  forces_full=0
  for f in $changed; do
    case "$f" in
      tools/nelisp-gate-mutation.sh|tools/gate-mutations.txt|Makefile|tools/ai/nelisp-ai.sh)
        forces_full=1 ;;
    esac
  done
  if [ "$forces_full" = 1 ]; then
    echo "gate-mutation: full sweep (the harness, the row table, or the gate"
    echo "  dispatch changed, so every row's verdict is back in question)"
  else
    rows=$(printf '%s\n' "$rows" | awk -F'|' -v c="$(printf '%s\n' "$changed" | tr '\n' ' ')" '
      { if (index(" " c " ", " " $2 " ")) print }')
    if [ -z "$rows" ]; then
      # Not `checked=0': in this tree that reads as "the gate died".  A
      # reasoned skip is a different outcome and says so, the same way
      # every other gate here reports one.
      echo "GATE-SKIP changed-only against '$base': no mutated file was touched"
      echo "gate-mutation: SKIP (no row's file changed since $base)"
      exit 0
    fi
    echo "gate-mutation: changed-only against '$base' ($(printf '%s\n' "$rows" | wc -l) of $(printf '%s\n' "$(grep -v '^#' tools/gate-mutations.txt | grep -v '^[[:space:]]*$')" | wc -l) rows)"
  fi
fi

pat=${NELISP_GATE_MUTATION_GREP:-}
if [ -n "$pat" ]; then
  rows=$(printf '%s\n' "$rows" | grep -F "$pat")
  [ -z "$rows" ] && { echo "gate-mutation: FAIL (no row matching '$pat')"; exit 1; }
  echo "gate-mutation: restricted to rows matching '$pat'"
fi

[ -z "$rows" ] && { echo "gate-mutation: FAIL (no mutations defined)"; exit 1; }

# Shard selection, for splitting the sweep across parallel CI jobs.
#
#   NELISP_GATE_MUTATION_SHARD=<k>/<n>   run rows where (index mod n) == k
#
# A full sweep is the single longest thing CI does -- 1257 s of the Linux
# lane's 46 minutes on run 32956362867, in one serial stretch.  The rows
# are independent (each injects, checks, and restores before the next
# begins), so they split cleanly.  Modulo rather than contiguous blocks:
# the slow rows (the six that rebuild the binary, the six that run a
# scoped ERT suite) are clustered in the table, and contiguous blocks
# would put them all in one shard.
# Rows selected before sharding (after scope/ONLY/GREP/BASE narrowing); the
# merge checks that every shard saw the same total.
selected_total=$(printf '%s\n' "$rows" | wc -l | tr -d ' ')
shard=${NELISP_GATE_MUTATION_SHARD:-}
shard_k=0; shard_n=1
if [ -n "$shard" ]; then
  shard_k=${shard%%/*}
  shard_n=${shard##*/}
  case "$shard_k/$shard_n" in
    [0-9]*/[0-9]*) ;;
    *) echo "gate-mutation: FAIL (NELISP_GATE_MUTATION_SHARD must be <k>/<n>, got '$shard')"; exit 1 ;;
  esac
  if [ "$shard_n" -lt 1 ] || [ "$shard_k" -ge "$shard_n" ]; then
    echo "gate-mutation: FAIL (shard $shard_k out of range for $shard_n shards)"
    exit 1
  fi
  rows=$(printf '%s\n' "$rows" | awk -v k="$shard_k" -v n="$shard_n" 'NR % n == k { print }')
  if [ -z "$rows" ]; then
    # More shards than rows.  Not a failure, but it must not read as a
    # clean full sweep either.
    echo "GATE-SKIP shard $shard_k of $shard_n has no rows (more shards than rows)"
    echo "gate-mutation: SKIP (empty shard $shard)"
    exit 0
  fi
  echo "gate-mutation: shard $shard_k of $shard_n ($(printf '%s\n' "$rows" | wc -l | tr -d ' ') rows)"
fi
assigned=$(printf '%s\n' "$rows" | wc -l | tr -d ' ')

# Show the selection and stop.  Inspecting which rows a filter picks used
# to mean starting a sweep and killing it, which is how an injected defect
# once got left in the tree -- the harness restores the file at the END of
# a row, so a kill mid-row keeps the mutation.  This exits before any row
# is touched.
if [ -n "${NELISP_GATE_MUTATION_LIST_ONLY:-}" ]; then
  printf '%s\n' "$rows" | awk -F'|' '{scope = (NF == 5 ? " [" $5 "]" : ""); printf "  %-46s %s%s\n", $1, $2, scope}'
  printf 'gate-mutation: %s row(s) selected (list-only; nothing was injected)\n' \
    "$(printf '%s\n' "$rows" | wc -l | tr -d ' ')"
  exit 0
fi


# Warm-up.  Every gate carries the conditional prerequisite `standalone-reader
# when the binary is absent', so on a cold tree the first gate would do the
# cold build inside its own gate budget and time out falsely.  Build once
# here, under the build budget, after every selection filter so an empty
# selection stays cheap.  Failure is a harness failure before any row runs.
in_scope=$(printf '%s\n' "$rows" | awk -F'|' -v t="$MUTATION_TARGET" \
  'NF >= 4 && ($5 == "" || $5 == t) { n++ } END { print n + 0 }')
if [ "$in_scope" -gt 0 ]; then
  if ! command -v timeout >/dev/null 2>&1; then
    echo "gate-mutation: FAIL (timeout(1) is required for the row and build budgets)"
    exit 1
  fi
  warm_start=$SECONDS
  if ! rebuild_checked "warm-up rebuild, no row yet"; then
    if [ "$rebuild_timed_out" = 1 ]; then
      echo "gate-mutation: FAIL (warm-up rebuild: TIMEOUT after ${build_timeout}s; no row was run)"
    else
      echo "gate-mutation: FAIL (warm-up rebuild failed; no row was run)"
    fi
    exit 1
  fi
  warmup_secs=$((SECONDS - warm_start))
  echo "gate-mutation: warm-up rebuild took ${warmup_secs}s"
  if ! baseline_save; then
    echo "gate-mutation: FAIL (cannot save the clean baseline copy under $baseline_dir; no row was run)"
    exit 1
  fi
  echo "gate-mutation: baseline copy saved ($(grep -vc ' absent$' "$baseline_dir/manifest.txt") present, $(grep -c ' absent$' "$baseline_dir/manifest.txt") absent)"
fi
timing_started=1
row_total=$(printf '%s\n' "$rows" | wc -l | tr -d ' ')

passed=0; failed=0; skipped=0
while IFS='|' read -r gate file expr what scope; do
  finish_row
  [ -z "${gate:-}" ] && continue
  row_index=$((row_index+1))
  if [ -n "${scope:-}" ] && [ "$scope" != "$MUTATION_TARGET" ]; then
    echo "  $gate: SKIP (row scope $scope does not include target $MUTATION_TARGET)"
    skipped=$((skipped+1))
    continue
  fi
  if [ -n "$shard_budget" ]; then
    row_worst=$(row_worst_case "$gate" "$file")
    # Per-row check, no latch: an expensive row that does not fit must not
    # stop cheaper rows after it from running.
    if [ $((SECONDS + row_worst)) -gt "$shard_budget" ]; then
      row_left=$((shard_budget - SECONDS))
      [ "$row_left" -lt 0 ] && row_left=0
      echo "  $gate: NOT RUN (estimate ${row_worst}s does not fit the ${row_left}s left of the ${shard_budget}s shard budget; file $file)"
      not_run=$((not_run+1))
      continue
    fi
  fi
  if [ "$binary_dirty" = 1 ]; then
    # An earlier rebuild timed out; do not let this row see a partial binary.
    recover_start=$SECONDS
    if ! rebuild_checked "recovery rebuild before row $gate, file $file"; then
      echo "gate-mutation: FAIL (recovery rebuild after an earlier rebuild timeout did not succeed; stopping, later rows would test a partial binary)"
      exit 1
    fi
    binary_dirty=0
    echo "gate-mutation: recovery rebuild took $((SECONDS - recover_start))s"
  fi
  start_row "$gate" "$file"
  # Restore the clean binary after this row when it rebuilt around its
  # injection, or when its gate builds a variant (which rewrites the fixed-path
  # companions of the default build).
  need_clean_binary=0
  if row_rebuilds "$gate" "$file" || gate_builds_variant "$gate"; then
    need_clean_binary=1; binary_may_be_dirty=1
  fi
  # nl-num is the one mutation row known to have produced a runaway smoke.
  # Establish a clean PASS before touching the source.  This prevents a
  # timeout or other non-zero result from being treated as the baseline when
  # the gate was already broken for an unrelated reason.
  if [ "$gate" = "nl-num-standalone-smoke" ]; then
    # The Makefile invokes the smoke binary with `--load
    # packages/nl-num/test/nl-num-standalone-smoke.el'; that test loads the
    # package source at runtime.  The mutation therefore changes the source
    # seen by this gate directly and does not require a rebuilt target binary.
    baseline_before_log="$(mktemp)"
    run_gate "$gate" "$file" "$baseline_before_log"
    baseline_before_rc=$?
    baseline_before_skip=$(grep -E '^GATE-SKIP ' "$baseline_before_log" | tail -1 | sed 's/^GATE-SKIP //' || true)
    if [ -n "$baseline_before_skip" ]; then
      rm -f "$baseline_before_log"
      echo "  $gate: SKIP (clean baseline is not runnable: $baseline_before_skip)"
      skipped=$((skipped+1))
      continue
    fi
    if [ "$baseline_before_rc" -ne 0 ]; then
      echo "  $gate: HARNESS ERROR (clean baseline before mutation failed rc=$baseline_before_rc; timeout is not a mutation verdict)"
      report_harness_log "clean baseline before mutation output:" "$baseline_before_log"
      rm -f "$baseline_before_log"
      failed=$((failed+1))
      continue
    fi
    rm -f "$baseline_before_log"
  fi
  backup="$(mktemp)"
  cp "$file" "$backup" || { echo "gate-mutation: FAIL (cannot back up $file)"; exit 1; }
  # Arm the interrupt-restore for this row before the file is touched.
  mutation_active_file="$file"; mutation_active_backup="$backup"
  sed -i "$expr" "$file"
  if cmp -s "$file" "$backup"; then
    echo "  $gate: SED MATCHED NOTHING -- the injection is stale ($what)"
    failed=$((failed+1))
    mutation_restore || exit 1
    rm -f "$backup"; continue
  fi
  # A gate that needs the binary must see the mutated source, so rebuild --
  # and the rebuild MUST be checked.  The first run of this harness reported
  # emacs-parity as "stayed green": the rebuild had failed with "Text file
  # busy" (the binary was still held by the previous gate's subprocess), the
  # OLD binary was used, and the gate passed on code that no longer existed.
  # A harness that can be fooled by a stale artifact is measuring nothing --
  # which is the class it exists to catch.
  #
  # standalone-reader-buffer-smoke (Doc 188 P1, 2026-08-23) joins this list
  # for the identical reason: its own Makefile rule's prerequisite is
  # `$(if $(wildcard target/nelisp target/nelisp.exe),,standalone-reader)'
  # -- conditional on the binary's ABSENCE, not on the source being newer,
  # exactly like emacs-parity's own `standalone-reader-test' prerequisite.
  # Confirmed by hand while adding this row: with a leftover target/nelisp
  # already present, `make standalone-reader-buffer-smoke' on freshly
  # mutated source ran the OLD binary and reported PASS.
  # Doc 201's `standalone-reader-winpath-smoke' and `standalone-reader-
  # fileattrs-smoke' (both mutate scripts/nelisp-stdlib-prelude.el) and
  # `standalone-reader-defvar-alloc-smoke'
  # (mutates scripts/nelisp-standalone-build.el) join for the same
  # reason a third time: both files are baked INTO the binary, both
  # gates carry the binary-absence prerequisite, so without a rebuild
  # each would run the old artifact and report PASS on code that is no
  # longer there.
  #
  # `standalone-reader-regexp-lead-filter-smoke' (Doc 201 §5.4) mutates
  # the same lisp/nelisp-stdlib-regexp.el and joins for the same reason.
  #
  # `standalone-reader-splitstring-perf-smoke' joins them and is a FIX,
  # not an addition: its row (added 2026-08-30 with Doc 201 §4 item 1)
  # mutates lisp/nelisp-stdlib-regexp.el, which
  # `nelisp-standalone--reader-prelude-source' reads with
  # `insert-file-contents' at BUILD time and bakes into the binary --
  # the same shape as the three above, and it was not listed here.  That
  # row was recorded in tools/gate-mutations.txt as never having
  # completed a `gate-mutation-verify' run; a stale binary is why it
  # could not have proved anything if it had.
  # Doc 199's `nelisp-thread-standalone-smoke' has the same conditional
  # prerequisite and mutates native-unit source, so it must rebuild both the
  # injected and restored binary for the same reason.  `ert-full' rows that
  # mutate the standalone build script also rebuild: Doc 200's cross-tag key
  # test executes target/nelisp, so a stale fixed binary would make the new
  # outer-tag-gate mutation read as green.
  if row_rebuilds "$gate" "$file" && [ -z "$baseline_version" ]; then
    echo "  $gate: HARNESS ERROR (no readable \"version\" in the baseline target/nelisp.version.json; the injected build cannot be pinned to the baseline version)"
    failed=$((failed+1))
    mutation_restore || exit 1
    rm -f "$backup"
    mutation_active_file=""; mutation_active_backup=""
    continue
  fi
  if row_rebuilds "$gate" "$file"; then
    if ! { unit_scratch_make && rebuild_checked; }; then
      unit_scratch_drop
      if [ "$rebuild_timed_out" = 1 ]; then
        echo "  $gate: TIMEOUT (rebuild with injection after ${build_timeout}s; file $file)"
        row_timed_out=1
      else
        echo "  $gate: HARNESS ERROR (rebuild with the injection failed; a stale binary would have read as PASS)"
      fi
      failed=$((failed+1))
      mutation_restore || exit 1
      rm -f "$backup"
      mutation_active_file=""; mutation_active_backup=""
      restore_clean_binary "after the failed injected build, gate $gate" || binary_dirty=1
      continue
    fi
    unit_scratch_drop
    if [ "$baseline_ready" = 1 ] && ! file_in_key_dirs "$file"; then
      echo "  $gate: injection check skipped ($file is a stamp KEY_FILE; it need not change the build output)"
    elif [ "$baseline_ready" = 1 ] && [ "$(set_sha)" = "$baseline_set_sha" ]; then
      echo "  $gate: HARNESS ERROR (injection did not reach the binary; file $file)"
      failed=$((failed+1))
      mutation_restore || exit 1
      rm -f "$backup"
      mutation_active_file=""; mutation_active_backup=""
      restore_clean_binary "after an injection that did not reach the binary, gate $gate" || binary_dirty=1
      continue
    fi
  elif gate_needs_rebuild "$gate" "$file"; then
    if gate_builds_variant "$gate"; then
      echo "  $gate: no default rebuild (the gate builds and runs its own variant)"
    else
      echo "  $gate: no rebuild ($file is not a build input)"
    fi
  fi
  gate_log="$(mktemp)"
  run_gate "$gate" "$file" "$gate_log"
  gate_rc=$?
  gate_timed_out=$run_gate_timeout
  gate_ok=0; [ "$gate_rc" -eq 0 ] && gate_ok=1
  if [ "$gate" = "nl-num-standalone-smoke" ]; then
    # Restore before judging a timeout.  Then rerun the same clean gate and
    # require PASS, proving both that the source returned byte-for-byte and
    # that the timeout belongs to the injected defect rather than to a broken
    # baseline or a contaminated binary.
    mutation_restore || exit 1
    if ! cmp -s "$file" "$backup"; then
      echo "  $gate: HARNESS ERROR (source was not restored after mutation)"
      failed=$((failed+1))
      rm -f "$backup"
      mutation_active_file=""; mutation_active_backup=""
      rm -f "$gate_log"
      continue
    fi
    baseline_after_log="$(mktemp)"
    run_gate "$gate" "$file" "$baseline_after_log"
    baseline_after_rc=$?
    baseline_after_skip=$(grep -E '^GATE-SKIP ' "$baseline_after_log" | tail -1 | sed 's/^GATE-SKIP //' || true)
    rm -f "$gate_log"
    rm -f "$backup"
    mutation_active_file=""; mutation_active_backup=""
    if [ -n "$baseline_after_skip" ]; then
      rm -f "$baseline_after_log"
      echo "  $gate: HARNESS ERROR (clean baseline became non-runnable after restore: $baseline_after_skip; source restoration was not a clean PASS)"
      failed=$((failed+1))
    elif [ "$baseline_after_rc" -ne 0 ]; then
      echo "  $gate: HARNESS ERROR (clean baseline after restore failed rc=$baseline_after_rc; source restoration was not a clean PASS)"
      report_harness_log "clean baseline after mutation output:" "$baseline_after_log"
      rm -f "$baseline_after_log"
      failed=$((failed+1))
    elif [ "$gate_rc" -eq 0 ]; then
      rm -f "$baseline_after_log"
      echo "  $gate: STAYED GREEN with a real defect in front of it (clean baseline before/after PASS; source restored byte-for-byte; $what)"
      failed=$((failed+1))
    elif [ "$gate_rc" -eq 124 ]; then
      rm -f "$baseline_after_log"
      echo "  $gate: went red by timeout (rc=124 after ${nl_num_mutation_timeout}s; clean baseline before/after PASS; source restored byte-for-byte; $what)"
      passed=$((passed+1))
    else
      rm -f "$baseline_after_log"
      echo "  $gate: went red (rc=$gate_rc; clean baseline before/after PASS; source restored byte-for-byte; $what)"
      passed=$((passed+1))
    fi
    continue
  fi
  # Same precedence `tools/ai/nelisp-ai.sh cmd_gate' and `gate-selfcheck'
  # (tools/nelisp-gate-selfcheck.el) use for these two lines: a reasoned
  # `GATE-SKIP REASON' is checked before anything else, because a gate that
  # explains why it did not run does not also owe a verdict.  Before this,
  # a row whose gate legitimately skips (emacs-parity, outside its pinned
  # Emacs 30.x major) exited 0 with no defect ever exercised, and this
  # harness read that exit code the same as a real pass: "STAYED GREEN with
  # a real defect in front of it" -- a gate that never ran cannot have
  # stayed anything.  Measured via `NELISP_EMACS_PARITY_HOST_VERSION=29.4
  # make gate-mutation' (mocks a host outside 30.x; CI's ubuntu/29.4 lane
  # hits this for real): emacs-parity's row failed the whole harness on a
  # host where the gate cannot run at all, independent of any real defect.
  skip_after=""
  if [ "$gate_timed_out" = 1 ]; then
    # A killed gate says nothing about lethality, and its partial output must
    # not be mined for a GATE-SKIP line.
    report_harness_log "gate output before the timeout:" "$gate_log"
  else
    skip_after=$(grep -E '^GATE-SKIP ' "$gate_log" | tail -1 | sed 's/^GATE-SKIP //' || true)
  fi
  # What the gate counted WITH the injection in place.  A count-versus-baseline
  # gate (emacs-compat) can stay green on one host and go red on another when
  # the injected defect is smaller than the host-dependent slack; the count
  # makes that visible in the CI log instead of leaving it to be guessed.
  mutated_count=$(grep -E '^GATE-COUNT |^emacs-compat: ' "$gate_log" 2>/dev/null | tail -2 | tr '\n' ' ' || true)
  rm -f "$gate_log"
  if [ -n "$skip_after" ]; then
    # A row can only prove anything on a host where its gate actually runs;
    # a skip is "not provable here", not a pass -- UNLESS the injected
    # defect is what caused the skip, in which case the defect hid itself
    # behind a skip path instead of being caught, which is exactly the
    # "STAYED GREEN" failure this harness exists to catch, just via a
    # different exit than a plain 0.  Tell the two apart by asking the SAME
    # question of the clean tree: `gates.expected' documents three outcomes
    # (runnable / the predicate said no / the predicate could not be asked)
    # and only a skip that repeats UNCHANGED on clean code is "said no" --
    # a skip that appears only once the defect lands is the defect itself,
    # dressed as "could not be asked".
    mutation_restore || exit 1
    if [ "$need_clean_binary" = 1 ]; then
      if ! restore_clean_binary "before the clean skip check, gate $gate"; then
        if [ "$rebuild_timed_out" = 1 ]; then
          echo "  $gate: TIMEOUT (rebuild after restore after ${build_timeout}s; file $file)"
          row_timed_out=1; binary_dirty=1
        else
          echo "  $gate: HARNESS ERROR (clean source was restored, but the binary could not be restored before the clean skip check)"
        fi
        failed=$((failed+1))
        rm -f "$backup"
        mutation_active_file=""; mutation_active_backup=""
        continue
      fi
    fi
    baseline_log="$(mktemp)"
    run_gate "$gate" "$file" "$baseline_log"
    baseline_timed_out=$run_gate_timeout
    skip_before=""
    if [ "$baseline_timed_out" = 1 ]; then
      report_harness_log "clean baseline output before the timeout:" "$baseline_log"
    else
      skip_before=$(grep -E '^GATE-SKIP ' "$baseline_log" | tail -1 | sed 's/^GATE-SKIP //' || true)
    fi
    rm -f "$baseline_log"
    if [ "$baseline_timed_out" = 1 ]; then
      echo "  $gate: TIMEOUT (clean-baseline gate run after ${run_gate_budget}s; file $file)"
      row_timed_out=1
      failed=$((failed+1))
    elif [ -n "$skip_before" ]; then
      echo "  $gate: SKIP (gate not runnable on this host: $skip_after)"
      skipped=$((skipped+1))
    else
      echo "  $gate: STAYED GREEN BY SKIPPING once the defect landed -- the clean tree does not skip for the same reason, so the injection itself trips the skip path and hides behind it ($what)"
      failed=$((failed+1))
    fi
    rm -f "$backup"
    continue
  fi
  if [ "$gate_timed_out" = 1 ]; then
    echo "  $gate: TIMEOUT (gate run with injection after ${run_gate_budget}s; file $file)"
    row_timed_out=1
    failed=$((failed+1))
  elif [ "$gate_ok" = 1 ]; then
    echo "  $gate: STAYED GREEN with a real defect in front of it ($what)"
    [ -n "$mutated_count" ] && echo "    $gate: gate count with the injection: $mutated_count"
    failed=$((failed+1))
  else
    echo "  $gate: went red as it should ($what)"
    [ -n "$mutated_count" ] && echo "    $gate: gate count with the injection: $mutated_count"
    passed=$((passed+1))
  fi
  mutation_restore || exit 1
  rm -f "$backup"
  if [ "$need_clean_binary" = 1 ]; then
    if ! restore_clean_binary "after the row, gate $gate"; then
      if [ "$rebuild_timed_out" = 1 ]; then
        echo "  $gate: TIMEOUT (rebuild after restore after ${build_timeout}s; file $file)"
        row_timed_out=1; binary_dirty=1
      else
        echo "  $gate: HARNESS ERROR (clean source was restored, but the binary could not be restored; later rows would otherwise test the injected artifact)"
      fi
      failed=$((failed+1))
      mutation_active_file=""; mutation_active_backup=""
      continue
    fi
  fi
done <<< "$rows"
print_timing_summary
checked=$((passed+failed))
accounted=$((passed+failed+skipped+not_run))
# Row accounting for the shard merge.  `nelisp-ai.sh gate' keeps only the
# checked= and findings= fields of GATE-COUNT, so the counts also go to a
# sidecar file under target/gate-mutation-rows/ (not target/gates/, which
# local `verify' reads in full); the CI upload step ships it with the report
# and only tools/nelisp-merge-gate-reports.sh reads it.
rows_dir=target/gate-mutation-rows
mkdir -p "$rows_dir"
printf '{"schema": "gate-mutation-rows/1", "shard": "%s/%s", "selected_total": %s, "assigned": %s, "passed": %s, "failed": %s, "skipped": %s, "not_run": %s}\n' \
  "$shard_k" "$shard_n" "$selected_total" "$assigned" "$passed" "$failed" "$skipped" "$not_run" \
  >"$rows_dir/gate-mutation-rows-$shard_k.json"
echo "GATE-COUNT checked=$checked findings=$((failed+not_run)) passed=$passed failed=$failed skipped=$skipped not_run=$not_run selected_total=$selected_total assigned=$assigned"
if [ "$accounted" -ne "$assigned" ]; then
  echo "gate-mutation: FAIL (row accounting: passed+failed+skipped+not_run=$accounted but $assigned rows were assigned)"
  exit 1
fi
if [ "$failed" -gt 0 ] || [ "$not_run" -gt 0 ]; then
  echo "gate-mutation: FAIL ($failed failed, $passed passed, $skipped skipped, $not_run not run)"
  exit 1
fi
echo "gate-mutation: PASS ($passed passed, $skipped skipped)"
