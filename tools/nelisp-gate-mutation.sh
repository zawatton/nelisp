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

rebuild_checked() {
  make standalone-reader $MUTATION_TARGET_ARG >/dev/null 2>&1 && return 0
  sleep 2
  make standalone-reader $MUTATION_TARGET_ARG >/dev/null 2>&1
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
    standalone-reader-buffer-smoke) return 0 ;;
    standalone-reader-winpath-smoke) return 0 ;;
    standalone-reader-defvar-alloc-smoke) return 0 ;;
    standalone-reader-fileattrs-smoke) return 0 ;;
    standalone-reader-splitstring-perf-smoke) return 0 ;;
    standalone-reader-regexp-lead-filter-smoke) return 0 ;;
    nelisp-thread-standalone-smoke) return 0 ;;
    nelisp-thread-allocating-standalone-smoke) return 0 ;;
    nelisp-thread-mirror-guard-standalone-smoke) return 0 ;;
    standalone-midform-gc-bounded) return 0 ;;
    standalone-reader-nonblocking-socket-smoke) return 0 ;;
    standalone-reader-tls-smoke) return 0 ;;
  esac
  # Doc 200: an `ert-full' row is only binary-sensitive when it mutates the
  # standalone build script itself.
  [ "$1:$2" = "ert-full:scripts/nelisp-standalone-build.el" ]
}

# `ert-full' rows run a real (deliberately red) `nelisp-ai.sh test', which
# writes a gate report into `NELISP_GATE_DIR' (default `target/gates').
# Left alone that would overwrite the tree's real `ert-full' report with a
# broken one on every `gate-mutation' run.  Scratch it into a throwaway
# directory instead; nothing else reads this one.
mutation_gate_dir="$(mktemp -d)"

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
mutation_cleanup() {
  if [ -n "${mutation_active_backup:-}" ] && [ -f "$mutation_active_backup" ]; then
    if cp "$mutation_active_backup" "$mutation_active_file" 2>/dev/null; then
      printf 'gate-mutation: restored %s (left mid-row)\n' "$mutation_active_file" >&2
    else
      printf 'gate-mutation: COULD NOT RESTORE %s -- the injection is still in the tree; %s holds the original\n' \
        "$mutation_active_file" "$mutation_active_backup" >&2
    fi
    rm -f "$mutation_active_backup"
  fi
  mutation_active_file=""; mutation_active_backup=""
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
run_gate() {
  local g="$1" f="$2" log="$3"
  if [ "$g" = "ert-full" ]; then
    if [ "$f" = "tools/ai/nelisp-ai.sh" ]; then
      NELISP_GATE_DIR="$mutation_gate_dir" \
        tools/ai/nelisp-ai.sh test >"$log" 2>&1
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
      NELISP_GATE_DIR="$mutation_gate_dir" \
        NELISP_GATE_MUTATION_TEST_FILES="$scoped_test" \
        tools/ai/nelisp-ai.sh test >"$log" 2>&1
    fi
  elif [ "$g" = "nelisp-thread-allocating-standalone-smoke" ]; then
    # The no-GC mutation can invalidate another worker's private frame before
    # it reaches the completion increment.  That missed-root manifestation is
    # an intentionally red hang, so bound the mutation run; the clean smoke
    # completes in well under one second on the same binary.
    timeout 30 make "$g" $MUTATION_TARGET_ARG >"$log" 2>&1
  else
    make "$g" $MUTATION_TARGET_ARG >"$log" 2>&1
  fi
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
shard=${NELISP_GATE_MUTATION_SHARD:-}
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


passed=0; failed=0; skipped=0
while IFS='|' read -r gate file expr what scope; do
  [ -z "${gate:-}" ] && continue
  if [ -n "${scope:-}" ] && [ "$scope" != "$MUTATION_TARGET" ]; then
    echo "  $gate: SKIP (row scope $scope does not include target $MUTATION_TARGET)"
    skipped=$((skipped+1))
    continue
  fi
  backup="$(mktemp)"
  cp "$file" "$backup" || { echo "gate-mutation: FAIL (cannot back up $file)"; exit 1; }
  # Arm the interrupt-restore for this row before the file is touched.
  mutation_active_file="$file"; mutation_active_backup="$backup"
  sed -i "$expr" "$file"
  if cmp -s "$file" "$backup"; then
    echo "  $gate: SED MATCHED NOTHING -- the injection is stale ($what)"
    failed=$((failed+1))
    cp "$backup" "$file"; rm -f "$backup"; continue
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
  if gate_needs_rebuild "$gate" "$file"; then
    if ! rebuild_checked; then
      echo "  $gate: HARNESS ERROR (rebuild with the injection failed; a stale binary would have read as PASS)"
      failed=$((failed+1))
      cp "$backup" "$file"; rm -f "$backup"; continue
    fi
  fi
  gate_log="$(mktemp)"
  run_gate "$gate" "$file" "$gate_log"
  gate_rc=$?
  gate_ok=0; [ "$gate_rc" -eq 0 ] && gate_ok=1
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
  skip_after=$(grep -E '^GATE-SKIP ' "$gate_log" | tail -1 | sed 's/^GATE-SKIP //' || true)
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
    cp "$backup" "$file"
    if gate_needs_rebuild "$gate" "$file"; then
      rebuild_checked || true
    fi
    baseline_log="$(mktemp)"
    run_gate "$gate" "$file" "$baseline_log"
    skip_before=$(grep -E '^GATE-SKIP ' "$baseline_log" | tail -1 | sed 's/^GATE-SKIP //' || true)
    rm -f "$baseline_log"
    if [ -n "$skip_before" ]; then
      echo "  $gate: SKIP (gate not runnable on this host: $skip_after)"
      skipped=$((skipped+1))
    else
      echo "  $gate: STAYED GREEN BY SKIPPING once the defect landed -- the clean tree does not skip for the same reason, so the injection itself trips the skip path and hides behind it ($what)"
      failed=$((failed+1))
    fi
    rm -f "$backup"
    continue
  fi
  if [ "$gate_ok" = 1 ]; then
    echo "  $gate: STAYED GREEN with a real defect in front of it ($what)"
    failed=$((failed+1))
  else
    echo "  $gate: went red as it should ($what)"
    passed=$((passed+1))
  fi
  cp "$backup" "$file"; rm -f "$backup"
  if gate_needs_rebuild "$gate" "$file"; then
    rebuild_checked || true
  fi
done <<< "$rows"
checked=$((passed+failed))
echo "GATE-COUNT checked=$checked findings=$failed passed=$passed failed=$failed skipped=$skipped"
if [ "$failed" -gt 0 ]; then
  echo "gate-mutation: FAIL ($failed failed, $passed passed, $skipped skipped)"
  exit 1
fi
echo "gate-mutation: PASS ($passed passed, $skipped skipped)"
