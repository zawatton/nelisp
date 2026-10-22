#!/usr/bin/env bash
# batch-f-check.sh CHECK [ARGS]: the machine checks of tools/ai/batch-f-progress.org.
#
# Binaries: target/integ/nelisp-{dyn,static} (BATCH_F_DYN / BATCH_F_STATIC
# override).  GNU 31.1 is /usr/local/bin/emacs.  Every NeLisp and GNU run is
# capped at 8 GiB of address space.  Exit 0 iff the check passes.
#
#   merged NAME       gnu-semantics | casefold | codegen | bridge | provide
#   integrity         the Unicode payload test (621386274) on host Emacs
#   hygiene           no conflict markers; every Elisp file the batch changed
#                     passes check-parens
#   boot              both linkages evaluate (+ 1 2)
#   built             both linkages exist and name HEAD's tree
#   gnu ITEM          gnu-semantics' fixtures (tools/ai/gnu-semantics-check.sh
#                     ITEM) on both linkages: buffer error shadow variables
#                     load printer syntax
#   casefold          test/standalone-casefold-domain-check: GNU still prints
#                     the 522-row oracle and both linkages print it
#   e-fixtures        batch E's GNU fixtures and the cold-load check
#   closures          batch E's interpreted-closure fixture
#   syntax-rt         batch E's syntax-rt semantics and char-table fixtures
#   bridge-test       the buffer-bridge lane's simulated-library test and its
#                     interpreted save-current-buffer extension
#   fixture NAME LOC  test/fixtures/NAME.el: GNU still prints the pin
#                     (NAME.LOC.gnu-31.1.expected, else NAME.gnu-31.1.expected)
#                     and both linkages print it byte for byte, empty stderr
#                     (batch-f-doc-closure, batch-f-skip-load, ...)
#   all-fixtures      every fixture check above, in one run
#   vm-memory         2.5M bytecode funcalls stay under 1 GiB; E's build does not
#   vm-stress         a collection at every VM poll, conservative scan off: the
#                     GNU bytecode corpus passes; E's build fails the check
#   vm-loop           the codegen lane's 18.3M funcall workload and a 100000-iteration
#                     bytecode with-current-buffer loop complete under the 8 GiB cap;
#                     E's build exits 88 on both
#   codegen-ert       codegen's ERT suites on host Emacs, on the merged tree
#   codegen-align     linked function entries and hot loop targets of the
#                     dynamic build are 16-byte aligned
#   census            native-free-symbol census: no free-symbol site, both
#   casetab           casetab's GNU fixtures (API 43, behaviour 47)
#   ns                ns-inventory: no unpinned collision beyond 1290
#   log KIND          pipeline evidence newer than the dynamic binary:
#                     common mcp standalone sweep fileio aot validate win timing
#   pipeline          every log check
cd "$(dirname "$0")/../.." || exit 2
ROOT=$PWD
DYN=${BATCH_F_DYN:-$ROOT/target/integ/nelisp-dyn}
ST=${BATCH_F_STATIC:-$ROOT/target/integ/nelisp-static}
GNU=/usr/local/bin/emacs
OUT=$ROOT/target/batch-f; mkdir -p "$OUT"
export TMPDIR=$ROOT/target/tmp XDG_CACHE_HOME=$ROOT/target/xdg-cache
mkdir -p "$TMPDIR" "$XDG_CACHE_HOME"

BASE=1aa530d09      # batch E, the parent of this batch
GNU_SEMANTICS_REV=a00f45cdf
CASEFOLD_REV=2c43a09cb1d03c6aa9905363633b7756e15062a5
CODEGEN_REV=5bca59688
E_FIXUPS_REV=784f9c3dd
# The buffer-bridge lane's tip (eval/f-buffer-bridge-20261022), pinned when the
# coordinator hands it over.  Empty means the lane has not delivered.
BRIDGE_REV=6ccf1362a

say() { echo "BATCH-F $*"; }
# The binaries may predate HEAD only by commits to this ledger's own files.
built_from_head_tree() {  # built_from_head_tree REV
  git diff --quiet "$1" HEAD -- . ':(exclude)tools/ai/batch-f-check.sh' \
    ':(exclude)tools/ai/batch-f-progress.org'
}
fail() { say "FAIL: $*"; exit 1; }
capped() { (ulimit -v 8388608; exec "$@"); }

# run_nelisp BIN FIXTURE LOCALE OUTFILE ERRFILE -> exit status
run_nelisp() {
  capped env LC_ALL="$3" timeout 300 "$1" -l "$2" > "$4" 2> "$5"
}
run_gnu() {
  capped env LC_ALL="$2" LANG= LANGUAGE= timeout 300 "$GNU" -Q --batch -l "$1" > "$3" 2> /dev/null
}

expected_for() {
  local name=$1 loc=$2
  if [ -f "test/fixtures/$name.$loc.gnu-31.1.expected" ]; then
    echo "test/fixtures/$name.$loc.gnu-31.1.expected"
  else
    echo "test/fixtures/$name.gnu-31.1.expected"
  fi
}

check_fixture() {
  local name=$1 loc=${2:-C.UTF-8} fx="test/fixtures/$1.el" want
  want=$(expected_for "$name" "$loc")
  [ -f "$fx" ] && [ -f "$want" ] || fail "missing $fx or $want"
  run_gnu "$fx" "$loc" "$OUT/$name.$loc.gnu.out" || fail "$name ($loc): GNU exited $?"
  cmp -s "$want" "$OUT/$name.$loc.gnu.out" || fail "$name ($loc): GNU no longer prints the pin"
  local tag bin rc
  for tag in dyn static; do
    bin=$DYN; [ $tag = static ] && bin=$ST
    [ -x "$bin" ] || fail "missing $tag binary $bin"
    run_nelisp "$bin" "$fx" "$loc" "$OUT/$name.$loc.$tag.out" "$OUT/$name.$loc.$tag.err"; rc=$?
    [ $rc = 0 ] || fail "$name ($loc) $tag: exit $rc"
    [ -s "$OUT/$name.$loc.$tag.err" ] && fail "$name ($loc) $tag: stderr not empty"
    cmp -s "$want" "$OUT/$name.$loc.$tag.out" ||
      fail "$name ($loc) $tag: differs from GNU ($(diff "$want" "$OUT/$name.$loc.$tag.out" | grep -c '^[<>]') lines)"
  done
  say "PASS fixture $name ($loc): GNU pin reproduced; dynamic and static identical"
}

eval_both() {   # eval_both FORM EXPECTED-STDOUT
  local tag bin got
  for tag in dyn static; do
    bin=$DYN; [ $tag = static ] && bin=$ST
    [ -x "$bin" ] || fail "missing $tag binary $bin"
    printf '%s\n' "$1" > "$OUT/eval.el"
    got=$(capped timeout 120 "$bin" -l "$OUT/eval.el" 2>/dev/null)
    [ "$got" = "$2" ] || fail "$tag: $1 printed '$got', want '$2'"
  done
}

# standalone_file BIN FILE TAG: test/perf/standalone-checks.sh on one check
# file, logs apart from the full corpus run so the two never collide.
standalone_file() {
  local bin=$1 file=$2 tag=$3
  capped env EVAL_BIN="$bin" STANDALONE_CHECK_LOGDIR="$OUT/standalone-$tag" \
    sh test/perf/standalone-checks.sh "$file" > "$OUT/standalone-$tag.log" 2>&1
}

log_check() {
  local kind=$1 f
  [ -x "$DYN" ] || fail "missing dynamic binary"
  case $kind in
    common) f=target/final-verify/integF-summary.log
            grep -q 'GATE-COUNT: 11 cases, 0 failures' "$f" 2>/dev/null || fail "COMMON not 11/0 in $f" ;;
    standalone) f=target/final-verify/checks-integF/standalone-checks.log
            grep -qx 'STANDALONE-CHECKS PASS' "$f" 2>/dev/null || fail "test/perf/standalone-checks.sh not PASS in $f" ;;
    mcp)    f=target/final-verify/integF-mcp.log
            sed -n 2p "$f" 2>/dev/null | grep -qx '2 tools=40' || fail "MCP not 40 tools in $f" ;;
    sweep)  f=target/integ/sweep-F.txt
            [ "$(grep -c 'SIGSEGV=0/' "$f" 2>/dev/null)" = 4 ] && ! grep -q 'other=[^ ]' "$f" ||
              fail "sweep not clean in $f" ;;
    fileio) f=target/integ/focused/summary.log
            [ "$(grep -c '^FILEIO2 .* PASS' "$f" 2>/dev/null)" = 2 ] || fail "fileio-2 not PASS on both in $f" ;;
    aot)    f=target/integ/focused/summary.log
            # The differential generates its cases from scripts/nelisp-aot-differential-cases.el:
            # 97 before codegen's Doc 229 programs, 211 on the merged tree (all compile natively).
            [ "$(grep -c '^AOT-DIFF .* cases=\(21[1-9]\|2[2-9][0-9]\|[3-9][0-9][0-9]\) wrong-chunks=0 crashes=0 PASS' "$f" 2>/dev/null)" = 2 ] ||
              fail "AOT differential not >=211/211 on both in $f" ;;
    validate) f=target/integ/validate-F.log
            grep -q '^VALIDATE F-dyn: 0 failing groups' "$f" 2>/dev/null || fail "codegen validate.sh not 0 failing groups in $f" ;;
    win)    f=target/win/F/smoke.log
            c=$(cat target/win/F/commit 2>/dev/null)
            [ -n "$c" ] && built_from_head_tree "$c" || fail "Windows build ${c:0:9} is not HEAD's tree"
            tail -1 "$f" 2>/dev/null | grep -q '^WIN-SMOKE PASS' || fail "Windows smoke not PASS in $f" ;;
    timing) f=target/integ/timing-F/report.txt
            grep -q 'ratio median' "$f" 2>/dev/null || fail "no timing report $f"
            # Every row above 1.03 needs a recorded cause: "PROBE :: KEY-PREFIX :: CAUSE".
            python3 - "$f" target/integ/timing-F/causes.txt <<'PY' || fail "timing rows above 1.03 without a cause in target/integ/timing-F/causes.txt"
import re, sys
report, causes = sys.argv[1:]
try:
    cause_lines = [l.split(' :: ') for l in open(causes, encoding='utf-8').read().splitlines() if ' :: ' in l]
except OSError:
    cause_lines = []
probe, missing = None, []
for line in open(report, encoding='utf-8'):
    if line.startswith('== '):
        probe = line[3:].strip()
        continue
    m = re.match(r'\s*(.*?): ratio median ([0-9.]+) ', line)
    if m and float(m.group(2)) > 1.03:
        key = m.group(1).strip().strip('"')
        if not any(c[0].strip() == probe and key.startswith(c[1].strip()) and len(c) > 2 and c[2].strip()
                   for c in cause_lines):
            missing.append(f'{probe}: {key} ({m.group(2)})')
for row in missing:
    print('NO CAUSE:', row)
sys.exit(1 if missing else 0)
PY
            ;;
    timing-vm) f=target/integ/timing-F/report.txt
            python3 - "$f" <<'PY' || fail "bytecode funcall or while loop above 1.03 (or missing) in $f"
import re, sys
want = {'bc-funcall.el': None, 'bc-while.el': None}
probe = None
for line in open(sys.argv[1], encoding='utf-8'):
    if line.startswith('== '):
        probe = line[3:].strip(); continue
    m = re.match(r'\s*(S#-WORK .*?): ratio median ([0-9.]+) ', line)
    if m and probe in want and want[probe] is None:
        want[probe] = float(m.group(2))
for name, ratio in want.items():
    print(name, ratio)
sys.exit(0 if all(r is not None and r <= 1.03 for r in want.values()) else 1)
PY
            ;;
    *) fail "unknown log kind $kind" ;;
  esac
  [ "$kind" = win ] || [ "$f" -nt "$DYN" ] || fail "$f is older than the dynamic binary"
  say "PASS log $kind ($f)"
}

case ${1:-} in
  merged)
    case ${2:-} in
      gnu-semantics)
        git merge-base --is-ancestor $GNU_SEMANTICS_REV HEAD || fail "eval/gnu-semantics-20261022 $GNU_SEMANTICS_REV not merged"
        [ -f tools/ai/gnu-semantics-check.sh ] || fail "gnu-semantics' check script missing"
        say "PASS merged gnu-semantics ($GNU_SEMANTICS_REV)" ;;
      casefold)
        git log --format=%B "$BASE"..HEAD | grep -qF "(cherry picked from commit $CASEFOLD_REV)" ||
          fail "2c43a09cb not cherry-picked with -x"
        [ "$(grep -c '^(defun nlre--current-translate-table' lisp/nelisp-stdlib-regexp.el)" = 1 ] ||
          fail "not exactly one canon read"
        grep -q 'nelisp-casetab--builtin-canon' lisp/nelisp-stdlib-regexp.el || fail "E's canon shortcut missing"
        grep -q 'nlre--unibyte-fold-agrees-p' lisp/nelisp-stdlib-regexp.el || fail "the wider domain proof is missing"
        ! grep -q 'search-canon' lisp/nelisp-stdlib-regexp.el scripts/nelisp-stdlib-prelude.el \
            packages/nelisp-emacs-foundation/src/emacs-cc-casetab-1.el || fail "a second canon shortcut remains"
        [ -f test/standalone-casefold-domain-check.el ] && [ -f test/standalone-casefold-domain-check.expected ] ||
          fail "casefold-domain fixture missing"
        say "PASS merged casefold (one canon shortcut, wider domain proof)" ;;
      codegen)
        git merge-base --is-ancestor $CODEGEN_REV HEAD || fail "eval/codegen-20261021 $CODEGEN_REV not merged"
        git merge-base --is-ancestor $E_FIXUPS_REV HEAD || fail "E's asm fixups $E_FIXUPS_REV missing"
        say "PASS merged codegen ($CODEGEN_REV; E's asm fixups kept)" ;;
      bridge)
        [ -n "$BRIDGE_REV" ] || fail "the buffer-bridge lane has not delivered (no pinned hash in this script)"
        git merge-base --is-ancestor "$BRIDGE_REV" HEAD || fail "bridge lane $BRIDGE_REV not merged"
        say "PASS merged bridge ($BRIDGE_REV)" ;;
      provide)
        grep -q "(provide 'nelisp-elc-reader)" scripts/nelisp-stdlib-prelude.el || fail "nelisp-elc-reader not provided"
        c=$(git log -1 --format=%H -S"(provide 'nelisp-elc-reader)" -- scripts/nelisp-stdlib-prelude.el)
        [ -n "$c" ] || fail "no commit introduces the provide"
        git merge-base --is-ancestor "$c" HEAD || fail "provide commit not in HEAD"
        git diff --quiet "$c" HEAD -- . ':(exclude)tools/ai/batch-f-check.sh' ':(exclude)tools/ai/batch-f-progress.org' ||
          fail "sources changed after the provide commit ${c:0:9}: it must be the last source commit"
        say "PASS merged provide (last source commit ${c:0:9})" ;;
      *) fail "usage: merged gnu-semantics|casefold|codegen|bridge|provide" ;;
    esac ;;
  integrity)
    capped timeout 300 "$GNU" -Q --batch -L lisp -L src -L test -l test/nelisp-regex-replacement-test.el \
      --eval '(ert-run-tests-batch-and-exit "unicode-data-is-intact")' > "$OUT/integrity.log" 2>&1 ||
      fail "unicode-data-is-intact"
    say "PASS integrity" ;;
  hygiene)
    bad=$(git grep -lE '^(<<<<<<<|>>>>>>>) ' HEAD -- lisp scripts src test tools packages ':!*.org' ':!*.md' | head -3)
    [ -z "$bad" ] || fail "conflict markers in: $bad"
    files=$(git diff --name-only --diff-filter=AM "$BASE" HEAD -- '*.el')
    [ -n "$files" ] || fail "no Elisp files changed since $BASE"
    FILES=$files capped timeout 300 "$GNU" -Q --batch --eval '
      (let (bad (n 0))
        (dolist (f (split-string (getenv "FILES") "\n" t))
          (setq n (1+ n))
          (with-temp-buffer
            (insert-file-contents f)
            (emacs-lisp-mode)
            (condition-case err (check-parens)
              (error (push (format "%s: %s" f (error-message-string err)) bad)))))
        (princ (format "checked %d files, %d unbalanced\n" n (length bad)))
        (dolist (b bad) (princ (concat b "\n")))
        (kill-emacs (if bad 1 0)))' > "$OUT/hygiene.log" 2>&1 || fail "check-parens: $(head -3 "$OUT/hygiene.log" | tr '\n' ' ')"
    say "PASS hygiene ($(head -1 "$OUT/hygiene.log"))" ;;
  boot)
    eval_both "(princ (+ 1 2))" 3
    say "PASS boot" ;;
  built)
    for tag in dyn static; do
      bin=$DYN; [ $tag = static ] && bin=$ST
      v=$(capped "$bin" --version 2>/dev/null | head -1)
      rev=$(printf '%s\n' "$v" | sed -n 's/.*-g\([0-9a-f]\{9\}\) (.*/\1/p')
      [ -n "$rev" ] && built_from_head_tree "$rev" || fail "$tag: '$v' is not built from HEAD's tree"
    done
    say "PASS built ($rev, HEAD's tree)" ;;
  gnu)
    item=${2:?gnu-semantics item}
    GNU_SEMANTICS_DYN=$DYN GNU_SEMANTICS_STATIC=$ST EMACS=$GNU bash tools/ai/gnu-semantics-check.sh "$item" \
      > "$OUT/gnu-$item.log" 2>&1 || fail "gnu-semantics $item ($(tail -n 1 "$OUT/gnu-$item.log" | cut -c1-160))"
    say "PASS gnu-semantics $item ($(tail -n 1 "$OUT/gnu-$item.log"))" ;;
  casefold)
    fx=test/standalone-casefold-domain-check
    [ -f $fx.el ] && [ -f $fx.expected ] || fail "casefold-domain fixture missing"
    [ "$(wc -l < $fx.expected)" -ge 523 ] || fail "oracle has fewer than 522 rows and the marker"
    capped timeout 300 "$GNU" -Q --batch -l $fx.el > "$OUT/casefold.gnu.out" 2> /dev/null
    cmp -s $fx.expected "$OUT/casefold.gnu.out" || fail "GNU no longer prints the oracle"
    for tag in dyn static; do
      bin=$DYN; [ $tag = static ] && bin=$ST
      [ -x "$bin" ] || fail "missing $tag binary"
      standalone_file "$bin" $fx.el casefold-$tag || fail "casefold-domain $tag: $(grep -h 'STANDALONE-CHECK' "$OUT/standalone-casefold-$tag.log" | head -1)"
    done
    say "PASS casefold (GNU oracle, $(( $(wc -l < $fx.expected) - 1 )) rows, both linkages)" ;;
  e-fixtures)
    for spec in batch-e-syntax-buffer:C.UTF-8 batch-e-category-table:C.UTF-8 batch-e-eval-special:C.UTF-8 \
                batch-e-quote-style:C batch-e-quote-style:C.UTF-8; do
      ( check_fixture "${spec%%:*}" "${spec#*:}" ) || exit 1
    done
    for tag in dyn static; do
      bin=$DYN; [ $tag = static ] && bin=$ST
      [ -x "$bin" ] || fail "missing $tag binary"
      capped timeout 60 "$bin" --cold-load-from /nonexistent/batch-f.cold --eval '(princ "ran")' \
        > "$OUT/coldload.$tag.out" 2> "$OUT/coldload.$tag.err"; rc=$?
      [ $rc = 1 ] || fail "cold-load $tag: exit $rc, want 1"
      [ -s "$OUT/coldload.$tag.out" ] && fail "cold-load $tag: evaluated past the missing image"
      grep -q 'could not load' "$OUT/coldload.$tag.err" || fail "cold-load $tag: no fatal message"
    done
    say "PASS e-fixtures (batch E's fixtures and cold-load)" ;;
  closures)
    [ -x "$DYN" ] && [ -x "$ST" ] || fail "missing binaries"
    mkdir -p target/elc-interpreted
    python3 - "$ROOT" "$DYN" "$ST" > target/elc-interpreted/build.json <<'EOF' || fail "manifest"
import hashlib, json, sys
root, dyn, st = sys.argv[1:]
sha = lambda p: hashlib.sha256(open(p, 'rb').read()).hexdigest()
print(json.dumps({'source': root, 'binaries': {'dynamic': dyn, 'static': st},
                  'inputs': {dyn: sha(dyn), st: sha(st)}}, indent=2))
EOF
    python3 tools/ai/elc-interpreted-check.py fixture > "$OUT/closures.log" 2>&1 || fail "closure fixture ($(tail -1 "$OUT/closures.log"))"
    say "PASS closures ($(tail -1 "$OUT/closures.log"))" ;;
  syntax-rt)
    mkdir -p target/syntax-rt   # syntax-rt-check.py writes its evidence there
    for m in semantics chartab; do
      SYNTAX_RT_DYN=$DYN SYNTAX_RT_STATIC=$ST SYNTAX_RT_CPU=0-7 python3 tools/ai/syntax-rt-check.py $m \
        > "$OUT/syntax-rt-$m.log" 2>&1 || fail "syntax-rt $m ($(tail -1 "$OUT/syntax-rt-$m.log"))"
    done
    say "PASS syntax-rt semantics and chartab" ;;
  bridge-test)
    [ -n "$BRIDGE_REV" ] || fail "the buffer-bridge lane has not delivered (no pinned hash in this script)"
    lane=test/standalone-buffer-function-cell-check   # the bridge lane's simulated library
    ext=test/standalone-save-current-buffer-cell-check # the same cases through the runtime's own save-current-buffer
    for f in $lane.el $lane.expected $ext.el $ext.expected test/fixtures/buffer-function-cell.el \
             test/fixtures/buffer-function-cell.eld; do
      [ -f "$f" ] || fail "missing $f"
    done
    for t in $lane $ext; do
      # Under GNU 31.1 nothing is recorded (its special forms bypass the function cells): both
      # sides print the same results, which is the pin.
      capped timeout 300 "$GNU" -Q --batch --load $t.el > "$OUT/bridge-${t##*/}.gnu.out" 2> /dev/null
      cmp -s $t.expected "$OUT/bridge-${t##*/}.gnu.out" || fail "GNU no longer prints $t.expected"
      for tag in dyn static; do
        bin=$DYN; [ $tag = static ] && bin=$ST
        [ -x "$bin" ] || fail "missing $tag binary"
        standalone_file "$bin" $t.el bridge-${t##*/}-$tag ||
          fail "$t on $tag: $(grep -h 'STANDALONE-CHECK' "$OUT/standalone-bridge-${t##*/}-$tag.log" | head -1 | cut -c1-200)"
      done
    done
    # Negative control: batch E's build (no bridge) must fail both, or the tests prove nothing.
    base=$ROOT/target/integ/base/nelisp-dyn
    [ -x "$base" ] || fail "negative control needs batch E's build at $base"
    for t in $lane $ext; do
      if standalone_file "$base" $t.el bridge-${t##*/}-base; then fail "negative control: batch E's build passes $t"; fi
    done
    say "PASS bridge-test (both linkages; batch E's build fails both)" ;;
  all-fixtures)
    for item in buffer error shadow variables load printer syntax; do
      ( "$0" gnu $item ) || exit 1
    done
    for k in casefold e-fixtures closures syntax-rt; do ( "$0" $k ) || exit 1; done
    for f in batch-f-doc-closure batch-f-skip-load; do ( "$0" fixture $f C.UTF-8 ) || exit 1; done
    ( "$0" bridge-test ) || exit 1
    say "PASS all fixtures" ;;
  fixture) check_fixture "${2:?fixture name}" "${3:-C.UTF-8}" ;;
  vm-memory)
    t=test/standalone-vm-gc-poll-memory-check
    [ -f $t.el ] && [ -f $t.expected ] || fail "missing $t"
    for tag in dyn static; do
      bin=$DYN; [ $tag = static ] && bin=$ST
      [ -x "$bin" ] || fail "missing $tag binary"
      capped timeout 600 "$bin" --load $t.el > "$OUT/vm-memory.$tag.out" 2> "$OUT/vm-memory.$tag.err"; rc=$?
      if [ $rc != 0 ] || [ -s "$OUT/vm-memory.$tag.err" ] || ! cmp -s $t.expected "$OUT/vm-memory.$tag.out"; then
        fail "$t on $tag: rc=$rc $(head -c 160 "$OUT/vm-memory.$tag.out" | tr '\n' ' ') $(head -c 100 "$OUT/vm-memory.$tag.err" | tr '\n' ' ')"
      fi
    done
    # Negative control: batch E's build (no poll in the VM) must not pass.
    base=$ROOT/target/integ/base/nelisp-dyn
    [ -x "$base" ] || fail "negative control needs batch E's build at $base"
    capped timeout 600 "$base" --load $t.el > "$OUT/vm-memory.base.out" 2> "$OUT/vm-memory.base.err"; rc=$?
    if [ $rc = 0 ] && cmp -s $t.expected "$OUT/vm-memory.base.out"; then fail "negative control: batch E's build passes $t"; fi
    say "PASS vm-memory (both linkages; batch E's build: rc=$rc, $(head -c 80 "$OUT/vm-memory.base.out" | tr '\n' ' '))" ;;
  vm-stress)
    # A full collection at every poll takes about five minutes a linkage: the two run side by side.
    t=test/vm-gc-poll-stress
    [ -f $t.el ] && [ -f $t.expected ] || fail "missing $t"
    for tag in dyn static; do
      bin=$DYN; [ $tag = static ] && bin=$ST
      [ -x "$bin" ] || fail "missing $tag binary"
      ( capped timeout 580 "$bin" --load $t.el > "$OUT/vm-stress.$tag.out" 2> "$OUT/vm-stress.$tag.err"; echo $? > "$OUT/vm-stress.$tag.rc" ) &
    done
    wait
    for tag in dyn static; do
      rc=$(cat "$OUT/vm-stress.$tag.rc" 2>/dev/null)
      if [ "$rc" != 0 ] || [ -s "$OUT/vm-stress.$tag.err" ] || ! cmp -s $t.expected "$OUT/vm-stress.$tag.out"; then
        fail "$t on $tag: rc=$rc $(head -c 200 "$OUT/vm-stress.$tag.out" | tr '\n' ' ') $(head -c 100 "$OUT/vm-stress.$tag.err" | tr '\n' ' ')"
      fi
    done
    # Negative control: batch E's build has no hook and no poll, so no collection runs and the check fails.
    base=$ROOT/target/integ/base/nelisp-dyn
    [ -x "$base" ] || fail "negative control needs batch E's build at $base"
    capped timeout 580 "$base" --load $t.el > "$OUT/vm-stress.base.out" 2> "$OUT/vm-stress.base.err"; rc=$?
    if [ $rc = 0 ] && cmp -s $t.expected "$OUT/vm-stress.base.out"; then fail "negative control: batch E's build passes $t"; fi
    say "PASS vm-stress (both linkages; batch E's build: rc=$rc, $(tail -n 1 "$OUT/vm-stress.base.out" | head -c 90))" ;;
  vm-loop)
    # The two end-to-end loops, each run on both linkages side by side; batch E's build exits 88.
    for name in batch-f-vm-funcall-18m batch-f-vm-buffer-loop; do
      fx=test/fixtures/$name
      [ -f $fx.el ] && [ -f $fx.gnu-31.1.expected ] || fail "missing $fx"
      run_gnu $fx.el C.UTF-8 "$OUT/$name.gnu.out" || fail "GNU on $name: $?"
      cmp -s $fx.gnu-31.1.expected "$OUT/$name.gnu.out" || fail "GNU no longer prints the pin of $name"
    done
    for tag in dyn static; do
      bin=$DYN; [ $tag = static ] && bin=$ST
      [ -x "$bin" ] || fail "missing $tag binary"
      rm -f "$OUT/vm-loop.$tag.rc"
      ( for name in batch-f-vm-funcall-18m batch-f-vm-buffer-loop; do
          run_nelisp "$bin" test/fixtures/$name.el C.UTF-8 "$OUT/$name.$tag.out" "$OUT/$name.$tag.err"
          echo "$name $? " >> "$OUT/vm-loop.$tag.rc"
        done ) &
    done
    wait
    for tag in dyn static; do
      for name in batch-f-vm-funcall-18m batch-f-vm-buffer-loop; do
        grep -q "^$name 0 " "$OUT/vm-loop.$tag.rc" 2>/dev/null &&
          cmp -s test/fixtures/$name.gnu-31.1.expected "$OUT/$name.$tag.out" ||
          fail "$name on $tag: $(grep "^$name " "$OUT/vm-loop.$tag.rc" 2>/dev/null) $(head -c 100 "$OUT/$name.$tag.err" | tr '\n' ' ')"
      done
    done
    base=$ROOT/target/integ/base/nelisp-dyn
    [ -x "$base" ] || fail "negative control needs batch E's build at $base"
    for name in batch-f-vm-funcall-18m batch-f-vm-buffer-loop; do
      run_nelisp "$base" test/fixtures/$name.el C.UTF-8 "$OUT/$name.base.out" "$OUT/$name.base.err"; rc=$?
      [ $rc = 88 ] || fail "negative control: batch E's build exited $rc on $name, want 88 (the 8 GiB cap)"
    done
    say "PASS vm-loop (both linkages complete both loops; batch E's build exits 88 on both)" ;;
  codegen-ert)
    for t in nelisp-aot-compiler-test nelisp-aot-s5-lowering-test nelisp-codegen-alignment-test \
             nelisp-artifact-native-exec-test nelisp-artifact-test nelisp-static-linker-test; do
      [ -f test/$t.el ] || fail "missing test/$t.el"
      capped timeout 900 "$GNU" --batch -Q -L lisp -L src -L scripts -L test --eval '(setq load-prefer-newer t)' \
        -l ert -l test/$t.el -f ert-run-tests-batch-and-exit > "$OUT/ert-$t.log" 2>&1 ||
        fail "$t: $(grep -E '^(Ran|   FAILED)' "$OUT/ert-$t.log" | head -3 | tr '\n' ' ')"
    done
    say "PASS codegen-ert" ;;
  codegen-align)
    [ -x "$DYN" ] || fail "missing dynamic binary"
    python3 - "$DYN" > "$OUT/codegen-align.log" 2>&1 <<'PY' || fail "linked alignment ($(tail -1 "$OUT/codegen-align.log"))"
import importlib.util, json, sys
spec = importlib.util.spec_from_file_location('codegen_alignment', 'tools/ai/codegen-alignment.py')
module = importlib.util.module_from_spec(spec); spec.loader.exec_module(module)
counts = module.inspect(sys.argv[1])
print(json.dumps(counts, sort_keys=True))
ok = counts['functions'] > 0 and counts['unaligned_functions'] == 0 and \
    counts['hot_backward_targets'] > 0 and counts['unaligned_hot_targets'] == 0
sys.exit(0 if ok else 1)
PY
    say "PASS codegen-align ($(tail -n 1 "$OUT/codegen-align.log"))" ;;
  census)
    for tag in dyn static; do
      bin=$DYN; [ $tag = static ] && bin=$ST
      [ -x "$bin" ] || fail "missing $tag binary"
      python3 tools/ai/native-free-symbol-census.py check "$bin" --max 0 > "$OUT/census-$tag.log" 2>&1 ||
        fail "free-symbol census $tag: $(tail -n 1 "$OUT/census-$tag.log" | cut -c1-160)"
    done
    say "PASS census (no free-symbol site, both linkages)" ;;
  casetab)
    for m in api behaviour; do
      CASETAB_DYNAMIC_BIN=$DYN CASETAB_STATIC_BIN=$ST CASETAB_EVIDENCE_DIR=$OUT/casetab CASETAB_TIMEOUT=300 \
        python3 tools/ai/casetab-check.py $m > "$OUT/casetab-$m.log" 2>&1 || fail "casetab $m"
    done
    say "PASS casetab api and behaviour" ;;
  ns)
    capped timeout 300 "$GNU" --batch -Q -L packages/nl-prelude/src -L packages/nl-ns/src \
      -l tools/nl-ns-inventory.el > "$OUT/ns.log" 2>&1
    n=$(grep -o 'FAIL ([0-9]* collision' "$OUT/ns.log" | tr -dc 0-9)
    [ -n "$n" ] && [ "$n" -le 1290 ] || fail "unpinned collisions: ${n:-?} (parents: 1290)"
    say "PASS ns ($n unpinned, as the parents)" ;;
  log) log_check "${2:?kind}" ;;
  pipeline)
    for k in common standalone mcp sweep fileio aot validate win timing; do ( log_check $k ) || exit 1; done
    say "PASS pipeline" ;;
  *) echo "usage: $0 CHECK [ARGS]" >&2; exit 2 ;;
esac
