#!/usr/bin/env bash
# batch-e-check.sh CHECK [ARGS]: the machine checks of tools/ai/batch-e-progress.org.
#
# Binaries: target/integ/nelisp-{dyn,static} (BATCH_E_DYN / BATCH_E_STATIC
# override).  GNU 31.1 is /usr/local/bin/emacs.  Every NeLisp and GNU run is
# capped at 8 GiB of address space.  Exit 0 iff the check passes.
#
#   merged            syntax-rt and elc-load closure commits merged; one
#                     case-fold canon shortcut; nelisp-elc-reader unprovided
#   integrity         the Unicode payload test (621386274) on host Emacs
#   interface         nelisp--syntax-standard-table is (standard-syntax-table)
#   closures          elc-load's GNU interpreted-closure fixture, both linkages
#   syntax-rt         syntax-rt's semantics and char-table GNU fixtures
#   fixture NAME LOC  test/fixtures/NAME.el: GNU still prints the pin
#                     (NAME.LOC.gnu-31.1.expected, else NAME.gnu-31.1.expected)
#                     and both linkages print it byte for byte, empty stderr
#   all-fixtures      every batch-e fixture in every locale it is pinned for
#   coldload          --cold-load-from NONEXISTENT is fatal on both linkages
#   built             both linkages exist and name HEAD (not -dirty)
#   casetab           casetab's GNU fixtures (API 43, behaviour 47)
#   ns                ns-inventory: no unpinned collision beyond 1290
#   log KIND          pipeline evidence newer than the dynamic binary:
#                     common mcp sweep fileio aot win timing
#   pipeline          every log check
cd "$(dirname "$0")/../.." || exit 2
ROOT=$PWD
DYN=${BATCH_E_DYN:-$ROOT/target/integ/nelisp-dyn}
ST=${BATCH_E_STATIC:-$ROOT/target/integ/nelisp-static}
GNU=/usr/local/bin/emacs
OUT=$ROOT/target/batch-e; mkdir -p "$OUT"
export TMPDIR=$ROOT/target/tmp XDG_CACHE_HOME=$ROOT/target/xdg-cache
mkdir -p "$TMPDIR" "$XDG_CACHE_HOME"

say() { echo "BATCH-E $*"; }
fail() { say "FAIL: $*"; exit 1; }
capped() { (ulimit -v 8388608; exec "$@"); }

# run_fixture BIN FIXTURE LOCALE OUTFILE ERRFILE -> exit status
run_nelisp() {
  env LC_ALL="$3" capped timeout 300 "$1" --load "$2" > "$4" 2> "$5"
}
run_gnu() {
  env LC_ALL="$2" LANG= LANGUAGE= capped timeout 300 "$GNU" -Q --batch -l "$1" > "$3" 2> /dev/null
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
    got=$(capped timeout 120 "$bin" --load "$OUT/eval.el" 2>/dev/null)
    [ "$got" = "$2" ] || fail "$tag: $1 printed '$got', want '$2'"
  done
}

log_check() {
  local kind=$1 f
  [ -x "$DYN" ] || fail "missing dynamic binary"
  case $kind in
    common) f=target/final-verify/integE-summary.log
            grep -q 'GATE-COUNT: 11 cases, 0 failures' "$f" 2>/dev/null || fail "COMMON not 11/0 in $f" ;;
    mcp)    f=target/final-verify/integE-mcp.log
            sed -n 2p "$f" 2>/dev/null | grep -q '^tools=40$' || fail "MCP not 40 tools in $f" ;;
    sweep)  f=target/integ/sweep-E.txt
            [ "$(grep -c 'SIGSEGV=0/' "$f" 2>/dev/null)" = 4 ] && ! grep -q 'other=[^ ]' "$f" ||
              fail "sweep not clean in $f" ;;
    fileio) f=target/integ/focused/summary.log
            [ "$(grep -c '^FILEIO2 .* PASS' "$f" 2>/dev/null)" = 2 ] || fail "fileio-2 not PASS on both in $f" ;;
    aot)    f=target/integ/focused/summary.log
            [ "$(grep -c '^AOT-DIFF .* cases=97 wrong-chunks=0 crashes=0 PASS' "$f" 2>/dev/null)" = 2 ] ||
              fail "AOT differential not 97/97 on both in $f" ;;
    win)    f=target/win/E/smoke.log
            [ "$(cat target/win/E/commit 2>/dev/null)" = "$(git rev-parse HEAD)" ] || fail "Windows build is not HEAD"
            tail -1 "$f" 2>/dev/null | grep -q '^WIN-SMOKE PASS' || fail "Windows smoke not PASS in $f" ;;
    timing) f=target/integ/timing-E/report.txt
            grep -q 'ratio median' "$f" 2>/dev/null || fail "no timing report $f" ;;
    *) fail "unknown log kind $kind" ;;
  esac
  [ "$kind" = win ] || [ "$f" -nt "$DYN" ] || fail "$f is older than the dynamic binary"
  say "PASS log $kind ($f)"
}

case ${1:-} in
  merged)
    git merge-base --is-ancestor 42f1e3900 HEAD || fail "syntax-rt 42f1e3900 not merged"
    git merge-base --is-ancestor c730f1a55 HEAD || fail "elc-load c730f1a55 not merged"
    [ "$(grep -c '^(defun nlre--current-translate-table' lisp/nelisp-stdlib-regexp.el)" = 1 ] ||
      fail "not exactly one canon read"
    grep -q 'nelisp-casetab--builtin-canon' lisp/nelisp-stdlib-regexp.el || fail "canon shortcut missing"
    ! grep -q 'search-canon' lisp/nelisp-stdlib-regexp.el scripts/nelisp-stdlib-prelude.el \
        packages/nelisp-emacs-foundation/src/emacs-cc-casetab-1.el || fail "a second canon shortcut remains"
    ! grep -q "(provide 'nelisp-elc-reader)" scripts/nelisp-stdlib-prelude.el || fail "nelisp-elc-reader provided"
    say "PASS merged" ;;
  integrity)
    capped timeout 300 "$GNU" -Q --batch -L lisp -L src -L test -l test/nelisp-regex-replacement-test.el \
      --eval '(ert-run-tests-batch-and-exit "unicode-data-is-intact")' > "$OUT/integrity.log" 2>&1 ||
      fail "unicode-data-is-intact"
    say "PASS integrity" ;;
  interface)
    eval_both "(princ (and (boundp 'nelisp--syntax-standard-table) (eq nelisp--syntax-standard-table (standard-syntax-table))))" t
    say "PASS interface" ;;
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
    for m in semantics chartab; do
      SYNTAX_RT_DYN=$DYN SYNTAX_RT_STATIC=$ST SYNTAX_RT_CPU=0-7 python3 tools/ai/syntax-rt-check.py $m \
        > "$OUT/syntax-rt-$m.log" 2>&1 || fail "syntax-rt $m ($(tail -1 "$OUT/syntax-rt-$m.log"))"
    done
    say "PASS syntax-rt semantics and chartab" ;;
  fixture) check_fixture "${2:?fixture name}" "${3:-C.UTF-8}" ;;
  all-fixtures)
    for spec in batch-e-syntax-buffer:C.UTF-8 batch-e-category-table:C.UTF-8 batch-e-eval-special:C.UTF-8 \
                batch-e-quote-style:C batch-e-quote-style:C.UTF-8; do
      ( check_fixture "${spec%%:*}" "${spec#*:}" ) || exit 1
    done
    "$0" coldload || exit 1
    say "PASS all fixtures" ;;
  coldload)
    for tag in dyn static; do
      bin=$DYN; [ $tag = static ] && bin=$ST
      [ -x "$bin" ] || fail "missing $tag binary"
      capped timeout 60 "$bin" --cold-load-from /nonexistent/batch-e.cold --eval '(princ "ran")' \
        > "$OUT/coldload.$tag.out" 2> "$OUT/coldload.$tag.err"; rc=$?
      # GNU: emacs --dump-file /nonexistent/x.pdmp exits 1 with
      # "emacs: could not load dump file \"...\": could not open file".
      [ $rc = 1 ] || fail "$tag: exit $rc, want 1"
      [ -s "$OUT/coldload.$tag.out" ] && fail "$tag: evaluated past the missing image"
      grep -q 'could not load' "$OUT/coldload.$tag.err" || fail "$tag: no fatal message"
    done
    say "PASS coldload" ;;
  built)
    head=$(git rev-parse --short=9 HEAD)
    for tag in dyn static; do
      bin=$DYN; [ $tag = static ] && bin=$ST
      v=$(capped "$bin" --version 2>/dev/null | head -1)
      case $v in *"-g$head ("*) ;; *) fail "$tag: '$v' does not name $head" ;; esac
    done
    say "PASS built ($head)" ;;
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
    for k in common mcp sweep fileio aot win timing; do ( log_check $k ) || exit 1; done
    say "PASS pipeline" ;;
  *) echo "usage: $0 CHECK [ARGS]" >&2; exit 2 ;;
esac
