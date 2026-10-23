#!/usr/bin/env bash
# integ-wh-check.sh CHECK [ARGS]: the machine checks of tools/ai/integ-wh-progress.org.
#
# Binary: target/integ/nelisp-dyn (INTEG_WH_DYN overrides).  GNU 31.1 is
# /usr/local/bin/emacs (GNU_EMACS overrides).  Every NeLisp and GNU run is capped
# at 8 GiB of address space.  Exit 0 iff the check passes.
#
#   merged NAME   codegen-wh | base | strings | hashtab | f-final | gc-port | callpath | callpath-final | codegen: the lane
#                 tip is the second
#                 parent of a first-parent merge commit between batch F and HEAD
#   hygiene       no conflict markers in tracked files; every Elisp file listed in
#                 tools/ai/integ-wh-resolved.txt (the hand-resolved ones) passes
#                 check-parens
#   invariants    the pinned conflict points: the root-evidence node bound agrees in
#                 its three files, the keymap extraction list is intact, and the
#                 bytecode lean-event test reads the WORD tag, not a header
#   constants     no unrouted write to GNU's six C-level constants (most-positive-fixnum, most-negative-fixnum,
#                 enable-multibyte-characters, font-weight/slant/width-table): tools/ai/integ-wh-const-writes.el
#   ns            host-Emacs ns-inventory: no unpinned collision beyond 1290
#   raw-file      literal file reads keep every byte (insert-file-contents-literally, no-conversion), against GNU
#   raw-literal   string literals with raw-byte escapes read unibyte, as GNU reads them (byte-code strings)
#   view-image    string views held by containers survive an image dump and restore (the census's SIGSEGV)
#   static        the static binary of the same tree: sanity and checks.sh with it in both slots
#   win           the Windows exe of the same tree and its wine smoke (WIN-SMOKE PASS)
#   alloc         gc-port's allocation rows: cons at most 100, a 128-byte string 1,300 and a 16-slot vector 700 Ir
#   gc3           gc-port's gc3 wall on the measurement CPU: worst median ratio against GNU at most 1.20
#   inliner-ert   the import-aware pass's 41 tests and codegen's four inliner ERT files on host Emacs
#   codegen-ert   the AOT compiler, codegen, S5 lowering, alignment, artifact and linker
#                 ERT suites on host Emacs
#   built         the dynamic binary exists and names HEAD's tree
#   boot          the dynamic binary evaluates (+ 1 2)
#   sanity        fib, a cons-heavy loop and a string-heavy loop print GNU's bytes
#   weakmark      fix 20: test/standalone-weak-symbol-mark-check.el passes on the binary and on a copy without
#                 its cold image, and a fresh-prelude boot dumps an image in-process four times
#   log KIND      pipeline evidence newer than the dynamic binary: checks standalone mcp
#                 fchecks (batch F's fixture checks) gcport (gc-port's light set)
cd "$(dirname "$0")/../.." || exit 2
ROOT=$PWD
DYN=${INTEG_WH_DYN:-$ROOT/target/integ/nelisp-dyn}
GNU=${GNU_EMACS:-/usr/local/bin/emacs}
OUT=$ROOT/target/integ-wh; mkdir -p "$OUT"
export TMPDIR=$ROOT/target/tmp XDG_CACHE_HOME=$ROOT/target/xdg-cache
mkdir -p "$TMPDIR" "$XDG_CACHE_HOME"

BASE=e98f397f7      # batch F (eval/integ-20261022f), the parent of this dry run

say() { echo "INTEG-WH $*"; }
CHECK=${1:-}${2:+ $2}
fail() { say "FAIL: $*"; printf '%s [%s] %s\n' "$(date +%T)" "$CHECK" "$*" >> "$OUT/fail.log" 2>/dev/null; exit 1; }
capped() { (ulimit -v 8388608; exec "$@"); }
# The binary may predate HEAD only by commits to this dry run's own files.
built_from_head_tree() {  # built_from_head_tree REV
  git diff --quiet "$1" HEAD -- . ':(exclude)tools/ai/integ-wh-check.sh' \
    ':(exclude)tools/ai/integ-wh-progress.org' ':(exclude)tools/ai/integ-wh-build.sh' \
    ':(exclude)tools/ai/integ-wh-sanity.el' ':(exclude)tools/ai/integ-wh-mcp-smoke.sh' \
    ':(exclude)tools/ai/integ-wh-resolved.txt' ':(exclude)tools/ai/integ-wh-*' \
    ':(exclude)tools/ai/gc-port-gc3.py' ':(exclude)tools/ai/gc-port-ir.py'
}

case ${1:-} in
  merged)
    case ${2:-} in
      codegen-wh) rev=27ed5f84a ;;
      base)       rev=702ccc8d7 ;;
      strings)    rev=375939a85 ;;
      hashtab)    rev=76f7c484a ;;
      f-final)    rev=b994d7227 ;;
      gc-port)    rev=7b8916b7f ;;
      callpath)   rev=e4755b52f ;;
      callpath-final) rev=82e68b055 ;;
      codegen)    rev=18f180830 ;;
      *) fail "usage: merged codegen-wh|base|strings|hashtab|f-final|gc-port|callpath|callpath-final|codegen" ;;
    esac
    git merge-base --is-ancestor $rev HEAD || fail "$2: $rev is not an ancestor of HEAD"
    git log --first-parent --merges --format='%P' "$BASE"..HEAD | awk '{print $2}' | grep -q "^$rev" ||
      fail "$2: no first-parent merge commit has $rev as its second parent"
    say "PASS merged $2 ($rev)" ;;
  hygiene)
    bad=$(git grep -lE '^(<<<<<<<|>>>>>>>) ' -- lisp scripts src test tools packages templates ':!*.org' ':!*.md' | head -3)
    [ -z "$bad" ] || fail "conflict markers in: $bad"
    files=$(grep -v '^#' tools/ai/integ-wh-resolved.txt 2>/dev/null | grep -E '\.el$')
    [ -n "$files" ] || fail "tools/ai/integ-wh-resolved.txt lists no Elisp file"
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
  invariants)
    # The root-evidence node bound must be the same number in its three files.
    bounds=""
    for f in lisp/nelisp-native-rooted-startup-evidence.el templates/nelisp-native-rooted-abi-proof.el.in \
             test/nelisp-native-compiler-startup-evidence-test.el; do
      b=$(grep -o -E '\(unless \(and \(<= nodes [0-9]+\) \(<= (depth|deepest) 64\)\)' "$f" | sort -u | grep -o -E '[0-9]+' | head -1)
      [ -n "$b" ] || fail "no root-evidence node bound in $f"
      bounds="$bounds $b"
    done
    [ "$(printf '%s\n' $bounds | sort -u | wc -l)" = 1 ] || fail "node bounds disagree:$bounds"
    n=$(grep -c "'(keymap--check keymap--compile-check key-parse key-valid-p keymap-set" scripts/nelisp-standalone-build.el)
    [ "$n" = 1 ] || fail "the keymap extraction list occurs $n times in scripts/nelisp-standalone-build.el"
    grep -q -x "      define-keymap--compile define-keymap))" scripts/nelisp-standalone-build.el ||
      fail "the keymap extraction list lost its define-keymap--compile line"
    body=$(sed -n '/(defun wf_bytecode_w_lean_event_p/,/(defun wf_bytecode_w_lean_unbind/p' scripts/nelisp-standalone-build.el)
    [ -n "$body" ] || fail "wf_bytecode_w_lean_event_p is missing"
    printf '%s\n' "$body" | grep -q -E 'ptr-read-u64|heap-read-u64' && fail "the lean-event test still reads a header"
    printf '%s\n' "$body" | grep -q -E 'nl_val_tag|sexp-tag' || fail "the lean-event test does not read the WORD tag"
    # The static symbol ids are positions: &optional and &rest stay the last two names of the extras
    # list (GNU's Qand_optional and Qand_rest), so no id is renumbered by a merge that adds names before them.
    ext=$(sed -n '/^(defconst nelisp-standalone--static-symbol-extras/,/^  "Static names beyond/p' scripts/nelisp-standalone-build.el)
    printf '%s\n' "$ext" | grep -q -x '    "&optional" "&rest")' ||
      fail "&optional and &rest are not the last two names of nelisp-standalone--static-symbol-extras"
    say "PASS invariants (node bound$bounds, one keymap list, lean-event reads the tag, &optional and &rest last)" ;;
  constants)
    # GNU's six C-level constants refuse writes (callpath 467c73b19).  The scan reads every Elisp file of the
    # runtime, the staged and vendored GNU libraries, the packages and the tests with the real reader; a
    # write site must be in tools/ai/integ-wh-const-writes.allow, and no file may be unreadable.
    capped timeout 590 "$GNU" -Q --batch -l tools/ai/integ-wh-const-writes.el -- scripts lisp src vendor/staged-emacs-lisp \
      vendor/emacs-lisp vendor/emacs-lisp-api packages standalone-compat nelisp-emacs-lib test > "$OUT/constants.log" 2>&1
    rc=$?
    last=$(tail -n 1 "$OUT/constants.log")
    [ $rc = 0 ] || fail "constants: rc=$rc: $(grep -v '^SITE allowed' "$OUT/constants.log" | tail -n 3 | cut -c1-200 | tr '\n' '|')"
    case $last in *" 0 unreadable") ;; *) fail "constants: $last" ;; esac
    say "PASS constants ($last)" ;;
  ns)
    bash tools/ai/batch-f-check.sh ns ;;
  codegen-ert)
    # A suite may fail only on the tests listed in tools/ai/integ-wh-ert-known-red.txt, which
    # are red on the lane tips themselves.
    known=$(grep -v '^#' tools/ai/integ-wh-ert-known-red.txt | grep -v '^$')
    nred=0; total=0
    for t in nelisp-aot-compiler-test nelisp-aot-codegen-test nelisp-aot-s5-lowering-test \
             nelisp-codegen-alignment-test nelisp-artifact-native-exec-test nelisp-artifact-test \
             nelisp-static-linker-test; do
      [ -f test/$t.el ] || fail "missing test/$t.el"
      capped timeout 900 "$GNU" --batch -Q -L lisp -L src -L scripts -L test --eval '(setq load-prefer-newer t)' \
        -l ert -l test/$t.el -f ert-run-tests-batch-and-exit > "$OUT/ert-$t.log" 2>&1
      total=$((total + 1))
      grep -q '^Ran ' "$OUT/ert-$t.log" || fail "$t: no ERT summary: $(tail -n 3 "$OUT/ert-$t.log" | tr '\n' ' ')"
      for name in $(grep -E '^   FAILED  [^ ]+$' "$OUT/ert-$t.log" | awk '{print $2}'); do
        if printf '%s\n' "$known" | grep -qx "$name"; then nred=$((nred + 1))
        else fail "$t: $name is red and not in tools/ai/integ-wh-ert-known-red.txt"; fi
      done
    done
    say "PASS codegen-ert ($total suites; $nred known-red tests, all listed)" ;;
  gc3)
    # gc-port's gc3 wall (three explicit collections of the cons, string and vector churn, paired rounds against GNU
    # 31.1 on the integration's measurement CPU 11, GCP_CPU overrides): the lane's tip measured 1.07 to 1.14; the
    # guard is the lane's worst figure plus the machine's spread.
    [ -x "$DYN" ] || fail "missing dynamic binary $DYN"
    GCP_CPU=${GCP_CPU:-11} capped timeout 590 python3 tools/ai/gc-port-gc3.py check --dyn "$DYN" --rounds 9 > "$OUT/gc3.log" 2>&1
    got=$(grep -E '^(PASS|FAIL) gc3 ' "$OUT/gc3.log")
    [ "$(printf '%s\n' "$got" | wc -l)" = 3 ] || fail "gc3: no three verdicts: $(tail -n 2 "$OUT/gc3.log" | tr '\n' ' ')"
    worst=$(printf '%s\n' "$got" | sed -n 's/.*median ratio x\([0-9.]*\).*/\1/p' | sort -n | tail -n 1)
    awk -v w="$worst" 'BEGIN { exit !(w <= 1.20) }' || fail "gc3: worst median ratio x$worst against GNU (guard 1.20; gc-port's tip 1.14)"
    say "PASS gc3 (worst median ratio x$worst against GNU; gc-port's tip x1.14)" ;;
  static)
    # The static linkage of the same tree.  target/final-verify/checks.sh takes the dynamic binary for its standalone
    # checks (SQLite and the FFI libraries need dynamic linkage) and the static one for the four thread groups, so
    # both slots are filled with their own binary here; gc-port's light set takes both too.  The static binary must
    # name HEAD's tree, print GNU's bytes on the sanity program, pass the 11 groups, and give gc-port's set the same
    # verdicts as the dynamic one (budget and vm-poll PASS; gc-stats fails only its known vectors tolerance, S2.6).
    S=${INTEG_WH_STATIC:-$ROOT/target/integ/nelisp-static}
    [ -x "$S" ] || fail "missing static binary $S"
    v=$(capped "$S" --version 2>/dev/null | head -1)
    rev=$(printf '%s\n' "$v" | sed -n 's/.*-g\([0-9a-f]\{9\}\)[ )-].*/\1/p')
    [ -n "$rev" ] && built_from_head_tree "$rev" || fail "'$v' is not built from HEAD's tree"
    case $v in *", static)"*) ;; *) fail "'$v' is not the static linkage" ;; esac
    INTEG_WH_DYN=$S bash "$0" sanity > "$OUT/static-sanity.out" 2>&1 || fail "static: $(tail -n 1 "$OUT/static-sanity.out")"
    # The meter exports LC_ALL=C; the standalone checks compare printed non-ASCII text with GNU's, so give them a UTF-8 locale.
    env LC_ALL=C.UTF-8 bash target/final-verify/checks.sh "$DYN" "$S" integ-wh-static > "$OUT/static-checks.out" 2>&1
    tail -n 1 "$OUT/static-checks.out" | grep -q '^GATE-COUNT: 11 cases, 0 failures$' ||
      fail "static: $(grep -E 'STANDALONE-CHECK FAIL|rc=[1-9]' "$OUT/static-checks.out" | head -n 2 | cut -c1-160 | tr '\n' '|')"
    GCP_WORK="$OUT/gcport-static" bash tools/ai/integ-wh-gcport.sh "$DYN" "$S" > "$OUT/static-gcport.out" 2>&1
    grep -qx 'GC-PORT-BUDGET PASS' "$OUT/static-gcport.out" && grep -qx 'GC-PORT-VM-POLL PASS' "$OUT/static-gcport.out" ||
      fail "static: gc-port's budget or vm-poll: $(cat "$OUT/static-gcport.out" | tr '\n' ' ')"
    [ "$(grep -c '^PASS nelisp-static \(dump\|restore\):' "$OUT/gcport-static/gc-stats.out")" = 2 ] &&
      [ "$(grep '^FAIL nelisp-static' "$OUT/gcport-static/gc-stats.out" | grep -vc 'dropped objects still counted (22 0 0 0 10 108)')" = 0 ] ||
      fail "static: gc-stats differs from the known vectors tolerance: $(grep '^FAIL nelisp-static' "$OUT/gcport-static/gc-stats.out" | head -n 2 | cut -c1-200)"
    say "PASS static ($rev, sanity identical to GNU, checks.sh 11 groups / 0 failures, gc-port budget and vm-poll PASS, gc-stats at its known tolerance)" ;;
  win)
    # The Windows build of the same tree and its wine smoke (tools/ai/win-smoke.sh build REV OUT, then smoke OUT).
    d=${INTEG_WH_WIN:-$(ls -dt "$ROOT"/target/win/*/ 2>/dev/null | head -n 1)}
    [ -f "$d/smoke.log" ] || fail "no smoke log in '$d'"
    c=$(cut -c1-9 "$d/commit" 2>/dev/null)
    [ -n "$c" ] && built_from_head_tree "$c" || fail "the Windows build ($c) is not HEAD's tree"
    tail -n 1 "$d/smoke.log" | grep -qx 'WIN-SMOKE PASS' || fail "$(tail -n 1 "$d/smoke.log")"
    [ -s "$d/nelisp.exe.cold" ] || fail "no cold image beside the exe"
    say "PASS win ($c, exe $(sha256sum "$d/nelisp.exe" | cut -c1-16), cold $(stat -c %s "$d/nelisp.exe.cold") B, WIN-SMOKE PASS)" ;;
  raw-file)
    # A literal read keeps every byte: insert-file-contents-literally and insert-file-contents under no-conversion
    # on a binary file, a UTF-8 text and a mixed file, against GNU 31.1 (the RAW argument of the read-file primitive).
    [ -x "$DYN" ] || fail "missing dynamic binary $DYN"
    d=$OUT/raw-file; rm -rf "$d"; mkdir -p "$d"
    python3 - "$d" <<'PYEOF' || fail "cannot write the raw-file fixtures"
import sys
d = sys.argv[1]
open(d + '/raw256.bin', 'wb').write(bytes(range(256)))
open(d + '/utf8.txt', 'wb').write('\u65e5\u672c\u8a9e\u306e\u30c6\u30ad\u30b9\u30c8\nabc \u00e9\r\n'.encode('utf-8'))
open(d + '/mixed.bin', 'wb').write(b'plain ascii\n' + bytes([0xc3, 0xa9, 0x80, 0xbf, 0xff, 0xc0, 0x41]) + '\u65e5'.encode('utf-8'))
PYEOF
    capped env IWH_RAW_DIR="$d" LC_ALL=C.UTF-8 LANG= LANGUAGE= timeout 300 "$GNU" -Q --batch -l tools/ai/integ-wh-raw-file.el \
      > "$d/gnu.out" 2> "$d/gnu.err" || fail "GNU exited $?"
    capped env IWH_RAW_DIR="$d" LC_ALL=C.UTF-8 timeout 300 "$DYN" -l tools/ai/integ-wh-raw-file.el \
      > "$d/dyn.out" 2> "$d/dyn.err"
    rc=$?
    [ $rc = 0 ] || fail "dynamic binary exited $rc: $(head -c 160 "$d/dyn.err" | tr '\n' ' ')"
    cmp -s "$d/gnu.out" "$d/dyn.out" ||
      fail "stdout differs from GNU: $(diff -a "$d/gnu.out" "$d/dyn.out" | grep -a '^[<>]' | head -n 2 | cut -c1-100 | tr '\n' '|')"
    say "PASS raw-file ($(wc -l < "$d/dyn.out") lines identical to GNU)" ;;
  raw-literal)
    # String literals with raw-byte escapes read as GNU reads them, unibyte: the byte-code strings of an .elc.
    [ -x "$DYN" ] || fail "missing dynamic binary $DYN"
    capped env LC_ALL=C.UTF-8 LANG= LANGUAGE= timeout 300 "$GNU" -Q --batch -l tools/ai/integ-wh-raw-literal.el \
      > "$OUT/raw-literal.gnu.out" 2> "$OUT/raw-literal.gnu.err" || fail "GNU exited $?"
    capped env LC_ALL=C.UTF-8 timeout 300 "$DYN" -l tools/ai/integ-wh-raw-literal.el \
      > "$OUT/raw-literal.dyn.out" 2> "$OUT/raw-literal.dyn.err"
    rc=$?
    [ $rc = 0 ] || fail "dynamic binary exited $rc: $(head -c 160 "$OUT/raw-literal.dyn.err" | tr '\n' ' ')"
    cmp -s "$OUT/raw-literal.gnu.out" "$OUT/raw-literal.dyn.out" ||
      fail "stdout differs from GNU: $(diff "$OUT/raw-literal.gnu.out" "$OUT/raw-literal.dyn.out" | grep '^[<>]' | head -n 2 | cut -c1-100 | tr '\n' '|')"
    say "PASS raw-literal ($(wc -l < "$OUT/raw-literal.dyn.out") lines identical to GNU)" ;;
  view-image)
    # String views held by containers survive an image dump and restore: the library images of the census crashed
    # in the first collection after --cold-load-from (stale view boxes of the dumping process).
    [ -x "$DYN" ] || fail "missing dynamic binary $DYN"
    out=$(capped timeout 600 bash tools/ai/integ-wh-view-image.sh "$DYN" 2>&1) ||
      fail "view-image: $(printf '%s\n' "$out" | tail -n 1 | cut -c1-240)"
    say "PASS view-image ($(printf '%s\n' "$out" | tail -n 1))" ;;
  alloc)
    # gc-port's allocation rows (callgrind, the constructor's inclusive Ir per call in an interpreted loop):
    # the lane's tip measured cons 94, a 128-byte string 1,242 and a 16-slot vector 648 Ir (GNU 26, 178, 215).
    [ -x "$DYN" ] || fail "missing dynamic binary $DYN"
    rm -rf "$OUT/alloc"; capped timeout 590 python3 tools/ai/gc-port-ir.py alloc --dyn "$DYN" --work "$OUT/alloc" > "$OUT/alloc.log" 2>&1
    cons=$(sed -n 's/.*alloc cons: NeLisp \([0-9]*\) Ir.*/\1/p' "$OUT/alloc.log")
    str=$(sed -n 's/.*alloc string: NeLisp \([0-9]*\) Ir.*/\1/p' "$OUT/alloc.log")
    vec=$(sed -n 's/.*alloc vector: NeLisp \([0-9]*\) Ir.*/\1/p' "$OUT/alloc.log")
    [ -n "$cons" ] && [ -n "$str" ] && [ -n "$vec" ] || fail "alloc: no three rows: $(tail -n 2 "$OUT/alloc.log" | tr '\n' ' ')"
    [ "$cons" -le 100 ] && [ "$str" -le 1300 ] && [ "$vec" -le 700 ] ||
      fail "alloc rows cons $cons (tip 94, cap 100), string $str (tip 1,242, cap 1,300), vector $vec (tip 648, cap 700)"
    say "PASS alloc (cons $cons, string $str, vector $vec Ir; the tip's 94, 1,242, 648)" ;;
  inliner-ert)
    # The word-heap line's import-aware pass (41 tests) and codegen's inliner (parallel-let, extern,
    # wrapper-initializer and hygiene suites): both must pass, except the listed known-red tests.
    known=$(grep -v '^#' tools/ai/integ-wh-ert-known-red.txt | grep -v '^$')
    nred=0; ran=0
    for t in nelisp-standalone-inline-import-test nelisp-standalone-inline-parallel-let-test \
             nelisp-standalone-extern-inline-test nelisp-inline-wrapper-test nelisp-standalone-inline-hygiene-test; do
      [ -f test/$t.el ] || fail "missing test/$t.el"
      capped timeout 900 "$GNU" --batch -Q -L lisp -L src -L scripts -L test --eval '(setq load-prefer-newer t)' \
        -l ert -l test/$t.el -f ert-run-tests-batch-and-exit > "$OUT/ert-$t.log" 2>&1
      grep -q '^Ran ' "$OUT/ert-$t.log" || fail "$t: no ERT summary: $(tail -n 3 "$OUT/ert-$t.log" | tr '\n' ' ')"
      ran=$((ran + $(grep -E '^Ran [0-9]+ tests' "$OUT/ert-$t.log" | awk '{print $2}')))
      for name in $(grep -E '^   FAILED  [^ ]+$' "$OUT/ert-$t.log" | awk '{print $2}'); do
        if printf '%s\n' "$known" | grep -qx "$name"; then nred=$((nred + 1))
        else fail "$t: $name is red and not in tools/ai/integ-wh-ert-known-red.txt"; fi
      done
    done
    say "PASS inliner-ert ($ran tests in 5 files; $nred known-red, listed)" ;;
  built)
    [ -x "$DYN" ] || fail "missing dynamic binary $DYN"
    v=$(capped "$DYN" --version 2>/dev/null | head -1)
    rev=$(printf '%s\n' "$v" | sed -n 's/.*-g\([0-9a-f]\{9\}\) (.*/\1/p')
    [ -n "$rev" ] && built_from_head_tree "$rev" || fail "'$v' is not built from HEAD's tree"
    say "PASS built ($rev, HEAD's tree)" ;;
  boot)
    [ -x "$DYN" ] || fail "missing dynamic binary $DYN"
    # --eval echoes its return value after princ's output; batch F's boot check knows.
    out=$(BATCH_F_DYN=$DYN BATCH_F_STATIC=$DYN bash tools/ai/batch-f-check.sh boot 2>&1) || fail "boot: $out"
    say "PASS boot" ;;
  sanity)
    [ -x "$DYN" ] || fail "missing dynamic binary $DYN"
    capped env LC_ALL=C.UTF-8 LANG= LANGUAGE= timeout 300 "$GNU" -Q --batch -l tools/ai/integ-wh-sanity.el \
      > "$OUT/sanity.gnu.out" 2> "$OUT/sanity.gnu.err" || fail "GNU exited $?"
    capped env LC_ALL=C.UTF-8 timeout 300 "$DYN" -l tools/ai/integ-wh-sanity.el \
      > "$OUT/sanity.dyn.out" 2> "$OUT/sanity.dyn.err"
    rc=$?
    [ $rc = 0 ] || fail "dynamic binary exited $rc (signal $(( rc > 128 ? rc - 128 : 0 )))"
    cmp -s "$OUT/sanity.gnu.out" "$OUT/sanity.dyn.out" || fail "stdout differs from GNU ($(diff "$OUT/sanity.gnu.out" "$OUT/sanity.dyn.out" | grep -c '^[<>]') lines)"
    [ -s "$OUT/sanity.dyn.err" ] && fail "stderr not empty: $(head -c 140 "$OUT/sanity.dyn.err" | tr '\n' ' ')"
    say "PASS sanity ($(wc -l < "$OUT/sanity.dyn.out") lines identical to GNU)" ;;
  weakmark)
    [ -x "$DYN" ] || fail "missing dynamic binary $DYN"
    # The collector's weak-table marker used to mark a symbol's cell and trace none of its slots (fix 20).
    # Run the check on the binary as built (with its cold image) and on a copy without the image, which
    # boots the prelude afresh, as the build does to make the image.
    c=$OUT/nocold; rm -rf "$c"; mkdir -p "$c"; cp -p "$DYN" "$c/nelisp" || fail "cannot copy the binary"
    for e in .buffer-local-bytecode.tsv .native-startup.el .variable-alias-bytecode.tsv .version.json; do
      [ -f "$DYN$e" ] && cp -p "$DYN$e" "$c/nelisp$e"; done
    for b in "$DYN" "$c/nelisp"; do
      capped env EVAL_BIN="$b" STANDALONE_CHECK_LOGDIR="$OUT/weakmark" sh test/perf/standalone-checks.sh \
        test/standalone-weak-symbol-mark-check.el > "$OUT/weakmark.out" 2>&1 \
        || fail "the weak-symbol check fails on $b: $(tail -n 3 "$OUT/weakmark.out" | tr '\n' ' ')"
    done
    # A fresh-prelude boot and an image dump in the same process: the unfixed binaries died in it or
    # produced an image with an empty name box, depending on the environment's size.
    for i in 1 2 3 4; do
      img=$OUT/weakmark-$i.img; rm -f "$img"
      if [ "$i" -gt 2 ] && command -v setarch > /dev/null; then run="setarch $(uname -m) -R"; else run=; fi
      got=$(capped $run timeout 300 "$c/nelisp" --eval "(progn (princ (symbol-name 'eq)) (princ (nelisp--arena-dump-image-stream \"$img\")))" 2>&1)
      rc=$?
      case $got in eq[0-9]*) ;; *) fail "fresh boot plus dump, run $i ($run): rc=$rc, '$(printf %s "$got" | head -c 80)'" ;; esac
      rm -f "$img"
    done
    say "PASS weakmark (the check on the binary and on a copy without its cold image; 4 fresh-boot dumps)" ;;
  n4)
    # The activation-record (N4) API corpus against live GNU 31.1: 81 rows (callpath item A).
    [ -x "$DYN" ] || fail "missing dynamic binary $DYN"
    out=$(capped timeout 280 python3 test/abi-n4-reference-test.py "$DYN" 2>&1) || fail "N4 reference: $(printf %s "$out" | tail -n 1 | cut -c1-200)"
    case $out in *"N4-REFERENCE checks=81 PASS"*) say "PASS n4 (81 rows equal to live GNU 31.1)" ;; *) fail "N4 reference: $(printf %s "$out" | tail -n 1 | cut -c1-200)" ;; esac ;;
  precise)
    # With the conservative stack scan off (nelisp--debug-switch 28) only precise roots count: the symbol
    # object roots (51 checks) and the argument words of an activation (19 checks).
    [ -x "$DYN" ] || fail "missing dynamic binary $DYN"
    a=$(capped timeout 280 "$DYN" --load test/standalone-symbol-precise-root-check.el 2>&1 | tail -n 1)
    b=$(capped timeout 280 "$DYN" --load test/standalone-argv-precise-root-check.el 2>&1 | tail -n 1)
    case $a in "SYM-PRECISE checks=51 failures=0 PASS") ;; *) fail "symbol precise roots: $a" ;; esac
    case $b in "ARGV-PRECISE checks=19 failures=0 PASS") ;; *) fail "argv precise roots: $b" ;; esac
    say "PASS precise (51 + 19 checks with the conservative scan off)" ;;
  log)
    [ -x "$DYN" ] || fail "missing dynamic binary $DYN"
    case ${2:-} in
      checks)     f=target/final-verify/integ-wh-summary.log
                  grep -q 'GATE-COUNT: 11 cases, 0 failures' "$f" 2>/dev/null || fail "checks.sh not 11/0 in $f" ;;
      standalone) f=target/final-verify/checks-integ-wh/standalone-checks.log
                  grep -qx 'STANDALONE-CHECKS PASS' "$f" 2>/dev/null || fail "test/perf/standalone-checks.sh not PASS in $f" ;;
      fchecks)    f=target/final-verify/integ-wh-fchecks.log
                  tail -n 1 "$f" 2>/dev/null | grep -q '^FCHECKS PASS ' || fail "batch F's fixture checks not all PASS in $f" ;;
      gcport)     f=target/final-verify/integ-wh-gcport.log
                  [ "$(grep -c '^GC-PORT-[A-Z-]* PASS' "$f" 2>/dev/null)" = 3 ] || fail "gc-port's light set not 3 PASS in $f" ;;
      mcp)        f=target/final-verify/integ-wh-mcp.log
                  sed -n 2p "$f" 2>/dev/null | grep -qx '2 tools=40' || fail "MCP not 40 tools in $f" ;;
      scale)      f=target/final-verify/integ-wh-scale.log
                  tail -n 1 "$f" 2>/dev/null | grep -qx 'IMAGE-SCALE PASS' || fail "the at-scale image check not PASS in $f" ;;
      arity)      f=target/final-verify/integ-wh-arity/report.json
                  sha=$(sha256sum "$DYN" | cut -d' ' -f1)
                  python3 - "$f" "$sha" <<'PY' || fail "the arity sweep did not pass on this binary ($f)"
import json, sys
d = json.load(open(sys.argv[1]))
assert d["passed"] and d["counts"]["failed"] == 0 and d["counts"]["gnu_passed"] == d["counts"]["gnu_cases"]
assert d["binary"]["sha256"] == sys.argv[2], "report is for another binary"
print("arity sweep: %d cases over %d builtins, 0 failed" % (d["counts"]["passed"], d["counts"]["builtins"]))
PY
                  ;;
      evalrows)   f=target/callpath-perf/evalrows/${EVALROWS_TAG:-wh2}/report.json
                  sha=$(sha256sum "$DYN" | cut -d' ' -f1)
                  python3 - "$f" "$sha" <<'PY' || fail "the interpreted builtin-call rows are not within the callpath stage's figures ($f)"
import json, sys
d = json.load(open(sys.argv[1]))
assert d["binary"]["sha256"] == sys.argv[2], "report is for another binary"
a = d["above_control"]["nelisp"]
# callpath e4755b52f measured 1,769 and 2,429 Ir above the control on its own base; the gc-port line adds a
# typed cons cell and the budget word to an allocation.  Without the callpath stage (m6) the rows were 7,216 and 10,286;
# the final tip (m9) measured 922 and 1,280.
assert a["plus"] <= 870 and a["cons"] <= 1140 and a["makevec"] <= 1310, a
# Interpreted loops per iteration and the bytecode rows, Ir per call above the control: the callpath final tip
# on the word-heap line (m9) measured while 4,720, while3 8,016, closure 7,920, funcall 600, optional-rest 842;
# the integrated tip (m11, fixes 22-25 on the codegen merge) measured 3,751, 6,338, 6,408, 607 and 876, and plus 798, cons 1,054, makevec 1,209.
lp = d["loops"]["nelisp"]
assert lp["while"] <= 4050 and lp["while3"] <= 6850 and lp["closure"] <= 6920, lp
bc = d["bc"]["nelisp"]; ctl = bc["pcf-control"]
assert bc["pcf-funcall"] - ctl <= 660 and bc["pcf-optional-rest"] - ctl <= 950, bc
print("Ir above control: plus %.0f cons %.0f makevec %.0f; loops while %.0f while3 %.0f closure %.0f; bytecode funcall %.0f optional-rest %.0f"
      % (a["plus"], a["cons"], a["makevec"], lp["while"], lp["while3"], lp["closure"],
         bc["pcf-funcall"] - ctl, bc["pcf-optional-rest"] - ctl))
PY
                  ;;
      *) fail "unknown log kind ${2:-}" ;;
    esac
    [ "$f" -nt "$DYN" ] || fail "$f is older than the dynamic binary"
    say "PASS log $2 ($f)" ;;
  *) echo "usage: $0 CHECK [ARGS]" >&2; exit 2 ;;
esac
