#!/usr/bin/env bash
# integ-wh-check.sh CHECK [ARGS]: the machine checks of tools/ai/integ-wh-progress.org.
#
# Binary: target/integ/nelisp-dyn (INTEG_WH_DYN overrides).  GNU 31.1 is
# /usr/local/bin/emacs (GNU_EMACS overrides).  Every NeLisp and GNU run is capped
# at 8 GiB of address space.  Exit 0 iff the check passes.
#
#   merged NAME   codegen-wh | base | strings | hashtab | f-final | gc-port | callpath: the lane
#                 tip is the second
#                 parent of a first-parent merge commit between batch F and HEAD
#   hygiene       no conflict markers in tracked files; every Elisp file listed in
#                 tools/ai/integ-wh-resolved.txt (the hand-resolved ones) passes
#                 check-parens
#   invariants    the pinned conflict points: the root-evidence node bound agrees in
#                 its three files, the keymap extraction list is intact, and the
#                 bytecode lean-event test reads the WORD tag, not a header
#   ns            host-Emacs ns-inventory: no unpinned collision beyond 1290
#   codegen-ert   the AOT compiler, codegen, S5 lowering, alignment, artifact and linker
#                 ERT suites on host Emacs
#   built         the dynamic binary exists and names HEAD's tree
#   boot          the dynamic binary evaluates (+ 1 2)
#   sanity        fib, a cons-heavy loop and a string-heavy loop print GNU's bytes
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
fail() { say "FAIL: $*"; exit 1; }
capped() { (ulimit -v 8388608; exec "$@"); }
# The binary may predate HEAD only by commits to this dry run's own files.
built_from_head_tree() {  # built_from_head_tree REV
  git diff --quiet "$1" HEAD -- . ':(exclude)tools/ai/integ-wh-check.sh' \
    ':(exclude)tools/ai/integ-wh-progress.org' ':(exclude)tools/ai/integ-wh-build.sh' \
    ':(exclude)tools/ai/integ-wh-sanity.el' ':(exclude)tools/ai/integ-wh-mcp-smoke.sh' \
    ':(exclude)tools/ai/integ-wh-resolved.txt'
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
      callpath)   rev=fd4f1d3a0 ;;
      *) fail "usage: merged codegen-wh|base|strings|hashtab|f-final|gc-port|callpath" ;;
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
    say "PASS invariants (node bound$bounds, one keymap list, lean-event reads the tag)" ;;
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
      *) fail "unknown log kind ${2:-}" ;;
    esac
    [ "$f" -nt "$DYN" ] || fail "$f is older than the dynamic binary"
    say "PASS log $2 ($f)" ;;
  *) echo "usage: $0 CHECK [ARGS]" >&2; exit 2 ;;
esac
