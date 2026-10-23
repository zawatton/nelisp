#!/usr/bin/env bash
# integ-wh-gc-discriminator.sh COMMAND [ARGS]: does anything rely on a string-data word to keep its string alive?
#
# GNU's conservative scan drops every word that points into string data (alloc.c mark_maybe_pointer, MEM_TYPE_NON_LISP):
# a string is a Lisp_Object, and nothing may hold only its data pointer across a collection.  This runtime's untyped blocks
# held such pointers in two shapes (the box of a mutable string builder, the frame slot of a legacy string view), so a
# first prototype let a data word read from a pinned block still mark its owner (the "hybrid" form).  The question this
# answers is whether that walk can go (the "pure" form, GNU's): both forms are built with every swept string's data
# overwritten with 0xDD (tools/ai/integ-wh-gc-poison.patch), so a read of a freed string shows as garbage or a crash, and
# the same checks run on both.  What differs between the forms is only the tree: the tree you run this in has one of them,
# and tools/ai/integ-wh-gc-hybrid.patch turns the pure tree into the other.
#
#   variants [REV]     build target/integ/nelisp-dyn-poison-pure and -poison-hyb from REV (default HEAD): REV plus the poison
#                      patch, and REV plus the hybrid and poison patches (one heavy unit each, about 11 minutes)
#   groups VARIANT     target/final-verify/checks.sh (the 11 groups) on target/integ/nelisp-dyn-VARIANT, with a private log
#                      directory (two runs sharing target/standalone-checks overwrite each other's files and report
#                      "GNU output mismatch" for the tests they overlap on: symbols and syntax-rt-chartab once)
#   forced VARIANT [SECONDS [FIRST]]
#                      every standalone check with (nelisp--debug-switch 70), a collection at every bytecode safepoint,
#                      SECONDS per test (default 25; a test that cannot finish is TIMEOUT, not a verdict), from test FIRST
#   goldens VARIANT    the GNU-compared goldens of tools/ai (no-props, string-repr, reader-labels, raw-literal, sanity)
#                      normally and forced, and gc-builders in both modes
#   lib VARIANT        the library's base, GUI and SKK images built with the variant and checked; needs NELISP_LIB (a
#                      checkout of the library with its build/*.el bundles) and NELISP_LIB_FIXTURES (a library tree with
#                      build/gui-skk-evil-image/vendor and packages/nelisp-gui-xcb/fixtures/SKK-JISYO.gui)
#
# Every NeLisp and GNU run is capped at 8 GiB of address space.  Output goes to target/disc/ (one line per test or step on
# stdout).  The exit status is the number of failures.  GNU_EMACS and HEAVY_RUN override /usr/local/bin/emacs and the
# nearest bin/heavy-run.sh.
cd "$(dirname "$0")/../.." || exit 2
ROOT=$PWD
GNU=${GNU_EMACS:-/usr/local/bin/emacs}
OUT=$ROOT/target/disc
export LC_ALL=C.UTF-8
mkdir -p "$OUT"
heavy() {  # heavy COMMAND...: one heavy unit
  local h=${HEAVY_RUN:-} d=$ROOT
  if [ -z "$h" ]; then
    while [ "$d" != / ]; do [ -x "$d/bin/heavy-run.sh" ] && { h=$d/bin/heavy-run.sh; break; }; d=$(dirname "$d"); done
  fi
  [ -x "$h" ] || { echo "heavy-run.sh not found; set HEAVY_RUN"; return 2; }
  "$h" "$@"
}
bin_of() { echo "$ROOT/target/integ/nelisp-dyn-$1"; }

cmd=${1:-}; shift
case $cmd in
  variants)
    rev=$(git rev-parse --verify "${1:-HEAD}^{commit}") || exit 2
    mk() {  # mk NAME PATCH...: a commit of REV with the patches applied, built as nelisp-dyn-NAME
      local name=$1; shift
      local idx; idx=$(mktemp) || exit 2; rm -f "$idx"
      GIT_INDEX_FILE=$idx git read-tree "$rev" || exit 2
      for p in "$@"; do GIT_INDEX_FILE=$idx git apply --cached "$p" || { echo "patch $p does not apply to $rev"; exit 2; }; done
      local tree; tree=$(GIT_INDEX_FILE=$idx git write-tree); rm -f "$idx"
      local c; c=$(git commit-tree "$tree" -p "$rev" -m "variant $name of $rev (not for any branch)")
      BUILD_OUT=target/integ/nelisp-dyn-$name HEAVY_RUN=${HEAVY_RUN:-} bash tools/ai/integ-wh-build.sh "$c"
    }
    mk poison-pure tools/ai/integ-wh-gc-poison.patch || exit 1
    mk poison-hyb tools/ai/integ-wh-gc-hybrid.patch tools/ai/integ-wh-gc-poison.patch || exit 1 ;;
  groups)
    v=$1; b=$(bin_of "$v"); [ -x "$b" ] || { echo "no binary $b"; exit 2; }
    [ -x target/final-verify/checks.sh ] || { echo "target/final-verify/checks.sh is missing (the lane's check driver)"; exit 2; }
    s=$(date +%s)
    (ulimit -v 8388608; STANDALONE_CHECK_LOGDIR=$ROOT/target/final-verify/sc-$v bash target/final-verify/checks.sh "$b" "$b" "disc-$v") \
      > "$OUT/groups-$v.log" 2>&1
    rc=$?
    echo "GROUPS $v rc=$rc $(( $(date +%s) - s ))s $(tail -n 1 "$OUT/groups-$v.log")"
    grep -E 'STANDALONE-CHECK FAIL| rc=[1-9]' "$OUT/groups-$v.log" | cut -c1-160
    exit "$rc" ;;
  forced)
    v=$1; to=${2:-25}; first=${3:-}; b=$(bin_of "$v"); [ -x "$b" ] || { echo "no binary $b"; exit 2; }
    d=$OUT/forced-$v; [ -n "$first" ] || rm -rf "$d"; mkdir -p "$d"
    started=0; [ -z "$first" ] && started=1; n=0; bad=0
    for f in test/standalone-*-check.el test/perf/word-heap-vectorlike-check.el; do
      name=$(basename "$f" .el)
      if [ $started = 0 ]; then [ "$name" = "$first" ] && started=1 || continue; fi
      n=$((n + 1)); s=$(date +%s)
      (ulimit -v 8388608; timeout "$to" nice -n 10 "$b" --eval '(nelisp--debug-switch 70)' --load "$f" > "$d/$name.out" 2> "$d/$name.err")
      code=$?
      res=PASS
      if [ $code = 124 ]; then res=TIMEOUT
      elif [ $code -ge 128 ]; then res=CRASH
      elif [ $code != 0 ]; then res=FAIL
      elif grep -q '^nelisp: uncaught error:' "$d/$name.err"; then res=FAIL
      else
        exp=${f%.el}.expected
        if [ -f "$exp" ] && { [ -s "$d/$name.err" ] || ! cmp -s "$exp" "$d/$name.out"; }; then res=FAIL; fi
      fi
      if [ $res = FAIL ] && grep -q "^$f " test/perf/standalone-checks-known-red.txt; then res=KNOWNRED; fi
      case $res in FAIL|CRASH) bad=$((bad + 1)) ;; esac
      echo "$res $name rc=$code $(( $(date +%s) - s ))s"
    done
    echo "FORCED $v: $n tests, $bad failures (TIMEOUT is no verdict)"
    exit "$bad" ;;
  goldens)
    v=$1; b=$(bin_of "$v"); [ -x "$b" ] || { echo "no binary $b"; exit 2; }
    g=$OUT/gold; mkdir -p "$g"; bad=0
    names="no-props string-repr reader-labels raw-literal sanity"
    for n in $names; do
      [ -f "$g/$n.gnu" ] || (ulimit -v 8388608; LANG= LANGUAGE= timeout 300 "$GNU" -Q --batch -l "tools/ai/integ-wh-$n.el" > "$g/$n.gnu" 2> "$g/$n.gnu.err") ||
        { echo "GNU failed on $n"; exit 2; }
    done
    for mode in normal forced; do
      if [ $mode = forced ]; then pre=(--eval '(nelisp--debug-switch 70)'); else pre=(); fi
      for n in $names; do
        s=$(date +%s)
        (ulimit -v 8388608; timeout 900 "$b" "${pre[@]}" -l "tools/ai/integ-wh-$n.el" > "$g/$n.$v.$mode" 2> "$g/$n.$v.$mode.err")
        rc=$?
        if [ $rc = 0 ] && cmp -s "$g/$n.gnu" "$g/$n.$v.$mode" && [ ! -s "$g/$n.$v.$mode.err" ]; then r=SAME
        else r=DIFF; bad=$((bad + 1)); fi
        echo "GOLDEN $v $mode $n rc=$rc $r $(( $(date +%s) - s ))s"
      done
      s=$(date +%s)
      (ulimit -v 8388608; timeout 900 "$b" "${pre[@]}" -l tools/ai/integ-wh-gc-builders.el > "$g/builders.$v.$mode" 2> "$g/builders.$v.$mode.err")
      rc=$?
      last=$(tail -n 1 "$g/builders.$v.$mode")
      case $last in "GC-BUILDERS PASS "*) r=PASS ;; *) r=FAIL; bad=$((bad + 1)) ;; esac
      echo "GOLDEN $v $mode gc-builders rc=$rc $r ($last) $(( $(date +%s) - s ))s"
    done
    exit "$bad" ;;
  lib)
    v=$1; b=$(bin_of "$v"); [ -x "$b" ] || { echo "no binary $b"; exit 2; }
    [ -d "${NELISP_LIB:-}" ] && [ -d "${NELISP_LIB_FIXTURES:-}" ] || { echo "set NELISP_LIB and NELISP_LIB_FIXTURES"; exit 2; }
    L=$OUT/lib-$v; rm -rf "$L"; cp -a "$NELISP_LIB" "$L" || exit 2; rm -rf "$L"/build/c-core-image*
    mkdir -p "$OUT/lib-$v-home"; bad=0
    step() {  # step LABEL BUNDLE ACTION
      local s rc; s=$(date +%s)
      (cd "$L" && ulimit -v 8388608 &&
       env -u DISPLAY -u WAYLAND_DISPLAY -u NELISP_GUI_FAULT -u NELISP_GUI_TEST_EXIT_GROUP -u NELISP_GUI_FIXTURE \
         HOME="$OUT/lib-$v-home" GSETTINGS_BACKEND=memory \
         NELISP_GUI_VENDOR_FIXTURE="$NELISP_LIB_FIXTURES/build/gui-skk-evil-image/vendor" NELISP_GUI_FIXTURE_OUT="$OUT/lib-$v-gui" \
         NELISP_GUI_SKK_DICTIONARY="$NELISP_LIB_FIXTURES/packages/nelisp-gui-xcb/fixtures/SKK-JISYO.gui" \
         ${2:+C_CORE_IMAGE_BUNDLE="$L/build/$2"} C_CORE_IMAGE_BUILD_TIMEOUT=900 C_CORE_IMAGE_ALLOW_WARNINGS=1 NELISP_BIN="$b" \
         bash tools/c-core-image.sh "$3") > "$OUT/lib-$v.$1.$3.out" 2>&1
      rc=$?
      [ $rc = 0 ] || bad=$((bad + 1))
      echo "LIB $v $1 $3 rc=$rc $(( $(date +%s) - s ))s $(tail -n 1 "$OUT/lib-$v.$1.$3.out" | cut -c1-120)"
    }
    step base "" build; step base "" check
    step gui nemacs-gui-bootstrap.el build; step gui nemacs-gui-bootstrap.el check
    # The first SKK build ends in "I have created an empty SKK Jisyo file for you" on stderr, which the builder counts as a
    # failure; the dictionary exists afterwards and the second build is the real one.
    step skk nemacs-gui-skk-evil-bootstrap.el build
    if [ "$bad" != 0 ] && grep -q 'empty SKK Jisyo' "$L"/build/c-core-image-nemacs-gui-skk-evil-bootstrap/build.err 2>/dev/null; then
      bad=$((bad - 1)); rm -rf "$L"/build/c-core-image-nemacs-gui-skk-evil-bootstrap; step skk nemacs-gui-skk-evil-bootstrap.el build
    fi
    step skk nemacs-gui-skk-evil-bootstrap.el check
    exit "$bad" ;;
  *)
    sed -n '2,/^# nearest bin/p' "$0" | cut -c3-; exit 2 ;;
esac
