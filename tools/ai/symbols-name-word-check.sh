#!/usr/bin/env bash
# symbols-name-word-check.sh CHECK [ARGS]: the machine checks of tools/ai/symbols-name-word-progress.org.
#
# Binaries (defaults relative to the worktree root, the environment overrides):
#   SNW_DYN, SNW_STATIC            this branch's builds   [target/integ/nelisp-dyn, nelisp-static]
#   SNW_BASE_DYN, SNW_BASE_STATIC  the batch's E build of 63ec9640e's inputs   [target/base/nelisp-dyn, nelisp-static]
# GNU 31.1 is /usr/local/bin/emacs (GNU_EMACS overrides).  Every NeLisp and GNU run is capped at 8 GiB of address
# space.  Exit 0 iff the check passes; the last line says why not.
#
#   baseline            S0.1  the baseline binaries are pinned and equal 63ec9640e's build inputs
#   boundary            S0.2  no defun listed in symbols-name-word-boundary.txt differs from 63ec9640e
#   census-negative     S0.3  the census fails on the baseline (design B) and says how many legacy names it saw
#   writers, readers    S1.1, S2.1  the pinned enumerations of the name-cell writers and readers
#   census              S1.2  every producer path leaves a String WORD (dynamic and static)
#   heap                S1.3  the intern table and the symbol blocks hold no legacy name box
#   image               S1.4  the census across a dump, a restore, a second dump and its restore
#   emit x86_64|aarch64 S2.2, S2.3  the emitter tests (test/nelisp-aot-symbol-name-word-test.el)
#   codegen-ert         S2.4  the AOT compiler and codegen ERT suites (the batch's arm)
#   semantics           S2.5  the 89 symbols cases against GNU and the 227 evalcall cases
#   built               S3.1  the binaries name HEAD's tree and boot; the F1 evidence lines hold their bounds
#   golden, arms-strings, arms-image, precise, weakmark   S3.2 to S3.6  the batch's arms on both linkages
#   run UNIT, verify UNIT  the long units: scale-dyn, scale-static, checks, bench-reader, bench-intern,
#                          bench-startup, bench-gc3, bench-scale (run writes target/snw/logs/UNIT.log)
#   win-built, win-cold, win-smoke   S4
cd "$(dirname "$0")/../.." || exit 2
ROOT=$PWD
DYN=${SNW_DYN:-$ROOT/target/integ/nelisp-dyn}
STATIC=${SNW_STATIC:-$ROOT/target/integ/nelisp-static}
BASE_DYN=${SNW_BASE_DYN:-$ROOT/target/base/nelisp-dyn}
BASE_STATIC=${SNW_BASE_STATIC:-$ROOT/target/base/nelisp-static}
GNU=${GNU_EMACS:-/usr/local/bin/emacs}
OUT=$ROOT/target/snw
LOGS=$OUT/logs
mkdir -p "$OUT" "$LOGS"
export TMPDIR=$ROOT/target/tmp XDG_CACHE_HOME=$ROOT/target/xdg-cache
mkdir -p "$TMPDIR" "$XDG_CACHE_HOME"
BASE_REV=63ec9640e
E_REV=3ed3be627

say() { echo "SNW $*"; }
CHECK=${1:-}${2:+ $2}
fail() { say "FAIL: $*"; printf '%s [%s] %s\n' "$(date +%T)" "$CHECK" "$*" >> "$LOGS/fail.log" 2>/dev/null; exit 1; }
capped() { (ulimit -v 8388608; exec "$@"); }
need() { [ -x "$1" ] || fail "missing binary $1 ($2)"; }
sha() { sha256sum "$1" | cut -d' ' -f1; }
# Each linkage in turn: "dyn" and "static" with their binaries.
linkages() { printf '%s\n' "dyn $DYN" "static $STATIC"; }
# The sha256 recorded for a log header.
bin_header() { printf 'SNW-LOG unit=%s dyn=%s static=%s base_dyn=%s base_static=%s tree=%s\n' "$1" \
  "$(sha "$DYN" 2>/dev/null)" "$(sha "$STATIC" 2>/dev/null)" "$(sha "$BASE_DYN" 2>/dev/null)" "$(sha "$BASE_STATIC" 2>/dev/null)" \
  "$(build_tree_hash)"; }
# A hash of the build inputs of the working tree: what a build of it would be made from.
build_tree_hash() {
  git ls-files -z lisp scripts src templates vendor packages standalone-compat nelisp-emacs-lib Makefile 2>/dev/null |
    sort -z | xargs -0 sha256sum 2>/dev/null | sha256sum | cut -c1-16
}
# The binary was built from HEAD's tree: its --version names a commit whose build inputs equal HEAD's.
built_from_head() {  # built_from_head BIN -> prints rev, status 0 iff the inputs equal
  local v rev
  v=$(capped "$1" --version 2>/dev/null | head -1)
  rev=$(printf '%s\n' "$v" | sed -n 's/.*-g\([0-9a-f]\{9\}\)[ )-].*/\1/p')
  [ -n "$rev" ] || return 1
  echo "$rev"
  git diff --quiet "$rev" HEAD -- lisp scripts src templates vendor packages standalone-compat nelisp-emacs-lib Makefile
}

# ------------------------------------------------------------------------------------------ S0
BUILD_INPUTS="lisp scripts src templates vendor packages standalone-compat nelisp-emacs-lib Makefile"
PIN_BASE_DYN=dd6c5eaed217c091c33cf37fe145d4ba1c601fedbb55c9880f760bac7bfaef79
PIN_BASE_STATIC=492d1cd14ee2b55dba90a60632ba27ab5550a37fe5952879bacfca0c162746e5
PIN_BASE_DYN_COLD=402bba6c8afc8b842d8c0a3c23737970e3a6d950eb1a93bb66aaa74742ac97c6
PIN_BASE_STATIC_COLD=01ed99d471369451198abb4a7d581fd8a76116d77bf3eff53729d0cc82de6834

case ${1:-} in
  baseline)
    git diff --quiet $E_REV $BASE_REV -- $BUILD_INPUTS || fail "$E_REV and $BASE_REV differ in build inputs: $(git diff --stat $E_REV $BASE_REV -- $BUILD_INPUTS | tail -n 1)"
    for f in "$BASE_DYN:$PIN_BASE_DYN" "$BASE_STATIC:$PIN_BASE_STATIC" "$BASE_DYN.cold:$PIN_BASE_DYN_COLD" "$BASE_STATIC.cold:$PIN_BASE_STATIC_COLD"; do
      p=${f%%:*}; want=${f##*:}
      [ -f "$p" ] || fail "missing baseline file $p"
      [ "$(sha "$p")" = "$want" ] || fail "$p is not the pinned E build (sha256 $(sha "$p" | cut -c1-12), pinned ${want:0:12})"
    done
    for b in "$BASE_DYN" "$BASE_STATIC"; do
      v=$(capped "$b" --version 2>/dev/null | head -1)
      case $v in *"$E_REV"*) ;; *) fail "$b names '$v', not $E_REV" ;; esac
    done
    say "PASS baseline (E build of $E_REV, build inputs equal to $BASE_REV's; dynamic ${PIN_BASE_DYN:0:8}, static ${PIN_BASE_STATIC:0:8})" ;;
  boundary)
    bash tools/ai/symbols-name-word-defuns.sh "$BASE_REV" tools/ai/symbols-name-word-boundary.txt > "$LOGS/boundary.out" 2>&1 ||
      fail "boundary: $(tail -n 3 "$LOGS/boundary.out" | tr '\n' '|' | cut -c1-300)"
    say "PASS boundary ($(tail -n 1 "$LOGS/boundary.out"))" ;;
  census-negative)
    need "$BASE_DYN" "S0.1"
    capped env LC_ALL=C.UTF-8 SNW_MODE=producers SNW_BASELINE=1 timeout 280 "$BASE_DYN" --load tools/ai/symbols-name-word-census.el \
      > "$LOGS/census-negative.out" 2> "$LOGS/census-negative.err"
    rc=$?
    last=$(tail -n 1 "$LOGS/census-negative.out")
    [ $rc != 0 ] || fail "the census passes on the baseline (design B): it cannot see a legacy name box"
    legacy=$(printf '%s\n' "$last" | sed -n 's/.* legacy=\([0-9]*\) .*/\1/p')
    [ -n "$legacy" ] && [ "$legacy" -ge 10 ] || fail "the census failed on the baseline but saw only '${legacy:-?}' legacy names: $last"
    say "PASS census-negative (the baseline fails with $legacy legacy name boxes among its producer paths: $last)" ;;

  # -------------------------------------------------------------------------------------- S1
  writers|readers)
    "$GNU" -Q --batch --version > /dev/null 2>&1 || fail "no GNU Emacs at $GNU"
    capped timeout 280 "$GNU" -Q --batch -l tools/ai/symbols-name-word-scan.el -- "$1" > "$LOGS/scan-$1.out" 2>&1 ||
      fail "$1: $(tail -n 4 "$LOGS/scan-$1.out" | tr '\n' '|' | cut -c1-400)"
    say "PASS $1 ($(tail -n 1 "$LOGS/scan-$1.out"))" ;;
  census)
    capped env LC_ALL=C.UTF-8 LANG= LANGUAGE= SNW_MODE=producers timeout 120 "$GNU" -Q --batch -l tools/ai/symbols-name-word-census.el \
      > "$OUT/census.gnu.out" 2> "$OUT/census.gnu.err"
    grep '^SEM ' "$OUT/census.gnu.out" > "$OUT/census.gnu.sem"
    [ -s "$OUT/census.gnu.sem" ] || fail "GNU printed no SEM lines"
    summary=""
    while read -r label bin; do
      need "$bin" "$label"
      capped env LC_ALL=C.UTF-8 SNW_MODE=producers timeout 280 "$bin" --load tools/ai/symbols-name-word-census.el \
        > "$OUT/census.$label.out" 2> "$OUT/census.$label.err"
      rc=$?
      last=$(tail -n 1 "$OUT/census.$label.out")
      [ $rc = 0 ] && case $last in *" PASS") true ;; *) false ;; esac ||
        fail "$label census rc=$rc: $(grep -m3 '^REP FAIL' "$OUT/census.$label.out" | cut -c1-150 | tr '\n' '|') $last"
      [ -s "$OUT/census.$label.err" ] && fail "$label census wrote to stderr: $(head -c 160 "$OUT/census.$label.err")"
      grep '^SEM ' "$OUT/census.$label.out" > "$OUT/census.$label.sem"
      cmp -s "$OUT/census.gnu.sem" "$OUT/census.$label.sem" ||
        fail "$label: SEM lines differ from GNU 31.1: $(diff "$OUT/census.gnu.sem" "$OUT/census.$label.sem" | grep '^[<>]' | head -n 2 | cut -c1-140 | tr '\n' '|')"
      summary="$summary $label: ${last#SNW-CENSUS }"
    done < <(linkages)
    say "PASS census (SEM lines equal to GNU 31.1;$summary)" ;;
  heap)
    summary=""
    while read -r label bin; do
      need "$bin" "$label"
      capped env LC_ALL=C.UTF-8 SNW_MODE=heap timeout 280 "$bin" --load tools/ai/symbols-name-word-census.el \
        > "$OUT/heap.$label.out" 2> "$OUT/heap.$label.err"
      rc=$?
      last=$(tail -n 1 "$OUT/heap.$label.out")
      [ $rc = 0 ] && case $last in *" PASS") true ;; *) false ;; esac ||
        fail "$label heap census rc=$rc: $(grep -m3 '^REP FAIL' "$OUT/heap.$label.out" | cut -c1-200 | tr '\n' '|') $last"
      summary="$summary $label: $(grep '^REP churn' "$OUT/heap.$label.out" | cut -c1-120)"
    done < <(linkages)
    say "PASS heap ($summary)" ;;
  image)
    summary=""
    while read -r label bin; do
      need "$bin" "$label"
      w=$OUT/image-$label; rm -rf "$w"; mkdir -p "$w"
      step() {  # step MODE IMAGE_OUT [extra args...]
        local mode=$1 img=$2; shift 2
        capped env LC_ALL=C.UTF-8 SNW_MODE=$mode SNW_IMAGE=$img timeout 280 "$bin" "$@" --load tools/ai/symbols-name-word-census.el \
          > "$w/$mode.out" 2> "$w/$mode.err"
        local rc=$?
        last=$(tail -n 1 "$w/$mode.out")
        [ $rc = 0 ] && case $last in *" PASS") true ;; *) false ;; esac ||
          fail "$label $mode rc=$rc: $(grep -m3 '^REP FAIL' "$w/$mode.out" | cut -c1-200 | tr '\n' '|') $last"
        [ -s "$w/$mode.err" ] && fail "$label $mode wrote to stderr: $(head -c 160 "$w/$mode.err")"
      }
      step dump "$w/g1.img"
      step verify "" --cold-load-from "$w/g1.img"
      step dump2 "$w/g2.img" --cold-load-from "$w/g1.img"
      step verify2 "" --cold-load-from "$w/g2.img"
      summary="$summary $label: $(for m in dump verify dump2 verify2; do tail -n 1 "$w/$m.out" | sed 's/SNW-CENSUS mode=//; s/ legacy=.*//'; done | tr '\n' ',')"
    done < <(linkages)
    say "PASS image ($summary)" ;;

  # -------------------------------------------------------------------------------------- S2
  emit)
    arch=${2:-}
    case $arch in x86_64|aarch64) ;; *) fail "usage: emit x86_64|aarch64" ;; esac
    [ -f test/nelisp-aot-symbol-name-word-test.el ] || fail "missing test/nelisp-aot-symbol-name-word-test.el"
    capped timeout 290 "$GNU" --batch -Q -L lisp -L src -L scripts -L test --eval '(setq load-prefer-newer t)' \
      -l ert -l test/nelisp-aot-symbol-name-word-test.el --eval "(ert-run-tests-batch-and-exit \"^nelisp-snw-$arch/\")" \
      > "$LOGS/emit-$arch.out" 2>&1
    rc=$?
    ran=$(sed -n 's/^Ran \([0-9]*\) tests, \([0-9]*\) results as expected, \([0-9]*\) unexpected.*/\1 \2 \3/p' "$LOGS/emit-$arch.out" | tail -n 1)
    set -- $ran
    [ -n "${1:-}" ] || fail "emit $arch: no ERT summary: $(tail -n 3 "$LOGS/emit-$arch.out" | tr '\n' '|')"
    [ "$3" = 0 ] && [ "$rc" = 0 ] || fail "emit $arch: $3 unexpected of $1: $(grep -E '^ +(FAILED|SKIPPED)' "$LOGS/emit-$arch.out" | head -n 4 | tr '\n' '|')"
    grep -q '^ *SKIPPED' "$LOGS/emit-$arch.out" && fail "emit $arch: a test was skipped: $(grep '^ *SKIPPED' "$LOGS/emit-$arch.out" | head -n 2 | tr '\n' '|')"
    [ "$1" -ge 8 ] || fail "emit $arch: only $1 tests ran (at least 8 are defined)"
    say "PASS emit $arch ($1 tests)" ;;
  codegen-ert)
    out=$(bash tools/ai/integ-wh-check.sh codegen-ert 2>&1) || fail "codegen-ert: $(printf '%s\n' "$out" | tail -n 1 | cut -c1-300)"
    say "PASS codegen-ert ($(printf '%s\n' "$out" | tail -n 1 | sed 's/^INTEG-WH PASS codegen-ert //'))" ;;
  semantics)
    [ -f test/standalone-symbols-check.el ] && [ -f test/standalone-symbols-check.expected ] || fail "missing the semantics fixture"
    exp=test/standalone-symbols-check.expected
    capped env LC_ALL=C.UTF-8 timeout 120 "$GNU" -Q --batch -l test/standalone-symbols-check.el 2> /dev/null | grep '^SYM' > "$OUT/semantics.gnu"
    cmp -s "$OUT/semantics.gnu" "$exp" || fail "GNU 31.1's own output differs from $exp"
    n=$(wc -l < "$exp")
    summary="GNU = $n lines"
    while read -r label bin; do
      need "$bin" "$label"
      capped env LC_ALL=C.UTF-8 timeout 280 "$bin" --load test/standalone-symbols-check.el 2> "$OUT/semantics.$label.err" | grep '^SYM' > "$OUT/semantics.$label"
      cmp -s "$OUT/semantics.$label" "$exp" ||
        fail "$label: the symbols fixture differs from GNU 31.1 in $(diff "$OUT/semantics.$label" "$exp" | grep -c '^[<>]') lines: $(diff "$OUT/semantics.$label" "$exp" | grep '^[<>]' | head -n 2 | cut -c1-120 | tr '\n' '|')"
      summary="$summary; $label equal"
      out=$(capped timeout 290 python3 tools/ai/callpath-perf-check.py evalcall --binary "$bin" 2>&1) ||
        fail "$label evalcall: $(printf '%s\n' "$out" | tail -n 1 | cut -c1-200)"
      case $out in *"evalcall PASS 227 cases"*) summary="$summary, evalcall 227" ;; *) fail "$label evalcall: $(printf '%s\n' "$out" | tail -n 1 | cut -c1-200)" ;; esac
    done < <(linkages)
    say "PASS semantics ($summary)" ;;

  # -------------------------------------------------------------------------------------- S3
  built)
    summary=""
    while read -r label bin; do
      need "$bin" "$label"
      rev=$(built_from_head "$bin") || fail "$label: '$(capped "$bin" --version 2>/dev/null | head -1)' is not built from HEAD's tree"
      v=$(capped "$bin" --version 2>/dev/null | head -1)
      case $label:$v in dyn:*", dynamic)"*|static:*", static)"*) ;; *) fail "$label: '$v' has the wrong linkage" ;; esac
      out=$(capped env BATCH_F_DYN="$bin" BATCH_F_STATIC="$bin" bash tools/ai/batch-f-check.sh boot 2>&1) || fail "$label boot: $out"
      summary="$summary $label $rev"
    done < <(linkages)
    # The F1 prover's figures, when the build recorded them (NELISP_ROOT_EVIDENCE_REPORT=1): inside the policy.
    for f in "$LOGS"/build-dyn.out "$LOGS"/build-static.out; do
      [ -f "$f" ] || continue
      n=$(sed -n 's/.*\[root-evidence\] nodes=\([0-9]*\).*/\1/p' "$f" | sort -n | tail -n 1)
      [ -z "$n" ] || [ "$n" -le 81920 ] || fail "$f: root-evidence nodes $n above the bound 81920"
      summary="$summary ${f##*/}:nodes=${n:-?}"
    done
    say "PASS built (HEAD's tree, booted:$summary)" ;;
  golden)
    summary=""
    while read -r label bin; do
      need "$bin" "$label"
      out=$(IG_WORK=$OUT/golden-$label bash tools/ai/integ-wh-image-golden.sh "$bin" 2>&1) || fail "$label golden: $(printf '%s\n' "$out" | tail -n 2 | tr '\n' '|' | cut -c1-300)"
      summary="$summary $label: $(printf '%s\n' "$out" | tail -n 1)"
    done < <(linkages)
    say "PASS golden ($summary)" ;;
  arms-strings|arms-image|precise|weakmark)
    case $1 in
      arms-strings) arms="reader-labels string-repr raw-literal" ;;
      arms-image)   arms="view-image n4" ;;
      precise)      arms="precise" ;;
      weakmark)     arms="weakmark" ;;
    esac
    summary=""
    while read -r label bin; do
      need "$bin" "$label"
      for a in $arms; do
        out=$(INTEG_WH_DYN="$bin" VIEW_IMAGE_WORK=$OUT/view-image-$label bash tools/ai/integ-wh-check.sh "$a" 2>&1) ||
          fail "$label $a: $(printf '%s\n' "$out" | tail -n 1 | cut -c1-300)"
        summary="$summary $label/$a"
      done
    done < <(linkages)
    say "PASS $1 ($summary)" ;;

  # -------------------------------------------------------------------------------------- units
  run)
    unit=${2:-}
    log=$LOGS/$unit.log
    s=$(date +%s)
    ( bin_header "$unit"
      case $unit in
        scale-dyn)    need "$DYN" dyn;       IG_WORK=$OUT/scale-dyn bash tools/ai/integ-wh-image-scale.sh "$DYN" ;;
        scale-static) need "$STATIC" static; IG_WORK=$OUT/scale-static bash tools/ai/integ-wh-image-scale.sh "$STATIC" ;;
        scale-base-dyn)    need "$BASE_DYN" base;       IG_WORK=$OUT/scale-base-dyn bash tools/ai/integ-wh-image-scale.sh "$BASE_DYN" ;;
        scale-base-static) need "$BASE_STATIC" base;    IG_WORK=$OUT/scale-base-static bash tools/ai/integ-wh-image-scale.sh "$BASE_STATIC" ;;
        checks)       need "$DYN" dyn; need "$STATIC" static
                      env LC_ALL=C.UTF-8 bash tools/ai/symbols-name-word-checks.sh "$DYN" "$STATIC" snw ;;
        checks-base)  need "$BASE_DYN" base; need "$BASE_STATIC" base
                      env LC_ALL=C.UTF-8 bash tools/ai/symbols-name-word-checks.sh "$BASE_DYN" "$BASE_STATIC" snw-base ;;
        bench-*)      python3 tools/ai/symbols-name-word-bench.py "${unit#bench-}" ;;
        *) echo "unknown unit $unit"; false ;;
      esac
      rc=$?
      echo "SNW-UNIT $unit rc=$rc $(( $(date +%s) - s ))s"
      [ $rc = 0 ] && echo "SNW-UNIT $unit PASS" || echo "SNW-UNIT $unit FAIL"
      exit $rc
    ) > "$log" 2>&1
    rc=$?
    tail -n 2 "$log"
    exit $rc ;;
  verify)
    unit=${2:-}
    log=$LOGS/$unit.log
    case $unit in
      bench-*)
        python3 tools/ai/symbols-name-word-bench.py verify "${unit#bench-}" > "$LOGS/$unit.verify.out" 2>&1 ||
          fail "$unit: $(tail -n 4 "$LOGS/$unit.verify.out" | tr '\n' '|' | cut -c1-400)"
        say "PASS verify $unit ($(grep -c ' ok' "$LOGS/$unit.verify.out") bounds met)"; exit 0 ;;
    esac
    [ -f "$log" ] || fail "no log $log (run: symbols-name-word-check.sh run $unit)"
    tail -n 1 "$log" | grep -qx "SNW-UNIT $unit PASS" || fail "$unit: the log does not end in PASS: $(tail -n 2 "$log" | tr '\n' '|' | cut -c1-200)"
    head -n 1 "$log" | grep -q '^SNW-LOG ' || fail "$unit: the log has no header"
    case $unit in
      scale-base-*|checks-base) want_dyn=$(sha "$BASE_DYN"); want_static=$(sha "$BASE_STATIC") ;;
      *) want_dyn=$(sha "$DYN"); want_static=$(sha "$STATIC") ;;
    esac
    hd=$(head -n 1 "$log")
    case $hd in *" dyn=$want_dyn "*" static=$want_static "*) ;; *) fail "$unit: the log was made with other binaries than the ones now in place" ;; esac
    case $unit in
      scale-base-*|checks-base) ;;
      *) for pair in "dyn $DYN" "static $STATIC"; do set -- $pair; built_from_head "$2" > /dev/null || fail "$unit: the $1 binary is not built from HEAD's tree"; done ;;
    esac
    say "PASS verify $unit ($(tail -n 2 "$log" | head -n 1))" ;;

  # -------------------------------------------------------------------------------------- S4
  win-built|win-cold|win-smoke)
    d=${SNW_WIN:-$ROOT/target/win/HEAD}
    [ -d "$d" ] || fail "no Windows build directory $d (tools/ai/win-smoke.sh build HEAD $d)"
    c=$(cut -c1-9 "$d/commit" 2>/dev/null)
    [ -n "$c" ] || fail "no commit file in $d"
    case $1 in
      win-built)
        git diff --quiet "$c" HEAD -- $BUILD_INPUTS || fail "the Windows build ($c) is not HEAD's tree"
        [ -s "$d/nelisp.exe" ] || fail "no exe in $d"
        say "PASS win-built ($c, exe $(sha "$d/nelisp.exe" | cut -c1-16))" ;;
      win-cold)
        [ -s "$d/nelisp.exe.cold" ] || fail "no cold image beside the exe"
        grep -q 'cold image -> .*(\([0-9]*\) bytes)' "$d/build.log" || fail "the build log has no cold-image line"
        say "PASS win-cold ($(stat -c %s "$d/nelisp.exe.cold") B, $(grep -o 'cold image -> .*' "$d/build.log" | tail -n 1 | grep -o '([0-9]* bytes)'))" ;;
      win-smoke)
        [ -f "$d/smoke.log" ] || fail "no smoke log in $d"
        tail -n 1 "$d/smoke.log" | grep -qx 'WIN-SMOKE PASS' || fail "$(tail -n 1 "$d/smoke.log")"
        git diff --quiet "$c" HEAD -- $BUILD_INPUTS || fail "the smoke ran on $c, not on HEAD's tree"
        say "PASS win-smoke ($c, $(grep -c '^PASS' "$d/smoke.log") PASS lines, WIN-SMOKE PASS)" ;;
    esac ;;
  *) echo "usage: $0 CHECK [ARGS]  (see the header)" >&2; exit 2 ;;
esac
