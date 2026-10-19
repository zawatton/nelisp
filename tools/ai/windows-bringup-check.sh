#!/bin/sh
# Acceptance entry points for windows-bringup-progress.org, from any cwd.
set -eu
cd "$(dirname "$0")/../.."
mkdir -p target/codex
case "${1:-}" in
S1|S2|S3|S4|S5|run-S4|wine-source|wine-cold)
  export WINEDEBUG=-all
  export WINEPREFIX="${WINEPREFIX:-$PWD/target/wine-prefix}"
  export TMPDIR="$PWD/target/tmp"
  mkdir -p "$TMPDIR"
  ;;
native) ;;
*) echo 'usage: windows-bringup-check.sh S1|S2|S3|S4|S5|run-S4|native|wine-source|wine-cold' >&2; exit 2 ;;
esac
case "$1" in
wine-source)
  shift
  exec wine target/nelisp.exe "$@"
  ;;
wine-cold)
  shift
  exec wine target/nelisp.exe --cold-load-from target/nelisp.exe.cold "$@"
  ;;
S1)
  timeout 90 wine target/nelisp.exe --eval '(+ 1 2)' >target/codex/s1-add.out 2>target/codex/s1-add.err
  grep -qx 3 target/codex/s1-add.out
  timeout 90 wine target/nelisp.exe --eval nil >target/codex/s1-nil.out 2>target/codex/s1-nil.err
  ;;
S2)
  test -s target/nelisp.exe.cold
  test -s target/codex/build-windows.log
  ! grep -Ei 'cold image dump failed|cold image skipped|error:|make:.*Error' target/codex/build-windows.log
  sha256sum -c target/codex/s2.sha256 >/dev/null
  ;;
S3)
  timeout 90 wine target/nelisp.exe --cold-load-from target/nelisp.exe.cold --eval '(+ 1 2)' >target/codex/s3-add.out 2>target/codex/s3-add.err
  grep -qx 3 target/codex/s3-add.out
  timeout 90 wine target/nelisp.exe --cold-load-from target/nelisp.exe.cold --load test/standalone-exit-stash-root-check.el >target/codex/s3-exit.out 2>target/codex/s3-exit.err
  timeout 90 wine target/nelisp.exe --cold-load-from target/nelisp.exe.cold --eval "(progn (require 'rx) (princ (if (string-match-p (rx (+ digit)) \"123\") \"RX-OK\" \"RX-FAIL\")))" >target/codex/s3-rx.out 2>target/codex/s3-rx.err
  grep -q RX-OK target/codex/s3-rx.out
  ;;
S4|S5)
  # Expensive suites run once; the meter revalidates their complete receipt.
  test "$(cat "target/codex/$1.status")" = PASS
  sha256sum -c "target/codex/$1.sha256" >/dev/null
  ;;
run-S4)
  # Sequential modes and distinct directories keep every test's evidence intact.
  rm -f target/codex/S4.status
  for mode in source cold; do
    wrapper="target/codex/wine-$mode.sh"
    cat >"$wrapper" <<EOF
#!/bin/sh
exec sh tools/ai/windows-bringup-check.sh wine-$mode "\$@"
EOF
    chmod +x "$wrapper"
    EVAL_BIN="$PWD/$wrapper" STANDALONE_CHECK_LOGDIR="target/codex/wine-$mode-checks" \
      sh test/perf/standalone-checks.sh >"target/codex/wine-$mode-checks.log" 2>&1
    cat "target/codex/wine-$mode-checks.log"
  done
  sha256sum target/nelisp.exe target/nelisp.exe.cold \
    scripts/nelisp-standalone-build.el tools/ai/windows-bringup-check.sh \
    test/perf/standalone-checks.sh test/perf/standalone-checks-known-red.txt \
    test/standalone-*-check.el test/bytecode-word-corpus.eld \
    target/codex/wine-source.sh target/codex/wine-cold.sh \
    target/codex/wine-source-checks.log target/codex/wine-cold-checks.log \
    target/codex/wine-source-checks/* target/codex/wine-cold-checks/* \
    >target/codex/S4.sha256
  # Include lazy providers and fixtures, so later source edits invalidate it.
  git ls-files -z '*.el' '*.eld' '*.py' '*.sh' | xargs -0 sha256sum >>target/codex/S4.sha256
  echo PASS >target/codex/S4.status
  ;;
native)
  # Run in MSYS2 on Windows. Keep S6 pending until the coordinator records it.
  mkdir -p target/tmp
  export TMPDIR="$PWD/target/tmp"
  # Shipping images contain the post-prelude heap; optional compiler libraries
  # must remain lazy. Compiler-prepared images are a separate development mode.
  NELISP_STANDALONE_NATIVE_COMPILER_COLD=0 NELISP_STANDALONE_COLD_IMAGE=1 make standalone-reader NELISP_STANDALONE_TARGET=windows-x86_64
  test -s target/nelisp.exe.cold
  target/nelisp.exe --eval '(+ 1 2)' >target/codex/native-add.out
  grep -qx 3 target/codex/native-add.out
  target/nelisp.exe --eval nil
  python3 tools/ai/windows-cold-auto-check.py native
  target/nelisp.exe --eval "(progn (require 'rx) (princ (if (string-match-p (rx (+ digit)) \"123\") \"RX-OK\" \"RX-FAIL\")))" >target/codex/native-plain-rx.out
  grep -q RX-OK target/codex/native-plain-rx.out
  target/nelisp.exe --cold-load-from target/nelisp.exe.cold --eval '(+ 1 2)' >target/codex/native-cold-add.out
  grep -qx 3 target/codex/native-cold-add.out
  target/nelisp.exe --cold-load-from target/nelisp.exe.cold --load test/standalone-exit-stash-root-check.el
  target/nelisp.exe --cold-load-from target/nelisp.exe.cold --load test/standalone-boot-libraries-check.el
  target/nelisp.exe --cold-load-from target/nelisp.exe.cold --eval "(progn (require 'rx) (princ (if (string-match-p (rx (+ digit)) \"123\") \"RX-OK\" \"RX-FAIL\")))" >target/codex/native-rx.out
  grep -q RX-OK target/codex/native-rx.out
  echo NATIVE-WINDOWS-S1-S3-PASS
  echo NATIVE-WINDOWS-W6-AUTO-COLD-PASS
  ;;
esac
