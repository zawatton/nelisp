#!/bin/sh
# Acceptance entry points for windows-bringup-progress.org, from any cwd.
set -eu
cd "$(dirname "$0")/../.."
mkdir -p target/codex
case "${1:-}" in
S1|S2|S3|S4|S5)
  export WINEDEBUG=-all
  export WINEPREFIX="${WINEPREFIX:-$PWD/target/wine-prefix}"
  ;;
native) ;;
*) echo 'usage: windows-bringup-check.sh S1|S2|S3|S4|S5|native' >&2; exit 2 ;;
esac
case "$1" in
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
native)
  # Run in MSYS2 on Windows. Keep S6 pending until the coordinator records it.
  mkdir -p target/tmp
  export TMPDIR="$PWD/target/tmp"
  NELISP_STANDALONE_NATIVE_COMPILER_COLD=1 make standalone-reader NELISP_STANDALONE_TARGET=windows-x86_64
  test -s target/nelisp.exe.cold
  target/nelisp.exe --eval '(+ 1 2)' >target/codex/native-add.out
  grep -qx 3 target/codex/native-add.out
  target/nelisp.exe --eval nil
  target/nelisp.exe --cold-load-from target/nelisp.exe.cold --eval '(+ 1 2)' >target/codex/native-cold-add.out
  grep -qx 3 target/codex/native-cold-add.out
  target/nelisp.exe --cold-load-from target/nelisp.exe.cold --load test/standalone-exit-stash-root-check.el
  target/nelisp.exe --cold-load-from target/nelisp.exe.cold --eval "(progn (require 'rx) (princ (if (string-match-p (rx (+ digit)) \"123\") \"RX-OK\" \"RX-FAIL\")))" >target/codex/native-rx.out
  grep -q RX-OK target/codex/native-rx.out
  echo NATIVE-WINDOWS-S1-S3-PASS
  ;;
esac
