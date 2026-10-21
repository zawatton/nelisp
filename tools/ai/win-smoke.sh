#!/usr/bin/env bash
# win-smoke.sh build REV OUT | smoke OUT | all REV OUT
#
# The word-heap line's Windows recipe: a Win64 PE built from a git-archive
# snapshot of REV (so edits in flight in the worktree cannot leak into the
# build), its cold image dumped under wine, then the wine smoke.  Run each
# phase as its own heavy unit:
#   bin/heavy-run.sh tools/ai/win-smoke.sh build HEAD target/win/HEAD
#   bin/heavy-run.sh tools/ai/win-smoke.sh smoke target/win/HEAD
# REV is any commit of the repository this script is run from.  OUT/src is
# the snapshot, OUT/nelisp.exe the PE with OUT/nelisp.exe.cold beside it,
# OUT/build.log the build log (its "[standalone-reader] cold image -> ...
# (N bytes)" line is the cold dump), OUT/smoke.log the smoke summary, last
# line "WIN-SMOKE PASS" or "WIN-SMOKE FAIL n".
#
# Environment (defaults in brackets):
#   WINEPREFIX          [/tmp/madblack-21-wine/version], reused, never created
#   NELISP_WINE_DLLS    directory of the DLLs copied beside the PE
#                       (vcruntime140.dll, vcruntime140_1.dll, winsqlite3.dll)
#   NELISP_WIN_SOAK     closure soak source [abi-integ-20261019's closure-soak.el]
# Every wine run and the build run under ulimit -v 8388608.  The build stops
# when less than 50 GB is free.
set -u
phase=${1:-}; shift || true
root=$(git rev-parse --show-toplevel) || exit 2
DLLS=${NELISP_WINE_DLLS:-/home/madblack-21/.cache/tmp/claude-1000/-home-madblack-21-Cowork-Notes/4943f038-0daf-45f9-bd77-7992d2ea1a08/scratchpad/wine-dlls}
SOAK=${NELISP_WIN_SOAK:-/home/madblack-21/Cowork/Notes/dev/nelisp/target/agent-worktrees/abi-integ-20261019/target/final-verify/closure-soak.el}
export WINEPREFIX=${WINEPREFIX:-/tmp/madblack-21-wine/version} WINEDEBUG=-all LC_ALL=C

build() {
  local rev=$1 out; out=$(realpath -m "$2")
  local avail; avail=$(df --output=avail -BG / | tail -1 | tr -dc 0-9)
  [ "$avail" -ge 50 ] || { echo "DISK LOW: ${avail}G" >&2; return 3; }
  [ -d "$WINEPREFIX" ] || { echo "no WINEPREFIX $WINEPREFIX" >&2; return 2; }
  local commit; commit=$(git -C "$root" rev-parse --verify "$rev^{commit}") || return 2
  rm -rf "$out/src" && mkdir -p "$out/src" || return 2
  git -C "$root" archive --format=tar "$commit" | tar -x -C "$out/src" || return 2
  echo "$commit" > "$out/commit"
  cp "$DLLS"/*.dll "$out"/ || return 2
  mkdir -p "$out/src/target/tmp" "$out/src/target/xdg-cache"
  local s; s=$(date +%s)
  ( cd "$out/src" || exit 2
    export TMPDIR=$out/src/target/tmp XDG_CACHE_HOME=$out/src/target/xdg-cache
    ulimit -v 8388608
    exec timeout 3000 env NELISP_STANDALONE_TARGET=windows-x86_64 \
      NELISP_STANDALONE_NATIVE_COMPILER_COLD=0 NELISP_STANDALONE_COLD_IMAGE=1 \
      NELISP_STANDALONE_COLD_RUNNER=wine NELISP_STANDALONE_READER_OUTPUT="$out/nelisp.exe" \
      /usr/local/bin/emacs --batch -Q -L lisp -L src -L scripts \
      --eval '(setq load-prefer-newer t)' \
      -l nelisp-standalone-build -f nelisp-standalone-build-reader ) > "$out/build.log" 2>&1
  local rc=$?
  echo "win build ${commit:0:9} rc=$rc $(( $(date +%s) - s ))s exe=$(sha256sum "$out/nelisp.exe" 2>/dev/null | cut -c1-16) cold=$(stat -c %s "$out/nelisp.exe.cold" 2>/dev/null || echo none)" | tee "$out/build.summary"
  [ "$rc" -eq 0 ] && [ -s "$out/nelisp.exe.cold" ]
}

smoke() {
  local out; out=$(realpath "$1") || return 2
  local log=$out/smoke.log fails=0 r line
  : > "$log"
  say() { echo "$*" | tee -a "$log"; }
  [ -s "$out/nelisp.exe.cold" ] || { say "no cold image"; say "WIN-SMOKE FAIL 1"; return 1; }
  local src=$out/src
  export TMPDIR=$src/target/tmp XDG_CACHE_HOME=$src/target/xdg-cache
  ( cd "$src" && ulimit -v 8388608 && timeout 300 wine "$out/nelisp.exe" --eval '(+ 1 2)' ) > "$out/eval.out" 2>&1
  r=$?; line=$(tr -d '\r' < "$out/eval.out" | tail -n 1)
  if [ "$r" -eq 0 ] && [ "$line" = 3 ]; then say "PASS eval (+ 1 2) = 3"; else say "FAIL eval rc=$r out=${line:0:60}"; fails=$((fails+1)); fi
  for t in standalone-bytecode-word-check standalone-printer-load-check \
           standalone-backtrace-locals-check standalone-unibyte-aset-check \
           standalone-sqlite-owned-check standalone-binding-parity-check; do
    if [ ! -f "$src/test/$t.el" ]; then say "SKIP $t (not on this line)"; continue; fi
    ( cd "$src" && ulimit -v 8388608 && timeout 900 wine "$out/nelisp.exe" --load "test/$t.el" ) > "$out/$t.out" 2>&1
    r=$?; line=$(tr -d '\r' < "$out/$t.out" | tail -n 1)
    if [ -f "$src/test/$t.expected" ]; then
      if [ "$r" -eq 0 ] && tr -d '\r' < "$out/$t.out" | cmp -s "$src/test/$t.expected" -; then
        say "PASS $t (== expected)"; else say "FAIL $t rc=$r (differs from expected)"; fails=$((fails+1)); fi
    elif [ "$r" -eq 0 ]; then say "PASS $t ${line:0:70}"
    else say "FAIL $t rc=$r ${line:0:70}"; fails=$((fails+1)); fi
  done
  ( cd "$src" && ulimit -v 8388608 && timeout 900 wine "$out/nelisp.exe" --load packages/nl-ffi/test/nl-ffi-memory-platform-smoke.el ) > "$out/ffi.out" 2>&1
  r=$?; line=$(tr -d '\r' < "$out/ffi.out" | grep -o 'FFI-MEMORY[^"]*' | head -1)
  if [ "$r" -eq 0 ] && [[ "$line" == "FFI-MEMORY: PASS"* ]]; then say "PASS ffi-memory ${line:0:40}"; else say "FAIL ffi-memory rc=$r ${line:0:60}"; fails=$((fails+1)); fi
  if [ -f "$SOAK" ]; then
    ( cd "$src" && ulimit -v 8388608 && timeout 1800 wine "$out/nelisp.exe" --load "$SOAK" ) > "$out/closure.out" 2>&1
    r=$?; line=$(tr -d '\r' < "$out/closure.out" | grep -o 'CLOSURE-SOAK[^"]*' | head -1)
    if [ "$r" -eq 0 ] && [[ "$line" == *" bad=0 "* ]]; then say "PASS closure-soak ${line:0:60}"; else say "FAIL closure-soak rc=$r ${line:0:60}"; fails=$((fails+1)); fi
  else
    say "SKIP closure-soak ($SOAK missing)"
  fi
  if [ "$fails" -eq 0 ]; then say "WIN-SMOKE PASS"; else say "WIN-SMOKE FAIL $fails"; fi
  [ "$fails" -eq 0 ]
}

case "$phase" in
  build) [ $# -eq 2 ] || { echo "usage: $0 build REV OUT" >&2; exit 2; }; build "$1" "$2" ;;
  smoke) [ $# -eq 1 ] || { echo "usage: $0 smoke OUT" >&2; exit 2; }; smoke "$1" ;;
  all)   [ $# -eq 2 ] || { echo "usage: $0 all REV OUT" >&2; exit 2; }; build "$1" "$2" && smoke "$2" ;;
  *) echo "usage: $0 build REV OUT | smoke OUT | all REV OUT" >&2; exit 2 ;;
esac
