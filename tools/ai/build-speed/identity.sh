#!/usr/bin/env bash
# B1.1 of tools/ai/build-speed-progress.org: two reader builds from empty
# unit caches, the toolchain interpreted (NELISP_STANDALONE_TOOLCHAIN_ELC=0)
# and byte-compiled, must link byte-identical binaries.  Prints both wall
# times; that interpreted run is also the controlled baseline for the speedup.
set -u
root=$(git rev-parse --show-toplevel) || exit 2
cd "$root" || exit 2
work=target/build-speed-identity
rm -rf "${work:?}"
mkdir -p "$work"
build() {  # MODE ELC
  local t0=$SECONDS
  NELISP_STANDALONE_TOOLCHAIN_ELC=$2 NELISP_STANDALONE_JOBS=${NELISP_STANDALONE_JOBS:-4} \
  NELISP_STANDALONE_TARGET=linux-x86_64 NELISP_STANDALONE_UNIT_CACHE_DIR="$root/$work/units-$1" \
    emacs --batch -Q -L lisp -L src -L scripts --eval '(setq load-prefer-newer t)' \
      -l nelisp-standalone-build -f nelisp-standalone-build-reader > "$work/$1.log" 2>&1 || return 1
  grep -m1 '\[standalone\] toolchain:' "$work/$1.log"
  cp target/nelisp "$work/nelisp-$1"
  echo "$1: $((SECONDS - t0)) s"
}
build interpreted 0 || { echo "interpreted build failed (see $work/interpreted.log)"; exit 1; }
build compiled 1 || { echo "compiled build failed (see $work/compiled.log)"; exit 1; }
grep -q 'toolchain: byte-compiled' "$work/compiled.log" || { echo "compiled build fell back to source"; exit 1; }
cmp "$work/nelisp-interpreted" "$work/nelisp-compiled" && echo "IDENTICAL: $(sha256sum < "$work/nelisp-compiled" | cut -c1-16)"
