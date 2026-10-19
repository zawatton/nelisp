#!/usr/bin/env bash
set -euo pipefail
root=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")/.." && pwd)
cd "$root"
binary=${1:-target/nelisp-dyn}
backend=${2:-in-house}
case "$backend" in in-house|gccjit) ;; *) exit 2;; esac
work=$(mktemp -d "$root/target/variable-u7c-XXXXXX")
chmod 700 "$work"
export TMPDIR="$work"
cp -- "$binary" "$work/reader"
chmod 500 "$work/reader"
if [[ -f $binary.cold ]]; then cp -- "$binary.cold" "$work/reader.cold"; fi
if [[ -f $binary.native-startup.el ]]; then cp -- "$binary.native-startup.el" "$work/reader.native-startup.el"; fi
status=0
for case_name in rich constant-set; do
  cache="$work/cache-$case_name"
  mkdir -m 700 "$cache"
  command=(timeout -k 5 290 "$work/reader")
  if [[ $backend != gccjit && -f $work/reader.cold ]]; then
    command+=(--cold-load-from "$work/reader.cold")
  fi
  command+=(-L lisp -L src -L scripts -L packages/nl-ffi/src -L packages/nl-prelude/src
            --load test/standalone-native-variable-u7c-driver.el)
  if ! U7C_BACKEND="$backend" U7C_CASE="$case_name" NELISP_NATIVE_CACHE="$cache" \
       "${command[@]}" >"$work/$case_name.out" 2>"$work/$case_name.err"; then
    status=1
  fi
  cat "$work/$case_name.out" "$work/$case_name.err"
  if [[ -s $work/$case_name.err ]] || ! rg -q "U7C-NATIVE-PASS.*case=$case_name" "$work/$case_name.out"; then
    status=1
  fi
done
echo "U7C-EVIDENCE=$work"
exit "$status"
