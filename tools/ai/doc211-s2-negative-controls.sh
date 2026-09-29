#!/usr/bin/env bash
set -euo pipefail
root=$(cd "$(dirname "$0")/../.." && pwd)
tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT
case ${1:?expected native or purity} in
  native)
    cp "$root/scripts/nelisp-standalone-build.el" "$tmp/build.el"
    sed -i 's/"nelisp--symbol-global-cell-p")/"nelisp--symbol-global-cell-p" "doc211-fake-native")/' "$tmp/build.el"
    if NN_SOURCE="$tmp/build.el" NN_INVENTORY="$root/tools/nelisp-native-inventory.txt" \
       emacs --batch -Q -l "$root/tools/nelisp-native-inventory.el" >"$tmp/log" 2>&1; then
      echo 'native inventory negative control unexpectedly passed' >&2; exit 1
    fi
    grep -q 'Native inventory/table mismatch' "$tmp/log"
    echo 'PASS: fake reader native is rejected'
    ;;
  purity)
    mkdir -p "$tmp/lisp" "$tmp/src" "$tmp/scripts"
    printf "(require 'nelisp-emacs-fake)\n" > "$tmp/src/fake.el"
    if bash "$root/tools/core-purity.sh" "$tmp" >"$tmp/log" 2>&1; then
      echo 'core purity negative control unexpectedly passed' >&2; exit 1
    fi
    grep -q 'nelisp-emacs-fake' "$tmp/log"
    echo 'PASS: fake package dependency is rejected'
    ;;
  *) echo "unknown control: $1" >&2; exit 2 ;;
esac
