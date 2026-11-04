#!/usr/bin/env bash
set -euo pipefail

script_dir=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
repo_root=$(cd -- "$script_dir/.." && pwd)
binary=${NELISP_BINARY:?set NELISP_BINARY to the dedicated standalone binary}
binary=$(realpath "$binary")
tmpdir=$(mktemp -d "${TMPDIR:-/tmp}/nelisp-quo.XXXXXX")
trap 'rm -rf "$tmpdir"' EXIT

if [[ "$(timeout 15s emacs --batch -Q --eval '(princ emacs-version)')" != "31.1" ]]; then
  echo "gnu-quo: GNU Emacs 31.1 is required" >&2
  exit 2
fi
cp "$script_dir/fixtures/gnu-bytecode-quo.el" "$tmpdir/gnu-bytecode-quo.el"
timeout 60s emacs --batch -Q -f batch-byte-compile "$tmpdir/gnu-bytecode-quo.el" \
  >"$tmpdir/compile.stdout" 2>"$tmpdir/compile.stderr"
timeout 15s emacs --batch -Q -l "$tmpdir/gnu-bytecode-quo.elc" \
  -l "$script_dir/standalone-bytecode-native-quo-gnu-oracle.el" \
  >"$tmpdir/gnu.stdout" 2>"$tmpdir/gnu.stderr"
grep -Fxq 'gnu-bytecode-oracle: (quo=3,-3,3.5 negate=-7,-2.5 opcode=91 return=135 divide-zero=arith-error wrong-type=wrong-type-argument underflow)' \
  "$tmpdir/gnu.stdout"
unlink "$tmpdir/gnu-bytecode-quo.el"
if [[ -e "$tmpdir/gnu-bytecode-quo.el" ]]; then
  echo "gnu-bytecode-quo: source fixture remained after host oracle" >&2
  exit 1
fi

set +e
NELISP_REPO_ROOT="$repo_root" NELISP_QUO_ELC="$tmpdir/gnu-bytecode-quo.elc" \
  timeout 60s "$binary" -L "$repo_root/lisp" -L "$repo_root/src" \
    --load "$script_dir/standalone-bytecode-native-quo-driver.el" \
    >"$tmpdir/runtime.stdout" 2>"$tmpdir/runtime.stderr"
status=$?
set -e
if [[ $status -ne 0 || -s "$tmpdir/runtime.stderr" ]] || \
   ! grep -Fxq 'gnu-bytecode-runtime: (quo=3,-3,3.5 negate=-7,-2.5 errors=arith-error,wrong-type-argument malformed=error)' \
     "$tmpdir/runtime.stdout"; then
  cat "$tmpdir/runtime.stdout" >&2
  cat "$tmpdir/runtime.stderr" >&2
  echo "gnu-quo: runtime assertion failed (exit $status)" >&2
  exit 1
fi
cat "$tmpdir/gnu.stdout"
cat "$tmpdir/runtime.stdout"
