#!/usr/bin/env bash
set -euo pipefail

script_dir=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
repo_root=$(cd -- "$script_dir/.." && pwd)
binary=${NELISP_BINARY:?set NELISP_BINARY to the dedicated standalone binary}
binary=$(realpath "$binary")
tmp_base=${TMPDIR:-"$repo_root/target/progress/tmp"}
mkdir -p "$tmp_base"
tmpdir=$(mktemp -d "$tmp_base/nelisp-mapcar.XXXXXX")
trap 'rm -rf "$tmpdir"' EXIT

gnu_version=$(timeout 15s emacs --batch -Q --eval '(princ emacs-version)')
if [[ "$gnu_version" != "31.1" ]]; then
  echo "mapcar-regression: GNU Emacs 31.1 is required" >&2
  exit 2
fi

if timeout 30s emacs --batch -Q -l "$script_dir/standalone-bytecode-native-mapcar-driver.el" >"$tmpdir/gnu.stdout" 2>"$tmpdir/gnu.stderr"; then
  gnu_status=0
else
  gnu_status=$?
fi
if [[ $gnu_status -ne 0 || -s "$tmpdir/gnu.stderr" ]] || ! grep -Fxq 'mapcar-regression: PASS' "$tmpdir/gnu.stdout"; then
  cat "$tmpdir/gnu.stdout" >&2
  cat "$tmpdir/gnu.stderr" >&2
  echo "mapcar-regression: GNU oracle failed (exit $gnu_status)" >&2
  exit 1
fi

if timeout 60s "$binary" -L "$repo_root/lisp" -L "$repo_root/src" -L "$repo_root/scripts" --load "$script_dir/standalone-bytecode-native-mapcar-driver.el" >"$tmpdir/runtime.stdout" 2>"$tmpdir/runtime.stderr"; then
  runtime_status=0
else
  runtime_status=$?
fi
if [[ $runtime_status -ne 0 || -s "$tmpdir/runtime.stderr" ]] || ! grep -Fxq 'mapcar-regression: PASS' "$tmpdir/runtime.stdout"; then
  cat "$tmpdir/runtime.stdout" >&2
  cat "$tmpdir/runtime.stderr" >&2
  echo "mapcar-regression: runtime failed (exit $runtime_status)" >&2
  exit 1
fi

cat "$tmpdir/gnu.stdout"
grep -Fx 'mapcar-regression: PASS' "$tmpdir/runtime.stdout"
