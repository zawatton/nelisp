#!/usr/bin/env bash
set -euo pipefail

script_dir=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
lane_root=$(cd -- "$script_dir/.." && pwd)
repo_root=${NELISP_REPO_ROOT:?Set NELISP_REPO_ROOT}
binary=${NELISP_DIFFERENTIAL_BINARY:?Set NELISP_DIFFERENTIAL_BINARY}
expected=${NELISP_DIFFERENTIAL_SHA256:?Set NELISP_DIFFERENTIAL_SHA256}
binary=$(realpath "$binary")
actual=$(sha256sum "$binary" | cut -d' ' -f1)
[[ "$actual" == "$expected" ]]
tmp_root=${TMPDIR:-$lane_root/target/progress}
mkdir -p "$tmp_root"
work_dir=$(mktemp -d "$tmp_root/variable-alias-callable.XXXXXX")
source_file="$work_dir/gnu-31.1-variable-alias-callable.el"
elc_file="$source_file"c
cp "$script_dir/fixtures/native-bytecode/gnu-31.1-variable-alias-callable.el" "$source_file"
[[ "$(emacs-31.1 --batch -Q --eval '(princ emacs-version)')" == 31.1 ]]
emacs-31.1 --batch -Q -f batch-byte-compile "$source_file" \
  >"$work_dir/compile.stdout" 2>"$work_dir/compile.stderr"
[[ -s "$elc_file" ]]
rm -f -- "$source_file"
[[ ! -e "$source_file" ]]
NELISP_ALIAS_SOURCE="$source_file" NELISP_ALIAS_ELC="$elc_file" \
  emacs-31.1 --batch -Q --load "$script_dir/standalone-gnu-variable-alias-callable-driver.el" \
  >"$work_dir/gnu.stdout" 2>"$work_dir/gnu.stderr"
NELISP_ALIAS_SOURCE="$source_file" NELISP_ALIAS_ELC="$elc_file" \
  timeout 90s "$binary" \
  -L "$lane_root/lisp" -L "$lane_root/src" \
  -L "$repo_root/lisp" -L "$repo_root/src" -L "$repo_root/scripts" \
  -L "$repo_root/packages/nl-prelude/src" \
  --load "$script_dir/standalone-gnu-variable-alias-callable-driver.el" \
  --eval '(progn)' \
  >"$work_dir/nelisp.stdout" 2>"$work_dir/nelisp.stderr"
cmp -s "$work_dir/gnu.stdout" "$work_dir/nelisp.stdout"
cmp -s "$work_dir/gnu.stderr" "$work_dir/nelisp.stderr"
[[ ! -s "$work_dir/gnu.stderr" ]]
[[ "$(cat "$work_dir/gnu.stdout")" == "$(cat "$work_dir/nelisp.stdout")" ]]
cat "$work_dir/nelisp.stdout"
printf 'artifacts=%s\nelc-sha256=' "$work_dir"
sha256sum "$elc_file" | cut -d' ' -f1
