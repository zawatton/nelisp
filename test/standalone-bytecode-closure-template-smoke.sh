#!/usr/bin/env bash
set -euo pipefail

repo_root=$(cd "$(dirname "$0")/.." && pwd)
source_root=${NELISP_SOURCE_ROOT:-$repo_root}
binary=${NELISP_BINARY:-"$source_root/target/nelisp"}
host_emacs=${GNU_EMACS_31_1:-emacs-31.1}
temp_dir=$(mktemp -d "${TMPDIR:-/tmp}/nelisp-closure-template.XXXXXX")
temp_source="$temp_dir/closure-template.el"
temp_elc="$temp_source"c
artifact="$temp_dir/closure-template.neln"
trap 'rm -rf "$temp_dir"' EXIT

if [[ ! -x "$binary" ]]; then
  echo "standalone-bytecode-closure-template: missing executable: $binary" >&2
  exit 2
fi
if ! command -v "$host_emacs" >/dev/null 2>&1; then
  echo "standalone-bytecode-closure-template: missing GNU Emacs 31.1: $host_emacs" >&2
  exit 2
fi

cp "$repo_root/test/fixtures/native-bytecode/gnu-31.1-closure-template.el" \
  "$temp_source"
NELISP_CLOSURE_TEMPLATE_SOURCE="$temp_source" "$host_emacs" -Q --batch \
  --eval '(unless (byte-compile-file (getenv "NELISP_CLOSURE_TEMPLATE_SOURCE")) (kill-emacs 2))' \
  --eval '(delete-file (getenv "NELISP_CLOSURE_TEMPLATE_SOURCE"))'
if [[ -e "$temp_source" || ! -s "$temp_elc" ]]; then
  echo "standalone-bytecode-closure-template: source-free ELC preparation failed" >&2
  exit 1
fi

output=$(NELISP_REPO_ROOT="$source_root" \
  NELISP_CLOSURE_TEMPLATE_ELC="$temp_elc" \
  NELISP_CLOSURE_TEMPLATE_ARTIFACT="$artifact" \
  "$binary" \
    -L "$repo_root/lisp" \
    -L "$source_root/lisp" \
    -L "$source_root/src" \
    -L "$source_root/scripts" \
    -L "$source_root/packages/nl-prelude/src" \
    --load "$repo_root/test/standalone-bytecode-closure-template-driver.el")

pass_string=STANDALONE_CLOSURE_TEMPLATE_PUBLIC_REFUSAL_PASS
if [[ "$output" != *"$pass_string"* ]]; then
  printf 'standalone-bytecode-closure-template: expected %s, got %s\n' \
    "$pass_string" "$output" >&2
  exit 1
fi
if [[ -e "$artifact" ]]; then
  echo "standalone-bytecode-closure-template: refusal created an artifact" >&2
  exit 1
fi
echo "$pass_string"
