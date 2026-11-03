#!/usr/bin/env bash
set -euo pipefail

repo_root=$(cd "$(dirname "$0")/.." && pwd)
binary=${NELISP_BINARY:-"$repo_root/target/nelisp"}
tmp_dir=$(mktemp -d "${TMPDIR:-/tmp}/nelisp-gnu-elc-load.XXXXXX")
trap 'rm -rf "$tmp_dir"' EXIT
source_copy="$tmp_dir/package-coldload.el"
elc="$tmp_dir/package-coldload.elc"
broken_provide="$tmp_dir/broken-provide.elc"
broken_bytecode="$tmp_dir/broken-bytecode.elc"

if [[ ! -x "$binary" ]]; then
  echo "gnu-elc-load: missing executable: $binary" >&2
  exit 2
fi
if [[ "$(emacs --batch -Q --eval '(princ emacs-version)')" != "31.1" ]]; then
  echo "gnu-elc-load: requires GNU Emacs 31.1" >&2
  exit 2
fi

cp "$repo_root/test/fixtures/native-bytecode/package-coldload.el" "$source_copy"
emacs --batch -Q -f batch-byte-compile "$source_copy"
test -f "$elc"
rm "$source_copy"
test ! -e "$source_copy"

cold_result=$(NELISP_REPO_ROOT="$repo_root" NELISP_GNU_ELC="$elc" \
  "$binary" -L "$repo_root/lisp" -L "$repo_root/src" \
    -L "$repo_root/scripts" -L "$repo_root/packages/nl-prelude/src" \
    --load "$repo_root/test/standalone-gnu-elc-load-driver.el" \
    --eval '(prin1 (nelisp-test-gnu-elc-load-cold-process))')
if [[ "$cold_result" != *t* ]]; then
  printf 'gnu-elc-load: source-free cold process returned %s\n' "$cold_result" >&2
  exit 1
fi

printf ';ELC\037\n(setq nelisp-gnu-elc-truncated-counter 1)\n(provide' > "$broken_provide"
printf ';ELC\037\n(setq nelisp-gnu-elc-truncated-counter 1)\n(defalias '\''truncated-bytecode #[nil "\\207" [] 1)' > "$broken_bytecode"
bad_result=$(NELISP_REPO_ROOT="$repo_root" \
  NELISP_GNU_ELC_BAD_PROVIDE="$broken_provide" \
  NELISP_GNU_ELC_BAD_BYTECODE="$broken_bytecode" \
  "$binary" -L "$repo_root/lisp" -L "$repo_root/src" \
    -L "$repo_root/scripts" -L "$repo_root/packages/nl-prelude/src" \
    --load "$repo_root/test/standalone-gnu-elc-load-driver.el" \
    --eval '(prin1 (nelisp-test-gnu-elc-truncated-control))')
if [[ "$bad_result" != *t* ]]; then
  printf 'gnu-elc-load: truncated-input controls returned %s\n' "$bad_result" >&2
  exit 1
fi

echo "gnu-elc-load: PASS (source-free GNU ELC host diagnostic, default refusal before effects, byte-code identity, NeLisp namespace isolation, truncated provide and #[...] refused before effects)"
