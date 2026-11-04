#!/usr/bin/env bash
set -euo pipefail

script_dir="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
repo_root="$(cd -- "$script_dir/.." && pwd)"
binary="${1:?usage: standalone-rooted-conditional-public-smoke.sh NELISP_BIN}"
mkdir -p "$repo_root/target/progress/tmp"
work="$(mktemp -d "$repo_root/target/progress/tmp/rooted-conditional-public.XXXXXX")"
trap 'rm -rf -- "$work"' EXIT

cp "$script_dir/fixtures/native-bytecode/rooted-conditional-public.el" "$work/fixture.el"
emacs --batch -Q -f batch-byte-compile "$work/fixture.el"
rm -f -- "$work/fixture.el"
test ! -e "$work/fixture.el"
export NELISP_CONDITIONAL_ELC="$work/fixture.elc"
export NELISP_CONDITIONAL_ARTIFACT="$work/conditional.nelr"
actual="$("$binary" \
  -L "$repo_root/lisp" -L "$repo_root/src" -L "$repo_root/scripts" \
  -L "$repo_root/packages/nl-prelude/src" \
  --load "$script_dir/standalone-rooted-conditional-public-driver.el")"
printf '%s\n' "$actual"
[[ "$actual" == *"rooted-conditional-public: PASS"* ]]
