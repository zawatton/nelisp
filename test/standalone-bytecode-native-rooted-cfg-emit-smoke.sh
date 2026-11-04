#!/usr/bin/env bash
set -euo pipefail

script_dir="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
repo_root="$(cd -- "$script_dir/.." && pwd)"
binary="$(realpath "${NELISP_BINARY:?Set NELISP_BINARY to the pinned raw-v2 binary}")"
expected="${NELISP_EXPECTED_SHA256:?Set NELISP_EXPECTED_SHA256}"
tmpdir="$(mktemp -d "${TMPDIR:-/tmp}/nelisp-rooted-cfg.XXXXXX")"
if [[ "${NELISP_ROOTED_CFG_KEEP_TMP:-0}" == 1 ]]; then
  trap 'printf "rooted-cfg temp retained: %s\n" "$tmpdir" >&2' EXIT
else
  trap 'rm -rf -- "$tmpdir"' EXIT
fi
source_path="$tmpdir/gnu-31.1-rooted-cfg-emit.el"
elc_path="${source_path}c"
artifact_dir="$tmpdir/artifacts"
stage_log="${NELISP_ROOTED_CFG_STAGE_LOG:-$repo_root/target/progress/gnu-rooted-cfg-emit/native-stage.log}"
mkdir -p -- "$artifact_dir" "$(dirname -- "$stage_log")"
[[ "$(sha256sum "$binary" | cut -d' ' -f1)" == "$expected" ]]
[[ "$(emacs --batch -Q --eval '(princ emacs-version)')" == "31.1" ]]
cp "$script_dir/fixtures/native-bytecode/gnu-31.1-rooted-cfg-emit.el" "$source_path"
emacs --batch -Q -f batch-byte-compile "$source_path" 2>"$tmpdir/gnu-compile.stderr"
[[ -f "$elc_path" ]]
rm -f -- "$source_path"
[[ ! -e "$source_path" ]]
rm -f -- "$stage_log"
run_form='(progn (nelisp-test-rooted-cfg-smoke) (princ "rooted-cfg: source-free GNU31 native smoke PASS"))'
if [[ "${NELISP_ROOTED_CFG_DIAGNOSTIC:-0}" == 1 ]]; then
  run_form='(progn (nelisp-test-rooted-cfg-diagnostic) (princ "rooted-cfg: one-entry diagnostic PASS"))'
fi
NELISP_ROOTED_CFG_SOURCE="$source_path" \
NELISP_ROOTED_CFG_ELC="$elc_path" \
NELISP_ROOTED_CFG_ARTIFACT_DIR="$artifact_dir" \
NELISP_ROOTED_CFG_STAGE_LOG="$stage_log" \
timeout 240 "$binary" \
  -L "$repo_root/lisp" -L "$repo_root/src" -L "$repo_root/scripts" \
  -L "$repo_root/packages/nl-prelude/src" \
  --load "$script_dir/standalone-bytecode-native-rooted-cfg-emit-driver.el" \
  --eval "$run_form"
