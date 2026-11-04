#!/usr/bin/env bash
set -euo pipefail

repo_root=$(cd "$(dirname "$0")/.." && pwd)
api_root=${NELISP_ARTIFACT_API_ROOT:-"$repo_root"}
binary=${NELISP_BINARY:-"$repo_root/target/nelisp"}
host=${EMACS:-emacs}
artifact_dir=$(mktemp -d "${TMPDIR:-/tmp}/nelisp-bc-native-rest.XXXXXX")
artifact="$artifact_dir/rest-return.neln"
el="$artifact_dir/rest-template.el"
elc="$artifact_dir/rest-template.elc"
trap 'rm -rf "$artifact_dir"' EXIT

cat >"$el" <<'ELISP'
(defalias 'nelisp-test-bytecode-native-rest-function
  (lambda (required &rest values) values))
(provide 'nelisp-test-bytecode-native-rest-template)
ELISP
"$host" -Q --batch --eval \
  "(unless (byte-compile-file \"$el\") (kill-emacs 1))"

if [[ ! -x "$binary" ]]; then
  echo "standalone-bytecode-native-rest: missing executable: $binary" >&2
  exit 2
fi

NELISP_REPO_ROOT="$repo_root" \
NELISP_ARTIFACT_API_ROOT="$api_root" \
NELISP_BC_NATIVE_REST_ARTIFACT="$artifact" \
NELISP_BC_NATIVE_REST_ELC="$elc" \
  "$host" -Q --batch \
    -L "$api_root/lisp" -L "$repo_root/lisp" \
    -L "$api_root/src" -L "$repo_root/src" \
    -L "$api_root/scripts" -L "$repo_root/scripts" \
    -L "$api_root/packages/nl-prelude/src" \
    -L "$repo_root/packages/nl-prelude/src" \
    -l "$repo_root/test/standalone-bytecode-native-rest-driver.el" \
    --eval '(nelisp-test-bytecode-native-rest-build)'

actual=$(NELISP_REPO_ROOT="$repo_root" \
  NELISP_BC_NATIVE_REST_ARTIFACT="$artifact" \
  NELISP_BC_NATIVE_REST_ELC="$elc" \
  "$binary" --eval \
    "(progn (load (expand-file-name \"test/standalone-bytecode-native-rest-driver.el\" (getenv \"NELISP_REPO_ROOT\")) nil nil t) (nelisp-test-bytecode-native-rest-run))")
expected='(t t t t)'
if [[ "$actual" != "$expected" ]]; then
  printf 'standalone-bytecode-native-rest: expected %s, got %s\n' \
    "$expected" "$actual" >&2
  exit 1
fi
echo 'standalone-bytecode-native-rest: PASS (source-free GNU .elc exact REST template, evaluator-built variable calls, rooted identity across GC, descriptor mutation rejected)'
