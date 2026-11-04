#!/usr/bin/env bash
set -euo pipefail

repo_root=$(cd "$(dirname "$0")/.." && pwd)
binary=${NELISP_BINARY:-"$repo_root/target/nelisp"}
host=${EMACS:-emacs}
temp_root=$(mktemp -d "${TMPDIR:-/tmp}/nelisp-public-rest.XXXXXX")
source="$temp_root/rest.el"
elc="$temp_root/rest.elc"
bad_elc="$temp_root/rest-bad.elc"
package="$temp_root/package"
manifest="$package/package.npkg"
trap 'rm -rf "$temp_root"' EXIT

if [[ ! -x "$binary" ]]; then
  echo "public-rest: missing standalone executable: $binary" >&2
  exit 2
fi

NELISP_REPO_ROOT="$repo_root" \
NELISP_PUBLIC_REST_SOURCE="$source" \
NELISP_PUBLIC_REST_ELC="$elc" \
NELISP_PUBLIC_REST_BAD_ELC="$bad_elc" \
  "$host" -Q --batch \
    -L "$repo_root/lisp" -L "$repo_root/src" -L "$repo_root/scripts" \
    -l "$repo_root/test/standalone-bytecode-native-public-rest-driver.el" \
    --eval '(nelisp-test-public-rest-prepare-elc)'

compile_result=$(NELISP_REPO_ROOT="$repo_root" \
  NELISP_PUBLIC_REST_ELC="$elc" \
  NELISP_PUBLIC_REST_BAD_ELC="$bad_elc" \
  NELISP_PUBLIC_REST_PACKAGE="$package" \
  "$binary" -L "$repo_root/lisp" -L "$repo_root/src" \
    -L "$repo_root/scripts" -L "$repo_root/packages/nl-prelude/src" \
    --load "$repo_root/test/standalone-bytecode-native-public-rest-driver.el" \
    --eval '(prin1 (nelisp-test-public-rest-compile-package))')
if [[ "$compile_result" != *t* || ! -f "$manifest" || -f "$source" ]]; then
  printf 'public-rest: package compile/publication failed: %s\n' "$compile_result" >&2
  exit 1
fi

cold_result=$(NELISP_REPO_ROOT="$repo_root" \
  NELISP_PUBLIC_REST_MANIFEST="$manifest" \
  "$binary" -L "$repo_root/lisp" -L "$repo_root/src" \
    -L "$repo_root/scripts" -L "$repo_root/packages/nl-prelude/src" \
    --load "$repo_root/test/standalone-bytecode-native-public-rest-driver.el" \
    --eval '(prin1 (nelisp-test-public-rest-cold-call))')
if [[ "$cold_result" != *":ok t"* || "$cold_result" != *":native-calls 1"* || \
      "$cold_result" != *":wrong-arity-refused t"* ]]; then
  printf 'public-rest: cold package call failed: %s\n' "$cold_result" >&2
  exit 1
fi

echo 'public-rest: PASS (cold source-free GNU .elc, public compile/publish/open/call, evaluator values, GC identity, wrong arity, corrupted descriptor rejected before artifact)'
