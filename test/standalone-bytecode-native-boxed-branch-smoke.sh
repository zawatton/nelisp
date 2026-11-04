#!/usr/bin/env bash
set -euo pipefail

repo_root=$(cd "$(dirname "$0")/.." && pwd)
api_root=${NELISP_ARTIFACT_API_ROOT:-"$repo_root"}
binary=${NELISP_BINARY:-"$repo_root/target/nelisp"}
host=${EMACS:-emacs}
artifact_dir=$(mktemp -d "${TMPDIR:-/tmp}/nelisp-bc-boxed-branch.XXXXXX")
artifact="$artifact_dir/boxed-branch.neln"
trap 'rm -rf "$artifact_dir"' EXIT

if [[ ! -x "$binary" ]]; then
  echo "standalone-bytecode-native-boxed-branch: missing executable: $binary" >&2
  exit 2
fi

NELISP_REPO_ROOT="$repo_root" NELISP_BOXED_BRANCH_ARTIFACT="$artifact" \
  "$host" -Q --batch -L "$api_root/lisp" -L "$repo_root/lisp" \
  -L "$api_root/src" -L "$repo_root/src" \
  -L "$api_root/scripts" -L "$repo_root/scripts" \
  -L "$api_root/packages/nl-prelude/src" -L "$repo_root/packages/nl-prelude/src" \
  -l "$repo_root/test/standalone-bytecode-native-boxed-branch-driver.el" \
  --eval '(nelisp-test-bytecode-native-boxed-branch-build)'

actual=$(NELISP_REPO_ROOT="$repo_root" NELISP_BOXED_BRANCH_ARTIFACT="$artifact" \
  "$binary" --eval \
  "(progn (load (expand-file-name \"test/standalone-bytecode-native-boxed-branch-driver.el\" (getenv \"NELISP_REPO_ROOT\")) nil nil t) (nelisp-test-bytecode-native-boxed-branch-run))")
expected='(t t t)'
if [[ "$actual" != "$expected" ]]; then
  printf 'standalone-bytecode-native-boxed-branch: expected %s, got %s\n' \
    "$expected" "$actual" >&2
  exit 1
fi
echo "standalone-bytecode-native-boxed-branch: PASS (nil/t/cons truthiness, rooted identity across GC, actual native jcc, unsupported-call refusal)"

optional_dir="$artifact_dir/optional-elc"
mkdir -p "$optional_dir"
optional_source="$optional_dir/optional-truthiness.el"
optional_elc="$optional_source"c
or_artifact="$optional_dir/optional-or.neln"
and_artifact="$optional_dir/optional-and.neln"
cp "$repo_root/test/fixtures/native-bytecode/optional-truthiness.el" "$optional_source"
"$host" -Q --batch -f batch-byte-compile "$optional_source"

NELISP_REPO_ROOT="$repo_root" \
NELISP_OPTIONAL_ELC="$optional_elc" \
NELISP_OPTIONAL_OR_ARTIFACT="$or_artifact" \
NELISP_OPTIONAL_AND_ARTIFACT="$and_artifact" \
  "$host" -Q --batch -L "$api_root/lisp" -L "$repo_root/lisp" \
  -L "$api_root/src" -L "$repo_root/src" \
  -L "$api_root/scripts" -L "$repo_root/scripts" \
  -L "$api_root/packages/nl-prelude/src" -L "$repo_root/packages/nl-prelude/src" \
  -l "$repo_root/test/standalone-bytecode-native-boxed-branch-driver.el" \
  --eval '(nelisp-test-bytecode-native-optional-elc-build)'

# The standalone runtime receives only the compiled file and native outputs.
rm "$optional_source"
actual=$(NELISP_REPO_ROOT="$repo_root" \
  NELISP_OPTIONAL_ELC="$optional_elc" \
  NELISP_OPTIONAL_OR_ARTIFACT="$or_artifact" \
  NELISP_OPTIONAL_AND_ARTIFACT="$and_artifact" \
  "$binary" --eval \
  "(progn (load (expand-file-name \"test/standalone-bytecode-native-boxed-branch-driver.el\" (getenv \"NELISP_REPO_ROOT\")) nil nil t) (nelisp-test-bytecode-native-optional-elc-run))")
expected='(t t t t t t t t)'
if [[ "$actual" != "$expected" ]]; then
  printf 'standalone-bytecode-native-boxed-branch: optional .elc expected %s, got %s\n' \
    "$expected" "$actual" >&2
  exit 1
fi
echo "standalone-bytecode-native-boxed-branch: PASS (GNU 31.1 named optional .elc, source-free VM/native parity, GC identity)"
