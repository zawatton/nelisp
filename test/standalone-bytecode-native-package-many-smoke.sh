#!/usr/bin/env bash
set -euo pipefail

repo_root=$(cd "$(dirname "$0")/.." && pwd)
binary=${NELISP_BINARY:-"$repo_root/target/nelisp"}
package_root=$(mktemp -d "${TMPDIR:-/tmp}/nelisp-native-package-many.XXXXXX")
source_copy="$package_root/package-many-functions.el"
elc="${source_copy}c"
package_dir="$package_root/native-package"
manifest="$package_dir/package.npkg"
duplicate_dir="$package_root/duplicate-package"
missing_dir="$package_root/missing-package"
tampered_dir="$package_root/tampered-package"
tampered_manifest="$tampered_dir/package.npkg"
mutation_dir="$package_root/mutation-package"
mutation_manifest="$mutation_dir/package.npkg"
driver=${NELISP_PACKAGE_MANY_DRIVER:-"$repo_root/test/standalone-bytecode-native-package-many-driver.el"}

cleanup() { rm -rf "$package_root"; }
trap cleanup EXIT

if [[ ! -x "$binary" ]]; then
  echo "native-package-many: missing executable: $binary" >&2
  exit 2
fi
if [[ "$(emacs --batch -Q --eval '(princ emacs-version)')" != "31.1" ]]; then
  echo "native-package-many: requires GNU Emacs 31.1 to prepare the .elc fixture" >&2
  exit 2
fi

cp "$repo_root/test/fixtures/native-bytecode/package-many-functions.el" "$source_copy"
emacs --batch -Q -f batch-byte-compile "$source_copy"
if [[ ! -f "$elc" ]]; then
  echo "native-package-many: GNU byte compiler did not emit .elc" >&2
  exit 1
fi

compile_result=$(NELISP_REPO_ROOT="$repo_root" NELISP_PACKAGE_ELC="$elc" \
  NELISP_PACKAGE_DIRECTORY="$package_dir" \
  NELISP_DUPLICATE_PACKAGE_DIRECTORY="$duplicate_dir" \
  NELISP_MISSING_PACKAGE_DIRECTORY="$missing_dir" \
  "$binary" -L "$repo_root/lisp" -L "$repo_root/src" -L "$repo_root/scripts" \
    -L "$repo_root/packages/nl-prelude/src" --load "$driver" \
    --eval '(prin1 (nelisp-test-bytecode-native-package-many-compile))')
if [[ "$compile_result" != *t* || "$compile_result" != *"entries=33"* ]]; then
  printf 'native-package-many: NeLisp compile returned %s\n' "$compile_result" >&2
  exit 1
fi

# The cold process has only module.elc inside the package; remove both host-side
# fixture inputs before opening that package.
rm "$source_copy" "$elc"
cp -a "$package_dir" "$tampered_dir"
entry=$(find "$tampered_dir" -maxdepth 1 -type f -name '*.neln' | sort | head -n 1)
printf x >> "$entry"

cold_result=$(NELISP_REPO_ROOT="$repo_root" NELISP_PACKAGE_MANIFEST="$manifest" \
  NELISP_TAMPERED_PACKAGE_MANIFEST="$tampered_manifest" \
  "$binary" -L "$repo_root/lisp" -L "$repo_root/src" -L "$repo_root/scripts" \
    -L "$repo_root/packages/nl-prelude/src" --load "$driver" \
    --eval '(prin1 (and (nelisp-test-bytecode-native-package-many-hash-control) (nelisp-test-bytecode-native-package-many-cold-run)))')
if [[ "$cold_result" != *t* || "$cold_result" != *"entries=33"* \
      || "$cold_result" != *"lazy-load=0-to-3"* \
      || "$cold_result" != *"gc-identity=PASS"* ]]; then
  printf 'native-package-many: cold process returned %s\n' "$cold_result" >&2
  exit 1
fi

# Negative mutation: remove the last entry from a copy of the 33-entry
# manifest. The ordinary cold-run assertion must reject the now-32-entry set.
cp -a "$package_dir" "$mutation_dir"
mutation_setup=$(NELISP_REPO_ROOT="$repo_root" NELISP_PACKAGE_MANIFEST="$mutation_manifest" \
  "$binary" -L "$repo_root/lisp" -L "$repo_root/src" -L "$repo_root/scripts" \
    -L "$repo_root/packages/nl-prelude/src" --load "$driver" \
    --eval '(prin1 (nelisp-test-bytecode-native-package-many-drop-last-entry))')
if [[ "$mutation_setup" != *t* ]]; then
  printf 'native-package-many: mutation setup returned %s\n' "$mutation_setup" >&2
  exit 1
fi
if NELISP_REPO_ROOT="$repo_root" NELISP_PACKAGE_MANIFEST="$mutation_manifest" \
  "$binary" -L "$repo_root/lisp" -L "$repo_root/src" -L "$repo_root/scripts" \
    -L "$repo_root/packages/nl-prelude/src" --load "$driver" \
    --eval '(prin1 (nelisp-test-bytecode-native-package-many-cold-run))' \
    >"$package_root/mutation.stdout" 2>"$package_root/mutation.stderr"; then
  echo "native-package-many: negative mutation unexpectedly passed" >&2
  exit 1
fi
if ! rg -q 'cold load did not retain all 33 entries' "$package_root/mutation.stderr"; then
  cat "$package_root/mutation.stderr" >&2
  echo "native-package-many: negative mutation failed for an unexpected reason" >&2
  exit 1
fi

echo "native-package-many: PASS (33 public entries, lazy open loads 0 units and first/middle/last calls load 3, GC identity, duplicate/missing/hash refusal, and red-on-drop-entry mutation)"
