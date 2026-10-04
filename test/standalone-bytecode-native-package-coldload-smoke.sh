#!/usr/bin/env bash
set -euo pipefail

repo_root=$(cd "$(dirname "$0")/.." && pwd)
binary=${NELISP_BINARY:-"$repo_root/../standalone-reader-fix/target/nelisp"}
package_root=$(mktemp -d "${TMPDIR:-/tmp}/nelisp-native-package.XXXXXX")
source_copy="$package_root/package-coldload.el"
elc="$package_root/package-coldload.elc"
package_dir="$package_root/native-package"
manifest="$package_dir/package.npkg"
stale_dir="$package_root/stale-package"
stale_manifest="$stale_dir/package.npkg"
truncated_dir="$package_root/truncated-package"
truncated_manifest="$truncated_dir/package.npkg"
tampered_dir="$package_root/tampered-manifest-package"
tampered_manifest="$tampered_dir/package.npkg"
fingerprint_dir="$package_root/fingerprint-package"
fingerprint_manifest="$fingerprint_dir/package.npkg"
race_dir="$package_root/race-package"
race_barrier="$package_root/race-barrier"

if [[ ! -x "$binary" ]]; then
  echo "native-package-coldload: missing executable: $binary" >&2
  exit 2
fi
if [[ "$(emacs --batch -Q --eval '(princ emacs-version)')" != "31.1" ]]; then
  echo "native-package-coldload: requires GNU Emacs 31.1 to prepare the .elc fixture" >&2
  exit 2
fi

cp "$repo_root/test/fixtures/native-bytecode/package-coldload.el" "$source_copy"
emacs --batch -Q -f batch-byte-compile "$source_copy"
if [[ ! -f "$elc" ]]; then
  echo "native-package-coldload: GNU byte compiler did not emit .elc" >&2
  exit 1
fi

compile_result=$(NELISP_REPO_ROOT="$repo_root" NELISP_PACKAGE_ELC="$elc" \
  NELISP_PACKAGE_DIRECTORY="$package_dir" \
  "$binary" -L "$repo_root/lisp" -L "$repo_root/src" -L "$repo_root/scripts" \
    -L "$repo_root/packages/nl-prelude/src" \
    --load "$repo_root/test/standalone-bytecode-native-package-coldload-driver.el" \
    --eval '(prin1 (nelisp-test-bytecode-native-package-compile))')
if [[ "$compile_result" != t ]]; then
  printf 'native-package-coldload: compile process returned %s\n' "$compile_result" >&2
  exit 1
fi

cold_result=$(NELISP_REPO_ROOT="$repo_root" NELISP_PACKAGE_MANIFEST="$manifest" \
  "$binary" -L "$repo_root/lisp" -L "$repo_root/src" -L "$repo_root/scripts" \
    -L "$repo_root/packages/nl-prelude/src" \
    --load "$repo_root/test/standalone-bytecode-native-package-coldload-driver.el" \
    --eval '(prin1 (nelisp-test-bytecode-native-package-cold-run))')
if [[ "$cold_result" != *t* || "$cold_result" != *"native-calls: default=4 optional-value=4"* ]]; then
  printf 'native-package-coldload: cold process returned %s\n' "$cold_result" >&2
  exit 1
fi

cp -a "$package_dir" "$stale_dir"
printf x >> "$stale_dir/nelisp-native-package-default.neln"
stale_result=$(NELISP_REPO_ROOT="$repo_root" NELISP_STALE_PACKAGE_MANIFEST="$stale_manifest" \
  "$binary" -L "$repo_root/lisp" -L "$repo_root/src" -L "$repo_root/scripts" \
    -L "$repo_root/packages/nl-prelude/src" \
    --load "$repo_root/test/standalone-bytecode-native-package-coldload-driver.el" \
    --eval '(prin1 (nelisp-test-bytecode-native-package-stale-control))')
if [[ "$stale_result" != t ]]; then
  printf 'native-package-coldload: stale-identity control returned %s\n' "$stale_result" >&2
  exit 1
fi

cp -a "$package_dir" "$truncated_dir"
truncated_result=$(NELISP_REPO_ROOT="$repo_root" NELISP_TRUNCATED_PACKAGE_MANIFEST="$truncated_manifest" \
  "$binary" -L "$repo_root/lisp" -L "$repo_root/src" -L "$repo_root/scripts" \
    -L "$repo_root/packages/nl-prelude/src" \
    --load "$repo_root/test/standalone-bytecode-native-package-coldload-driver.el" \
    --eval '(prin1 (nelisp-test-bytecode-native-package-truncated-control))')
if [[ "$truncated_result" != t ]]; then
  printf 'native-package-coldload: truncated .elc control returned %s\n' "$truncated_result" >&2
  exit 1
fi

cp -a "$package_dir" "$tampered_dir"
tampered_result=$(NELISP_REPO_ROOT="$repo_root" NELISP_TAMPERED_PACKAGE_MANIFEST="$tampered_manifest" \
  "$binary" -L "$repo_root/lisp" -L "$repo_root/src" -L "$repo_root/scripts" \
    -L "$repo_root/packages/nl-prelude/src" \
    --load "$repo_root/test/standalone-bytecode-native-package-coldload-driver.el" \
    --eval '(prin1 (nelisp-test-bytecode-native-package-tampered-manifest-control))')
if [[ "$tampered_result" != t ]]; then
  printf 'native-package-coldload: tampered manifest controls returned %s\n' "$tampered_result" >&2
  exit 1
fi

cp -a "$package_dir" "$fingerprint_dir"
fingerprint_result=$(NELISP_REPO_ROOT="$repo_root" NELISP_FINGERPRINT_PACKAGE_MANIFEST="$fingerprint_manifest" \
  "$binary" -L "$repo_root/lisp" -L "$repo_root/src" -L "$repo_root/scripts" \
    -L "$repo_root/packages/nl-prelude/src" \
    --load "$repo_root/test/standalone-bytecode-native-package-coldload-driver.el" \
    --eval '(prin1 (nelisp-test-bytecode-native-package-fingerprint-control))')
if [[ "$fingerprint_result" != t ]]; then
  printf 'native-package-coldload: ABI fingerprint control returned %s\n' "$fingerprint_result" >&2
  exit 1
fi

mkdir "$race_barrier"
for identifier in a b; do
  (
    set +e
    timeout 45s env NELISP_REPO_ROOT="$repo_root" NELISP_PACKAGE_ELC="$elc" \
      NELISP_RACE_PACKAGE_DIRECTORY="$race_dir" \
      NELISP_RACE_BARRIER="$race_barrier" NELISP_RACE_ID="$identifier" \
      "$binary" -L "$repo_root/lisp" -L "$repo_root/src" -L "$repo_root/scripts" \
        -L "$repo_root/packages/nl-prelude/src" \
        --load "$repo_root/test/standalone-bytecode-native-package-coldload-driver.el" \
        --eval '(prin1 (nelisp-test-bytecode-native-package-race-publish))' \
        >"$package_root/race-$identifier.log" 2>&1
    printf '%s\n' "$?" >"$package_root/race-$identifier.status"
  ) &
done
wait
race_a_rc=$(cat "$package_root/race-a.status")
race_b_rc=$(cat "$package_root/race-b.status")
winner_count=0
[[ "$race_a_rc" == 0 ]] && winner_count=$((winner_count + 1))
[[ "$race_b_rc" == 0 ]] && winner_count=$((winner_count + 1))
if [[ "$winner_count" -ne 1 ]]; then
  printf 'native-package-publication-race: expected one winner, got a=%s b=%s\n' \
    "$race_a_rc" "$race_b_rc" >&2
  tail -20 "$package_root/race-a.log" "$package_root/race-b.log" >&2
  exit 1
fi
if [[ ! -f "$race_barrier/ready-a" || ! -f "$race_barrier/ready-b" ]]; then
  echo "native-package-publication-race: both publishers did not reach the absent-dir barrier" >&2
  exit 1
fi
loser_log="$package_root/race-a.log"
[[ "$race_a_rc" == 0 ]] && loser_log="$package_root/race-b.log"
if ! rg -q 'file-already-exists: \("Creating directory" "File exists"' "$loser_log"; then
  printf 'native-package-publication-race: loser failed for an unexpected reason: %s\n' \
    "$(tail -4 "$loser_log")" >&2
  exit 1
fi
if [[ ! -f "$race_dir/package.npkg" || ! -f "$race_dir/module.elc" || \
      ! -f "$race_dir/nelisp-native-package-default.neln" || \
      ! -f "$race_dir/nelisp-native-package-optional-value.neln" || \
      -e "$race_dir/package.npkg.tmp" ]]; then
  echo "native-package-publication-race: winner package incomplete or temporary manifest leaked" >&2
  exit 1
fi
race_hash_before=$(find "$race_dir" -maxdepth 1 -type f -print0 | sort -z | xargs -0 sha256sum)
race_cold_result=$(NELISP_REPO_ROOT="$repo_root" NELISP_PACKAGE_MANIFEST="$race_dir/package.npkg" \
  "$binary" -L "$repo_root/lisp" -L "$repo_root/src" -L "$repo_root/scripts" \
    -L "$repo_root/packages/nl-prelude/src" \
    --load "$repo_root/test/standalone-bytecode-native-package-coldload-driver.el" \
    --eval '(prin1 (nelisp-test-bytecode-native-package-cold-run))')
if [[ "$race_cold_result" != *t* || "$race_cold_result" != *"native-calls: default=4 optional-value=4"* ]]; then
  printf 'native-package-publication-race: surviving package failed cold open: %s\n' \
    "$race_cold_result" >&2
  exit 1
fi
race_hash_after=$(find "$race_dir" -maxdepth 1 -type f -print0 | sort -z | xargs -0 sha256sum)
if [[ "$race_hash_before" != "$race_hash_after" ]]; then
  echo "native-package-publication-race: loser/cold-open changed winner artifacts" >&2
  exit 1
fi

echo "native-package-coldload: PASS (two-process .elc package, side effects once, lazy native entries=2, VM/native identity across GC, disabled-native and redefinition controls, stale hash, truncated .elc, path traversal, duplicate entries, and ABI fingerprint mismatch refused)"
echo "native-package-publication-race: PASS (both processes synchronized after absent-dir check, one winner, loser preserved complete package, cold open succeeded)"
