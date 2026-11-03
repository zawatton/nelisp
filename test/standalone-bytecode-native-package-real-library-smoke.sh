#!/usr/bin/env bash
set -euo pipefail

script_dir=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
repo_root=$(cd -- "$script_dir/.." && pwd)
binary=${NELISP_BINARY:-}
package_root=$(mktemp -d "${TMPDIR:-/tmp}/nelisp-real-library.XXXXXX")
trap 'rm -rf "$package_root"' EXIT

if [[ -z "$binary" || ! -x "$binary" ]]; then
  printf 'real-library-native: set NELISP_BINARY to a standalone reader\n' >&2
  exit 2
fi
binary=$(realpath "$binary")
check_source_pin() {
  local manifest=$1
  if ! (cd "$repo_root" && sha256sum -c "$manifest"); then
    printf 'real-library-native: GNU source pin mismatch: %s\n' "$manifest" >&2
    exit 1
  fi
}
check_source_pin vendor/emacs-lisp/calendar/gnu-emacs-31.1.sha256
check_source_pin vendor/emacs-lisp/calendar/cal-menu-gnu-emacs-31.1.sha256
check_source_pin vendor/emacs-lisp/emacs-lisp/easymenu-gnu-emacs-31.1.sha256
if ! command -v timeout >/dev/null 2>&1; then
  echo "real-library-native: timeout utility unavailable" >&2
  exit 2
fi
if [[ "$(timeout 30s emacs --batch -Q --eval '(princ emacs-version)')" != "31.1" ]]; then
  echo "real-library-native: requires GNU Emacs 31.1" >&2
  exit 2
fi

source_gz=$(timeout 30s emacs --batch -Q --eval \
  '(let ((library (locate-library "cal-french")))
     (unless library (error "cal-french library not found"))
     (princ (concat (file-name-sans-extension library) ".el.gz")))')
if [[ ! -r "$source_gz" ]]; then
  printf 'real-library-native: GNU library source unavailable: %s\n' "$source_gz" >&2
  exit 2
fi

source_copy="$package_root/cal-french.el"
elc="$source_copy"c
package_dir="$package_root/native-package"
manifest="$package_dir/package.npkg"
gzip -dc "$source_gz" > "$source_copy"
timeout 120s emacs --batch -Q -f batch-byte-compile "$source_copy" >/dev/null
if [[ ! -f "$elc" ]]; then
  echo "real-library-native: GNU byte compiler did not emit .elc" >&2
  exit 1
fi

compile_result=$(timeout 180s env NELISP_REPO_ROOT="$repo_root" \
  NELISP_REAL_LIBRARY_ELC="$elc" \
  NELISP_REAL_LIBRARY_PACKAGE="$package_dir" \
  "$binary" -L "$repo_root/lisp" -L "$repo_root/src" -L "$repo_root/scripts" \
    -L "$repo_root/packages/nl-prelude/src" \
    --load "$script_dir/standalone-bytecode-native-package-real-library-driver.el" \
    --eval '(princ (nelisp-test-real-library-package-compile))')
if [[ "$compile_result" != t ]]; then
  printf 'real-library-native: package compile returned %s\n' "$compile_result" >&2
  exit 1
fi
if [[ ! -f "$manifest" || ! -f "$package_dir/module.elc" || \
      ! -f "$package_dir/calendar-french-accents-p.neln" ]]; then
  echo "real-library-native: package omitted manifest, ELC, or native entry" >&2
  exit 1
fi
if find "$package_dir" -maxdepth 1 -type f -name '*.el' -print -quit | grep -q .; then
  echo "real-library-native: source .el leaked into package" >&2
  exit 1
fi
rm -f "$source_copy" "$elc"

cold_result=$(timeout 180s env NELISP_REPO_ROOT="$repo_root" \
  NELISP_REAL_LIBRARY_MANIFEST="$manifest" \
  "$binary" -L "$repo_root/lisp" -L "$repo_root/src" -L "$repo_root/scripts" \
    -L "$repo_root/packages/nl-prelude/src" \
    --load "$script_dir/standalone-bytecode-native-package-real-library-driver.el" \
    --eval '(princ (nelisp-test-real-library-package-cold-run))')
expected='native-library-result:(t 1 t 1; load-evals=1)'
if [[ "$cold_result" != "$expected" ]]; then
  printf 'real-library-native: expected %s, got %s\n' "$expected" "$cold_result" >&2
  exit 1
fi
printf '%s\n' 'real-library-native: PASS (GNU cal-french.elc, source-free cold open, native body count=1, require-once, VM control)'
