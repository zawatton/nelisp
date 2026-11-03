#!/usr/bin/env bash
set -euo pipefail

script_dir=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
repo_root=$(cd -- "$script_dir/.." && pwd)
binary=${NELISP_BINARY:?set NELISP_BINARY to the pinned standalone runtime}
if [[ ! -x "$binary" ]]; then
  echo "GNU tq smoke: runtime is missing or not executable: $binary" >&2
  exit 1
fi
binary=$(realpath "$binary")
expected_binary_sha=aea8ea77673f590d5716b5861a37366c849706e6fa05f54c6813f1cd9872db4a
actual_binary_sha=$(sha256sum "$binary" | cut -d' ' -f1)
if [[ "$actual_binary_sha" != "$expected_binary_sha" ]]; then
  echo "GNU tq smoke: standalone binary SHA mismatch: $actual_binary_sha" >&2
  exit 1
fi

vendor_dir="$repo_root/vendor/emacs-lisp/emacs-lisp"
source="$vendor_dir/tq.el"
manifest="$vendor_dir/tq-gnu-emacs-31.1.sha256"
vendor_dir_rel=vendor/emacs-lisp/emacs-lisp
pin_rel="$vendor_dir_rel/tq-gnu-emacs-31.1.sha256"
tmpdir=$(mktemp -d "${TMPDIR:-/tmp}/nelisp-gnu-tq.XXXXXX")
trap 'rm -rf -- "$tmpdir"' EXIT HUP INT TERM

check_pins_at() {
  local root=$1 pin_manifest=$2 expected relative actual
  while read -r expected relative; do
    [[ -n "$expected" ]] || continue
    actual=$(sha256sum "$root/$relative" | cut -d' ' -f1)
    [[ "$actual" == "$expected" ]] || return 1
  done < "$root/$pin_manifest"
}

must_reject() {
  local checker=$1 root=$2 pin_manifest=$3
  if "$checker" "$root" "$pin_manifest"; then return 1; else return 0; fi
}

check_pins_disabled() { return 0; }
check_pins_at "$repo_root" "$pin_rel"

fixture="$tmpdir/fixture"
mkdir -p "$fixture/$vendor_dir_rel"
cp "$source" "$fixture/$vendor_dir_rel/tq.el"
cp "$manifest" "$fixture/$vendor_dir_rel/tq-gnu-emacs-31.1.sha256"
printf '\n;; mutated pin fixture\n' >> "$fixture/$vendor_dir_rel/tq.el"
if ! must_reject check_pins_at "$fixture" "$pin_rel"; then
  echo 'GNU tq smoke: pin checker accepted modified source' >&2
  exit 1
fi
if must_reject check_pins_disabled "$fixture" "$pin_rel"; then
  echo 'GNU tq smoke: negative control did not detect disabled comparison' >&2
  exit 1
fi

if [[ "$(timeout 30s emacs --batch -Q --eval '(princ emacs-version)')" != 31.1 ]]; then
  echo 'GNU tq smoke: requires GNU Emacs 31.1' >&2
  exit 1
fi
elc="$tmpdir/tq.elc"
source_tmp="$tmpdir/tq.el"
cp "$source" "$source_tmp"
timeout 60s emacs --batch -Q -f batch-byte-compile "$source_tmp" >/dev/null
[[ -f "$elc" ]]

oracle=$(timeout 30s emacs --batch -Q -L "$vendor_dir" --eval \
  '(progn (load "tq.el") (princ (format "%S" (tq-queue (cons (list (quote queue)) (cons (quote process) (quote buffer)))))))')
export NELISP_TQ_SOURCE="$source_tmp"
export NELISP_TQ_ELC="$elc"
export NELISP_TQ_ARTIFACT="$tmpdir/tq-queue.nelr"
export NELISP_TQ_ORACLE="$oracle"
if [[ ! -f "$source_tmp" ]]; then
  echo "GNU tq smoke: expected temporary source before deletion: $source_tmp" >&2
  exit 1
fi
rm -f "$source_tmp"

set +e
timeout 150s "$binary" -L "$repo_root/lisp" -L "$repo_root/src" -L "$repo_root/scripts" \
  -L "$repo_root/packages/nl-prelude/src" \
  --load "$script_dir/standalone-bytecode-native-gnu-tq-driver.el" \
  --eval '(nelisp-test-gnu-tq-raw-smoke)' >"$tmpdir/stdout" 2>"$tmpdir/stderr"
status=$?
set -e
if [[ $status -ne 0 ]]; then
  printf 'GNU tq smoke: runtime exit %d (expected native/VM marker missing)\n' "$status" >&2
  cat "$tmpdir/stderr" >&2
  exit "$status"
fi
if grep -Eiq 'uncaught error:|^Error:|backtrace' "$tmpdir/stderr"; then
  echo 'GNU tq smoke: unexpected runtime error text on stderr' >&2
  cat "$tmpdir/stderr" >&2
  exit 1
fi
marker='GNU-TQ-RAW: PASS (GNU 31.1 pinned ELC, removed source, raw CAR, mutable identity across GC, VM and stock parity)'
if ! grep -Fxq "$marker" "$tmpdir/stdout"; then
  printf 'GNU tq smoke: runtime exit %d but expected marker missing\n' "$status" >&2
  cat "$tmpdir/stdout" >&2
  cat "$tmpdir/stderr" >&2
  exit 1
fi
printf 'gnu-tq-raw: PASS (runtime exit %d; marker present; source removed; pin and mutation controls passed)\n' "$status"
