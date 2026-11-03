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
task_tmp="$repo_root/target/gnu-tq-smoke-tmp"
mkdir -p "$task_tmp"
tmpdir=$(mktemp -d "$task_tmp/gnu-tq-package.XXXXXX")
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
export NELISP_TQ_PACKAGE="$tmpdir/tq-package"
export NELISP_TQ_ORACLE="$oracle"
export NELISP_TQ_PHASE=writer
if [[ ! -f "$source_tmp" ]]; then
  echo "GNU tq smoke: expected temporary source before deletion: $source_tmp" >&2
  exit 1
fi
rm -f "$source_tmp"

run_runtime_phase() {
  local phase=$1
  export NELISP_TQ_PHASE="$phase"
  set +e
  timeout 180s "$binary" -L "$repo_root/lisp" -L "$repo_root/src" -L "$repo_root/scripts" \
  -L "$repo_root/packages/nl-prelude/src" \
  --load "$script_dir/standalone-bytecode-native-gnu-tq-package-driver.el" \
  >"$tmpdir/$phase.stdout" 2>"$tmpdir/$phase.stderr"
  local status=$?
  set -e
  if [[ $status -ne 0 ]]; then
    printf 'GNU tq %s phase: runtime exit %d\n' "$phase" "$status" >&2
    cat "$tmpdir/$phase.stderr" >&2
    exit "$status"
  fi
  if grep -Eiq 'uncaught error:|^Error:|backtrace' "$tmpdir/$phase.stderr"; then
    echo "GNU tq $phase phase: unexpected runtime error text on stderr" >&2
    cat "$tmpdir/$phase.stderr" >&2
    exit 1
  fi
}

run_runtime_phase writer
rm -f "$source_tmp" "$elc"
if [[ -e "$source_tmp" || -e "$elc" ]]; then
  echo 'GNU tq smoke: original source or ELC survived writer phase' >&2
  exit 1
fi
export NELISP_TQ_ELC="$tmpdir/tq-package/module.elc"
run_runtime_phase reader
if ! grep -Fxq 'GNU-TQ-WRITER: PASS (ELC data only; unsupported rejected)' "$tmpdir/writer.stdout"; then
  echo 'GNU tq smoke: writer marker missing' >&2
  cat "$tmpdir/writer.stdout" >&2
  exit 1
fi
if ! grep -Fxq 'GNU-TQ-READER: PASS (cold open, lazy native, VM controls, close)' "$tmpdir/reader.stdout"; then
  echo 'GNU tq smoke: reader marker missing' >&2
  cat "$tmpdir/reader.stdout" >&2
  exit 1
fi
printf 'gnu-tq-raw-package: PASS (separate writer/reader processes; sources removed; pin and mutation controls passed)\n'
