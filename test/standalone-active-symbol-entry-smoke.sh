#!/usr/bin/env bash
set -euo pipefail

if [[ $# -ne 2 ]]; then
  printf 'usage: %s NELISP_BIN GNU_EMACS\n' "$0" >&2
  exit 2
fi

nelisp_bin=$1
gnu_emacs=$2
script_dir=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
driver="$script_dir/standalone-active-symbol-entry-driver.el"
for executable in "$nelisp_bin" "$gnu_emacs"; do
  [[ -x "$executable" ]] || { printf 'not executable: %s\n' "$executable" >&2; exit 2; }
done

tmp_parent=${TMPDIR:-"$script_dir/../target"}
mkdir -p "$tmp_parent"
log_dir=$(mktemp -d "$tmp_parent/active-symbol-entry.XXXXXX")
marker=ACTIVE-SYMBOL-ENTRY-PASS

check_run() {
  local name=$1
  local expected_marker=$2
  shift
  shift
  local status=0 marker_count
  if TMPDIR="$log_dir" "$@" >"$log_dir/$name.out" 2>"$log_dir/$name.err"; then
    status=0
  else
    status=$?
  fi
  printf '%s\n' "$status" >"$log_dir/$name.status"
  if [[ $status -ne 0 || -s "$log_dir/$name.err" ]]; then
    printf '%s failed: status=%s stderr=%s\n' "$name" "$status" "$log_dir/$name.err" >&2
    cat "$log_dir/$name.out" >&2
    cat "$log_dir/$name.err" >&2
    exit 1
  fi
  marker_count=$(grep -Fxc "$expected_marker" "$log_dir/$name.out" || true)
  if [[ $marker_count -ne 1 ]] || grep -Ev "^(${expected_marker}|nil)$" "$log_dir/$name.out" >/dev/null; then
    printf '%s completion output invalid: %s\n' "$name" "$log_dir/$name.out" >&2
    cat "$log_dir/$name.out" >&2
    exit 1
  fi
}

check_run nelisp "$marker" "$nelisp_bin" --load "$driver"
check_run gnu "$marker" "$gnu_emacs" --batch -Q --load "$driver"
check_run alias-slot SYMBOL-ENTRY-ALIAS-SLOT-PASS \
  "$nelisp_bin" -L "$script_dir/../lisp" \
  --load "$script_dir/../lisp/nelisp-env.el" \
  --load "$script_dir/standalone-symbol-entry-alias-slot-driver.el"
printf 'ACTIVE-SYMBOL-ENTRY-SMOKE-PASS\nlogs=%s\n' "$log_dir"
