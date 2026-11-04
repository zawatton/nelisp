#!/usr/bin/env bash
set -euo pipefail

script_dir=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
lane_root=$(cd -- "$script_dir/.." && pwd)
repo_root=${NELISP_REPO_ROOT:-}
binary=${NELISP_DIFFERENTIAL_BINARY:-}
if [[ -z "$repo_root" || ! -d "$repo_root/src" || -z "$binary" || ! -x "$binary" ]]; then
  echo 'variable-alias-differential: set NELISP_REPO_ROOT and NELISP_DIFFERENTIAL_BINARY' >&2
  exit 2
fi
binary=$(realpath "$binary")
tmp_root=${TMPDIR:-$lane_root/target/progress}
mkdir -p "$tmp_root"
work_dir=$(mktemp -d "$tmp_root/nelisp-variable-alias.XXXXXX")
source_file="$work_dir/gnu-31.1-variable-alias.el"
elc_file="$source_file"c
cp "$script_dir/fixtures/native-bytecode/gnu-31.1-variable-alias.el" "$source_file"
gnu_version=$(timeout 30s emacs --batch -Q --eval '(princ emacs-version)')
if [[ "$gnu_version" != 31.1 ]]; then
  echo "variable-alias-differential: expected GNU Emacs 31.1, got $gnu_version" >&2
  exit 2
fi
timeout 60s emacs --batch -Q -f batch-byte-compile "$source_file" \
  >"$work_dir/compile.stdout" 2>"$work_dir/compile.stderr"
if [[ ! -s "$elc_file" ]]; then
  echo 'variable-alias-differential: GNU compiler omitted ELC' >&2
  exit 1
fi
python3 -c 'from pathlib import Path; Path(__import__("sys").argv[1]).unlink()' "$source_file"
if [[ -e "$source_file" ]]; then
  echo 'variable-alias-differential: source removal failed' >&2
  exit 1
fi

oracle="$script_dir/standalone-gnu-variable-alias-oracle.el"
set +e
NELISP_ALIAS_ELC="$elc_file" timeout 60s emacs --batch -Q \
  --eval '(setq load-verbose nil)' --load "$oracle" \
  >"$work_dir/gnu.stdout" 2>"$work_dir/gnu.stderr"
gnu_status=$?
NELISP_ALIAS_ELC="$elc_file" timeout 60s "$binary" \
  -L "$repo_root/lisp" -L "$repo_root/src" -L "$repo_root/scripts" \
  -L "$repo_root/packages/nl-prelude/src" \
  --eval '(setq load-verbose nil)' --load "$oracle" \
  >"$work_dir/nelisp.stdout" 2>"$work_dir/nelisp.stderr"
nelisp_status=$?
set -e

expected=$(cat <<'EOF'
alias-oracle: elc-loaded
alias-oracle: function-and-plist-isolation
alias-oracle: chain-read-write
alias-oracle: dynamic-binding
alias-oracle: funcall
alias-oracle: cycle-rejection
alias-oracle: boundness
variable-alias: PASS
EOF
)
if [[ "$gnu_status" -ne 0 || "$(cat "$work_dir/gnu.stdout")" != "$expected" || \
      -s "$work_dir/gnu.stderr" ]]; then
  echo 'variable-alias-differential: GNU oracle failed its strict exit/marker/stderr checks' >&2
  printf 'exit=%s\n' "$gnu_status" >&2
  cat "$work_dir/gnu.stdout" "$work_dir/gnu.stderr" >&2
  echo "artifacts=$work_dir" >&2
  exit 1
fi
if [[ "$nelisp_status" -ne 0 || "$(cat "$work_dir/nelisp.stdout")" != "$expected" || \
      -s "$work_dir/nelisp.stderr" ]]; then
  echo 'variable-alias-differential: NeLisp parity check failed (red is retained)' >&2
  printf 'exit=%s\n' "$nelisp_status" >&2
  cat "$work_dir/nelisp.stdout" "$work_dir/nelisp.stderr" >&2
  echo "artifacts=$work_dir" >&2
  exit 1
fi
if ! cmp -s "$work_dir/gnu.stdout" "$work_dir/nelisp.stdout" || \
   ! cmp -s "$work_dir/gnu.stderr" "$work_dir/nelisp.stderr"; then
  echo 'variable-alias-differential: runtime output differs' >&2
  echo "artifacts=$work_dir" >&2
  exit 1
fi
printf '%s\n' "$expected"
printf 'artifacts=%s\nelc-sha256=' "$work_dir"
sha256sum "$elc_file" | cut -d ' ' -f 1
