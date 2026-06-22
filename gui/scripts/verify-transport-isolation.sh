#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
GUI_ROOT=$(CDPATH= cd -- "$SCRIPT_DIR/.." && pwd)

usage() {
  cat <<'EOF'
Usage: scripts/verify-transport-isolation.sh [--strict-artifacts]

Verify that the native GUI IR transport path rewrite does not leave fixed
/tmp/nemacs-* transport paths when NEMACS_TRANSPORT_DIR is non-default.

By default this is a non-destructive IR check. It also reports fixed native
artifact references if any are reintroduced. Use --strict-artifacts to make
those references fail the check.

Environment:
  NEMACS_VERIFY_TRANSPORT_DIR   isolated rewrite target
  NEMACS_VERIFY_CONFIG_PATH     isolated native config target
  NEMACS_X_DISPLAY_NUM          display number baked into IR, default 0
EOF
}

strict_artifacts=0
case "${1:-}" in
  "")
    ;;
  --strict-artifacts)
    strict_artifacts=1
    ;;
  -h|--help|help)
    usage
    exit 0
    ;;
  *)
    usage >&2
    exit 2
    ;;
esac

cd "$GUI_ROOT"

td=${NEMACS_VERIFY_TRANSPORT_DIR:-${TMPDIR:-/tmp}/nelisp-gui-transport-isolation-$$/transport}
mkdir -p "$td"
td=$(CDPATH= cd -- "$td" && pwd)
cfg=${NEMACS_VERIFY_CONFIG_PATH:-$td/.nemacs-artifacts/nemacs.cfg}
mkdir -p "$(dirname -- "$cfg")"
cfg=$(cd "$(dirname -- "$cfg")" && pwd)/$(basename -- "$cfg")

tmpdir=$(mktemp -d "${TMPDIR:-/tmp}/nemacs-transport-isolation.XXXXXX")
trap 'rm -rf "$tmpdir"' EXIT

paths_file="$tmpdir/ir-paths.txt"
leaks_file="$tmpdir/ir-leaks.txt"
artifact_refs_file="$tmpdir/artifact-refs.txt"

printf '== native IR transport rewrite ==\n'
printf 'transport dir: %s\n' "$td"
printf 'config path:   %s\n' "$cfg"

NEMACS_TRANSPORT_DIR="$td" NEMACS_CONFIG_PATH="$cfg" emacs -Q --batch \
  -l nemacs-editor.el \
  -l nemacs-editor-transport.el \
  --eval "(progn
            (require 'cl-lib)
            (let* ((td (file-name-as-directory (expand-file-name (getenv \"NEMACS_TRANSPORT_DIR\"))))
                   (paths (sort (nemacs--ptr-write-u8-paths xfont-sexp) #'string<))
                   (leaks nil))
              (dolist (path paths)
                (princ path)
                (princ \"\n\")
                (when (and (string-prefix-p \"/tmp/nemacs-\" path)
                           (not (string-prefix-p td path)))
                  (push path leaks)))
              (with-temp-file \"$leaks_file\"
                (dolist (path (sort (delete-dups leaks) #'string<))
                  (insert path \"\n\")))))" >"$paths_file"

if [ -s "$leaks_file" ]; then
  printf 'FAIL: fixed /tmp transport paths remain in rewritten IR:\n' >&2
  sed 's/^/  /' "$leaks_file" >&2
  exit 1
fi

if ! grep -qx "$td/nemacs-keys" "$paths_file"; then
  printf 'FAIL: rewritten IR did not contain %s/nemacs-keys\n' "$td" >&2
  exit 1
fi

if ! grep -qx "$cfg" "$paths_file"; then
  printf 'FAIL: rewritten IR did not contain isolated config path %s\n' "$cfg" >&2
  exit 1
fi

if grep -qx '/tmp/nemacs.cfg' "$paths_file"; then
  printf 'FAIL: rewritten IR still contains fixed native config path\n' >&2
  exit 1
fi

printf 'PASS: rewritten IR has isolated transport and config paths\n'

printf '\n== fixed native artifact references ==\n'
rg -n --no-heading '/tmp/nemacs(\.cfg|-win\.bin|-win\.transport-dir|-win\.x-display)' \
  bin/nemacs nemacs-build.sh nemacs-editor-transport.el >"$artifact_refs_file" || true

if [ -s "$artifact_refs_file" ]; then
  if [ "$strict_artifacts" = 1 ]; then
    printf 'FAIL: fixed native artifact paths remain:\n' >&2
    sed 's/^/  /' "$artifact_refs_file" >&2
    exit 1
  fi
  printf 'WARN: fixed native artifact paths still exist (use --strict-artifacts to fail):\n'
  sed 's/^/  /' "$artifact_refs_file"
else
  printf 'PASS: no fixed native artifact path references found\n'
fi
