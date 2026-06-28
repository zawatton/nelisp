#!/usr/bin/env bash
# Check related repository refs before running the standalone CI workflow.
set -euo pipefail

NELISP_REF="main"
NELISP_EMACS_REF=""
ANVIL_REF=""

usage() {
  cat <<'USAGE'
usage: tools/check-standalone-related-refs.sh [--nelisp-ref REF] [--nelisp-emacs-ref REF] [--anvil-ref REF]
USAGE
}

while [ "$#" -gt 0 ]; do
  case "$1" in
    --nelisp-ref)
      if [ "$#" -lt 2 ]; then
        usage >&2
        exit 2
      fi
      NELISP_REF="$2"
      shift 2
      ;;
    --nelisp-emacs-ref)
      if [ "$#" -lt 2 ]; then
        usage >&2
        exit 2
      fi
      NELISP_EMACS_REF="$2"
      shift 2
      ;;
    --anvil-ref)
      if [ "$#" -lt 2 ]; then
        usage >&2
        exit 2
      fi
      ANVIL_REF="$2"
      shift 2
      ;;
    -h|--help)
      usage
      exit 0
      ;;
    *)
      usage >&2
      exit 2
      ;;
  esac
done

if [ -z "$NELISP_EMACS_REF" ]; then
  NELISP_EMACS_REF="$NELISP_REF"
fi
if [ -z "$ANVIL_REF" ]; then
  ANVIL_REF="$NELISP_REF"
fi

check_remote_branch() {
  local repo="$1" ref="$2" url
  if [ -z "$ref" ]; then
    echo "error: empty ref for $repo" >&2
    exit 2
  fi
  url="https://github.com/zawatton/${repo}.git"
  if ! git ls-remote --exit-code --heads "$url" "$ref" >/dev/null 2>&1; then
    echo "error: missing branch $ref in $repo" >&2
    exit 1
  fi
  echo "[related-refs] PASS: $repo $ref"
}

check_remote_branch nelisp "$NELISP_REF"
check_remote_branch nelisp-emacs "$NELISP_EMACS_REF"
check_remote_branch anvil.el "$ANVIL_REF"

echo
echo "Dispatch command:"
printf 'gh workflow run stage-d-v3.0-standalone.yml --ref %s --field nelisp_emacs_ref=%s --field anvil_ref=%s\n' \
  "$NELISP_REF" "$NELISP_EMACS_REF" "$ANVIL_REF"
