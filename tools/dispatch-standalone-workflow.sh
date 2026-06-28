#!/usr/bin/env bash
# Preflight and optionally dispatch the stage-d-v3.0 standalone workflow.
set -euo pipefail

NELISP_REF="main"
NELISP_EMACS_REF=""
ANVIL_REF=""
WORKFLOW="stage-d-v3.0-standalone.yml"
OUT_DIR="target/stage-d-v3.0-verification-logs"
RUN=0
WATCH=0
FETCH_LOGS=0

usage() {
  cat <<'USAGE'
usage: tools/dispatch-standalone-workflow.sh [--nelisp-ref REF] [--nelisp-emacs-ref REF] [--anvil-ref REF] [--workflow FILE] [--out-dir DIR] [--run] [--watch] [--fetch-logs]
USAGE
}

while [ "$#" -gt 0 ]; do
  case "$1" in
    --nelisp-ref)
      [ "$#" -ge 2 ] || { usage >&2; exit 2; }
      NELISP_REF="$2"
      shift 2
      ;;
    --nelisp-emacs-ref)
      [ "$#" -ge 2 ] || { usage >&2; exit 2; }
      NELISP_EMACS_REF="$2"
      shift 2
      ;;
    --anvil-ref)
      [ "$#" -ge 2 ] || { usage >&2; exit 2; }
      ANVIL_REF="$2"
      shift 2
      ;;
    --workflow)
      [ "$#" -ge 2 ] || { usage >&2; exit 2; }
      WORKFLOW="$2"
      shift 2
      ;;
    --out-dir)
      [ "$#" -ge 2 ] || { usage >&2; exit 2; }
      OUT_DIR="$2"
      shift 2
      ;;
    --run)
      RUN=1
      shift
      ;;
    --watch)
      WATCH=1
      shift
      ;;
    --fetch-logs)
      FETCH_LOGS=1
      shift
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

require_command() {
  if ! command -v "$1" >/dev/null 2>&1; then
    echo "error: required command not found: $1" >&2
    exit 1
  fi
}

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
  echo "[standalone-dispatch] PASS: $repo $ref"
}

require_command git
if [ "$RUN" -eq 1 ]; then
  require_command gh
fi

check_remote_branch nelisp "$NELISP_REF"
check_remote_branch nelisp-emacs "$NELISP_EMACS_REF"
check_remote_branch anvil.el "$ANVIL_REF"

dispatch_cmd=(
  gh workflow run "$WORKFLOW"
  --ref "$NELISP_REF"
  --field "nelisp_emacs_ref=$NELISP_EMACS_REF"
  --field "anvil_ref=$ANVIL_REF"
)

if [ "$RUN" -ne 1 ]; then
  echo
  echo "Dry run. Dispatch with:"
  printf '%q ' "${dispatch_cmd[@]}"
  echo
  echo
  echo "Then audit logs with:"
  printf 'bash tools/fetch-standalone-verification-logs.sh --out-dir %q\n' "$OUT_DIR"
  exit 0
fi

"${dispatch_cmd[@]}"
sleep 5
run_id="$(gh run list --workflow "$WORKFLOW" --branch "$NELISP_REF" --limit 1 --json databaseId --jq '.[0].databaseId')"
if [ -z "$run_id" ] || [ "$run_id" = "null" ]; then
  echo "error: no workflow run found for $WORKFLOW on $NELISP_REF" >&2
  exit 1
fi
echo "[standalone-dispatch] run id: $run_id"

watch_status=0
if [ "$WATCH" -eq 1 ] || [ "$FETCH_LOGS" -eq 1 ]; then
  gh run watch "$run_id" --exit-status || watch_status=$?
  if [ "$watch_status" -ne 0 ] && [ "$FETCH_LOGS" -ne 1 ]; then
    exit "$watch_status"
  fi
fi

if [ "$FETCH_LOGS" -eq 1 ]; then
  bash "$(dirname "$0")/fetch-standalone-verification-logs.sh" --run-id "$run_id" --workflow "$WORKFLOW" --out-dir "$OUT_DIR"
fi

if [ "$watch_status" -ne 0 ]; then
  exit "$watch_status"
fi
