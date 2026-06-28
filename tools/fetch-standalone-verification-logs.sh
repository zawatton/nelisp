#!/usr/bin/env bash
# Download and audit stage-d-v3.0 standalone workflow verification logs.
set -euo pipefail

RUN_ID=""
WORKFLOW="stage-d-v3.0-standalone.yml"
OUT_DIR="target/stage-d-v3.0-verification-logs"
AUDIT_ONLY=0

usage() {
  cat <<'USAGE'
usage: tools/fetch-standalone-verification-logs.sh [--run-id ID] [--workflow FILE] [--out-dir DIR] [--audit-only]
USAGE
}

while [ "$#" -gt 0 ]; do
  case "$1" in
    --run-id)
      if [ "$#" -lt 2 ]; then
        usage >&2
        exit 2
      fi
      RUN_ID="$2"
      shift 2
      ;;
    --workflow)
      if [ "$#" -lt 2 ]; then
        usage >&2
        exit 2
      fi
      WORKFLOW="$2"
      shift 2
      ;;
    --out-dir)
      if [ "$#" -lt 2 ]; then
        usage >&2
        exit 2
      fi
      OUT_DIR="$2"
      shift 2
      ;;
    --audit-only)
      AUDIT_ONLY=1
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

if [ "$AUDIT_ONLY" -ne 1 ] && ! command -v gh >/dev/null 2>&1; then
  echo "error: required command not found: gh" >&2
  exit 1
fi

if [ "$AUDIT_ONLY" -eq 1 ]; then
  if [ ! -d "$OUT_DIR" ]; then
    echo "error: audit directory not found: $OUT_DIR" >&2
    exit 1
  fi
elif [ -z "$RUN_ID" ]; then
  RUN_ID="$(gh run list --workflow "$WORKFLOW" --limit 1 --json databaseId --jq '.[0].databaseId')"
  if [ -z "$RUN_ID" ] || [ "$RUN_ID" = "null" ]; then
    echo "error: no workflow runs found for $WORKFLOW" >&2
    exit 1
  fi
  echo "[standalone-logs] using latest run $RUN_ID"
fi

mkdir -p "$OUT_DIR"
if [ "$AUDIT_ONLY" -ne 1 ]; then
  gh run download "$RUN_ID" --pattern 'stage-d-v3.0-*-log' --dir "$OUT_DIR"
fi

missing=""
for platform in linux-x86_64 macos-aarch64 macos-x86_64 windows-x86_64; do
  log="$(find "$OUT_DIR" -type f -name "$platform.log" | head -n 1 || true)"
  if [ -z "$log" ]; then
    missing="${missing}${missing:+, }$platform"
    continue
  fi
  if ! grep -q "Cross-platform verify PASS" "$log"; then
    echo "error: verification log missing PASS marker: $log" >&2
    exit 1
  fi
  if ! grep -q "anvil-standalone-svg-smoke" "$log"; then
    echo "error: verification log missing anvil SVG smoke marker: $log" >&2
    exit 1
  fi
  echo "[standalone-logs] PASS: $platform $log"
done

if [ -n "$missing" ]; then
  echo "error: missing verification logs: $missing" >&2
  exit 1
fi

echo "[standalone-logs] PASS: all verification logs downloaded to $OUT_DIR"
