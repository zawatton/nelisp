#!/usr/bin/env bash
# POSIX standalone anvil CAD/SVG smoke.
#
# Exercises the user-facing bin/anvil CAD/SVG commands through a standalone
# nelisp reader and the nelisp-emacs/anvil.el shim load path.  Pass
# --svg-dir DIR to use a real SVG directory; otherwise a small sample SVG is
# generated in the smoke work directory.
set -euo pipefail

SVG_DIR="${ANVIL_STANDALONE_SVG_DIR:-}"
WORK_DIR=""
ANVIL_BIN=""
SUITE="auto"

usage() {
  echo "usage: $0 [--suite NAME] [--svg-dir DIR] [--work-dir DIR] [--anvil BIN]" >&2
  echo "suite: auto, real-edit, annotate, outline, extract, basic, batch, entity, late, late-extract, late-batch, generate, full" >&2
}

while [ "$#" -gt 0 ]; do
  case "$1" in
    --suite) SUITE="$2"; shift 2 ;;
    --svg-dir) SVG_DIR="$2"; shift 2 ;;
    --work-dir) WORK_DIR="$2"; shift 2 ;;
    --anvil) ANVIL_BIN="$2"; shift 2 ;;
    -h|--help) usage; exit 0 ;;
    *) usage; exit 2 ;;
  esac
done

REPO_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
cd "$REPO_ROOT"

if [ -z "$ANVIL_BIN" ]; then
  ANVIL_BIN="$REPO_ROOT/bin/anvil"
fi
if [ -z "$WORK_DIR" ]; then
  WORK_DIR="$REPO_ROOT/target/anvil-standalone-svg-smoke-posix"
fi

if [ ! -x "$ANVIL_BIN" ]; then
  echo "[anvil-standalone-svg-smoke] FAIL: anvil launcher not executable: $ANVIL_BIN" >&2
  exit 1
fi
if [ "$ANVIL_BIN" = "$REPO_ROOT/bin/anvil" ]; then
  found_nelisp=0
  for cand in \
      "$REPO_ROOT/target/nelisp" \
      "$REPO_ROOT/target/nelisp.exe" \
      "$REPO_ROOT/target/nelisp-macos-aarch64" \
      "$REPO_ROOT/target/nelisp-macos-x86_64" \
      "$REPO_ROOT/target/nelisp-aarch64" \
      "$REPO_ROOT/target/nelisp-aarch64.exe" \
  ; do
    if [ -x "$cand" ]; then
      found_nelisp=1
      break
    fi
  done
  if [ "$found_nelisp" -ne 1 ]; then
    echo "[anvil-standalone-svg-smoke] FAIL: standalone nelisp missing under $REPO_ROOT/target" >&2
    exit 1
  fi
fi

mkdir -p "$WORK_DIR"
rm -f "$WORK_DIR"/input.svg \
      "$WORK_DIR"/output.svg \
      "$WORK_DIR"/os-identity.txt \
      "$WORK_DIR"/outline.txt \
      "$WORK_DIR"/extract.txt \
      "$WORK_DIR"/batch-output.svg \
      "$WORK_DIR"/entity-input.svg \
      "$WORK_DIR"/entity-extract.txt \
      "$WORK_DIR"/entity-batch-output.svg \
      "$WORK_DIR"/late-input.svg \
      "$WORK_DIR"/late-extract.txt \
      "$WORK_DIR"/late-batch-output.svg \
      "$WORK_DIR"/generated.svg \
      "$WORK_DIR"/entities.json

INPUT_SOURCE=""
INPUT_SOURCE_CKSUM=""
REAL_INPUT=0
if [ -n "$SVG_DIR" ]; then
  if [ ! -d "$SVG_DIR" ]; then
    echo "[anvil-standalone-svg-smoke] FAIL: --svg-dir not found: $SVG_DIR" >&2
    exit 1
  fi
  INPUT_SOURCE="$(find "$SVG_DIR" -maxdepth 1 -type f -name '*.svg' | head -n 1 || true)"
fi

if [ -n "$INPUT_SOURCE" ]; then
  REAL_INPUT=1
  INPUT_SOURCE_CKSUM="$(cksum < "$INPUT_SOURCE")"
  cp "$INPUT_SOURCE" "$WORK_DIR/input.svg"
else
  INPUT_SOURCE="$WORK_DIR/input.svg"
  printf '%s\n' '<svg xmlns="http://www.w3.org/2000/svg" width="80" height="40"><rect x="1" y="1" width="10" height="10"/></svg>' > "$WORK_DIR/input.svg"
fi
INPUT_CKSUM="$(cksum < "$WORK_DIR/input.svg")"
printf '%s\n' '[{"type":"text","layer":"codex-generate","text":"NELISP-EMACS-GENERATE","p1":[4,5],"height":7},{"type":"line","layer":"codex-generate","p1":[1,1],"p2":[20,1]}]' > "$WORK_DIR/entities.json"
{
  printf '%s' '<svg xmlns="http://www.w3.org/2000/svg" width="80" height="40"><rect x="1" y="1" width="10" height="10"/><!--'
  i=0
  while [ "$i" -lt 9000 ]; do
    printf x
    i=$((i + 1))
  done
  printf '%s\n' '--><text class="codex-late">NELISP-EMACS-LATE</text></svg>'
} > "$WORK_DIR/late-input.svg"
printf '%s\n' '<svg xmlns="http://www.w3.org/2000/svg"><text class="codex-entity">A &amp; B</text></svg>' > "$WORK_DIR/entity-input.svg"

if [ "$SUITE" = "auto" ]; then
  if [ "$REAL_INPUT" -eq 1 ]; then
    SUITE="real-edit"
  else
    SUITE="full"
  fi
fi
case "$SUITE" in
  real-edit|annotate|outline|extract|basic|batch|entity|late|late-extract|late-batch|generate|full) ;;
  *)
    echo "[anvil-standalone-svg-smoke] FAIL: unknown suite: $SUITE" >&2
    exit 2
    ;;
esac
if [ "$REAL_INPUT" -eq 1 ] && [ "$SUITE" != "real-edit" ]; then
  echo "[anvil-standalone-svg-smoke] FAIL: real SVG mode only supports --suite real-edit" >&2
  exit 2
fi

RUN_IDENTITY=0
RUN_ANNOTATE=0
RUN_OUTLINE=0
RUN_EXTRACT=0
RUN_BATCH=0
RUN_ENTITY=0
RUN_LATE_EXTRACT=0
RUN_LATE_BATCH=0
RUN_GENERATE=0
case "$SUITE" in
  real-edit) RUN_IDENTITY=1; RUN_ANNOTATE=1; RUN_OUTLINE=1 ;;
  annotate) RUN_ANNOTATE=1 ;;
  outline) RUN_ANNOTATE=1; RUN_OUTLINE=1 ;;
  extract) RUN_ANNOTATE=1; RUN_EXTRACT=1 ;;
  basic) RUN_IDENTITY=1; RUN_ANNOTATE=1; RUN_OUTLINE=1; RUN_EXTRACT=1 ;;
  batch) RUN_ANNOTATE=1; RUN_BATCH=1 ;;
  entity) RUN_ENTITY=1 ;;
  late) RUN_LATE_EXTRACT=1; RUN_LATE_BATCH=1 ;;
  late-extract) RUN_LATE_EXTRACT=1 ;;
  late-batch) RUN_LATE_BATCH=1 ;;
  generate) RUN_GENERATE=1 ;;
  full) RUN_IDENTITY=1; RUN_ANNOTATE=1; RUN_OUTLINE=1; RUN_EXTRACT=1; RUN_BATCH=1; RUN_ENTITY=1; RUN_LATE_EXTRACT=1; RUN_LATE_BATCH=1; RUN_GENERATE=1 ;;
esac

if [ "$RUN_IDENTITY" -eq 1 ]; then
  "$ANVIL_BIN" standalone-os-identity --out "$WORK_DIR/os-identity.txt"
  case "$(uname -s 2>/dev/null || echo)" in
    Darwin*)
      grep -q ":system-type darwin" "$WORK_DIR/os-identity.txt" || {
        echo "[anvil-standalone-svg-smoke] FAIL: macOS standalone system-type missing" >&2
        exit 1
      }
      grep -q ':path-separator ":"' "$WORK_DIR/os-identity.txt" || {
        echo "[anvil-standalone-svg-smoke] FAIL: macOS standalone path-separator mismatch" >&2
        exit 1
      }
      ;;
    Linux*)
      grep -q ":system-type gnu/linux" "$WORK_DIR/os-identity.txt" || {
        echo "[anvil-standalone-svg-smoke] FAIL: Linux standalone system-type missing" >&2
        exit 1
      }
      grep -q ':path-separator ":"' "$WORK_DIR/os-identity.txt" || {
        echo "[anvil-standalone-svg-smoke] FAIL: Linux standalone path-separator mismatch" >&2
        exit 1
      }
      ;;
    MINGW*|MSYS*|CYGWIN*)
      grep -q ":system-type windows-nt" "$WORK_DIR/os-identity.txt" || {
        echo "[anvil-standalone-svg-smoke] FAIL: Windows standalone system-type missing" >&2
        exit 1
      }
      grep -q ':path-separator ";"' "$WORK_DIR/os-identity.txt" || {
        echo "[anvil-standalone-svg-smoke] FAIL: Windows standalone path-separator mismatch" >&2
        exit 1
      }
      ;;
  esac
fi

if [ "$RUN_ANNOTATE" -eq 1 ]; then
  "$ANVIL_BIN" cad-annotate-svg \
    --in "$WORK_DIR/input.svg" \
    --out "$WORK_DIR/output.svg" \
    --text NELISP-EMACS-SMOKE \
    --layer codex-smoke \
    --x 12 \
    --y 12 \
    --height 8
  grep -q "NELISP-EMACS-SMOKE" "$WORK_DIR/output.svg" || {
    echo "[anvil-standalone-svg-smoke] FAIL: annotation marker missing" >&2
    exit 1
  }
  if [ "$(cksum < "$WORK_DIR/input.svg")" != "$INPUT_CKSUM" ]; then
    echo "[anvil-standalone-svg-smoke] FAIL: input SVG was modified" >&2
    exit 1
  fi
  if [ "$REAL_INPUT" -eq 1 ] && [ "$(cksum < "$INPUT_SOURCE")" != "$INPUT_SOURCE_CKSUM" ]; then
    echo "[anvil-standalone-svg-smoke] FAIL: original SVG was modified" >&2
    exit 1
  fi
  if cmp -s "$WORK_DIR/input.svg" "$WORK_DIR/output.svg"; then
    echo "[anvil-standalone-svg-smoke] FAIL: output SVG is identical to input" >&2
    exit 1
  fi
fi

if [ "$RUN_OUTLINE" -eq 1 ]; then
  "$ANVIL_BIN" cad-read-outline-svg \
    --in "$WORK_DIR/output.svg" \
    --out "$WORK_DIR/outline.txt"
  grep -q ":format svg" "$WORK_DIR/outline.txt" || {
    echo "[anvil-standalone-svg-smoke] FAIL: outline does not report SVG" >&2
    exit 1
  }
fi

if [ "$RUN_EXTRACT" -eq 1 ]; then
  "$ANVIL_BIN" cad-extract-svg \
    --in "$WORK_DIR/output.svg" \
    --out "$WORK_DIR/extract.txt" \
    --type text
  grep -q "NELISP-EMACS-SMOKE" "$WORK_DIR/extract.txt" || {
    echo "[anvil-standalone-svg-smoke] FAIL: extract marker missing" >&2
    exit 1
  }
fi

if [ "$RUN_BATCH" -eq 1 ]; then
  "$ANVIL_BIN" cad-batch-update-svg \
    --in "$WORK_DIR/output.svg" \
    --out "$WORK_DIR/batch-output.svg" \
    --layer codex-smoke \
    --type text \
    --match-text NELISP-EMACS-SMOKE \
    --find NELISP-EMACS-SMOKE \
    --replace NELISP-EMACS-BATCH \
    --set-layer codex-batch
  grep -q "NELISP-EMACS-BATCH" "$WORK_DIR/batch-output.svg" || {
    echo "[anvil-standalone-svg-smoke] FAIL: batch marker missing" >&2
    exit 1
  }
  grep -q 'class="codex-batch"' "$WORK_DIR/batch-output.svg" || {
    echo "[anvil-standalone-svg-smoke] FAIL: batch layer missing" >&2
    exit 1
  }
  if cmp -s "$WORK_DIR/output.svg" "$WORK_DIR/batch-output.svg"; then
    echo "[anvil-standalone-svg-smoke] FAIL: batch output is identical to annotated output" >&2
    exit 1
  fi
fi

if [ "$RUN_ENTITY" -eq 1 ]; then
  "$ANVIL_BIN" cad-extract-svg \
    --in "$WORK_DIR/entity-input.svg" \
    --out "$WORK_DIR/entity-extract.txt" \
    --type text \
    --layer codex-entity
  grep -q ':text "A & B"' "$WORK_DIR/entity-extract.txt" || {
    echo "[anvil-standalone-svg-smoke] FAIL: entity extract decoded text missing" >&2
    exit 1
  }

  "$ANVIL_BIN" cad-batch-update-svg \
    --in "$WORK_DIR/entity-input.svg" \
    --out "$WORK_DIR/entity-batch-output.svg" \
    --layer codex-entity \
    --type text \
    --match-text "A & B" \
    --find "&" \
    --replace "and" \
    --set-layer codex-entity-batch
  grep -q "A and B" "$WORK_DIR/entity-batch-output.svg" || {
    echo "[anvil-standalone-svg-smoke] FAIL: entity batch marker missing" >&2
    exit 1
  }
  grep -q 'class="codex-entity-batch"' "$WORK_DIR/entity-batch-output.svg" || {
    echo "[anvil-standalone-svg-smoke] FAIL: entity batch layer missing" >&2
    exit 1
  }
fi

if [ "$RUN_LATE_EXTRACT" -eq 1 ]; then
  "$ANVIL_BIN" cad-extract-svg \
    --in "$WORK_DIR/late-input.svg" \
    --out "$WORK_DIR/late-extract.txt" \
    --type text \
    --layer codex-late
  grep -q "NELISP-EMACS-LATE" "$WORK_DIR/late-extract.txt" || {
    echo "[anvil-standalone-svg-smoke] FAIL: late extract marker missing" >&2
    exit 1
  }
fi

if [ "$RUN_LATE_BATCH" -eq 1 ]; then
  "$ANVIL_BIN" cad-batch-update-svg \
    --in "$WORK_DIR/late-input.svg" \
    --out "$WORK_DIR/late-batch-output.svg" \
    --layer codex-late \
    --type text \
    --match-text NELISP-EMACS-LATE \
    --find NELISP-EMACS-LATE \
    --replace NELISP-EMACS-LATE-BATCH \
    --set-layer codex-late-batch
  grep -q "NELISP-EMACS-LATE-BATCH" "$WORK_DIR/late-batch-output.svg" || {
    echo "[anvil-standalone-svg-smoke] FAIL: late batch marker missing" >&2
    exit 1
  }
  grep -q 'class="codex-late-batch"' "$WORK_DIR/late-batch-output.svg" || {
    echo "[anvil-standalone-svg-smoke] FAIL: late batch layer missing" >&2
    exit 1
  }
fi

if [ "$RUN_GENERATE" -eq 1 ]; then
  "$ANVIL_BIN" cad-generate-svg \
    --out "$WORK_DIR/generated.svg" \
    --entities-file "$WORK_DIR/entities.json" \
    --overwrite
  grep -q "NELISP-EMACS-GENERATE" "$WORK_DIR/generated.svg" || {
    echo "[anvil-standalone-svg-smoke] FAIL: generated marker missing" >&2
    exit 1
  }
fi

echo "[anvil-standalone-svg-smoke] PASS"
echo "input : $INPUT_SOURCE"
echo "work  : $WORK_DIR"
echo "suite : $SUITE"
if [ "$REAL_INPUT" -eq 1 ]; then
  echo "source-cksum: $INPUT_SOURCE_CKSUM"
  echo "copy-cksum  : $INPUT_CKSUM"
fi
if [ "$RUN_ANNOTATE" -eq 1 ]; then
  echo "output: $WORK_DIR/output.svg"
  if [ "$REAL_INPUT" -eq 1 ]; then
    echo "output-cksum: $(cksum < "$WORK_DIR/output.svg")"
  fi
fi
if [ "$RUN_BATCH" -eq 1 ]; then
  echo "batch : $WORK_DIR/batch-output.svg"
fi
if [ "$RUN_ENTITY" -eq 1 ]; then
  echo "entity-extract: $WORK_DIR/entity-extract.txt"
  echo "entity-batch  : $WORK_DIR/entity-batch-output.svg"
fi
if [ "$RUN_LATE_EXTRACT" -eq 1 ]; then
  echo "late-extract: $WORK_DIR/late-extract.txt"
fi
if [ "$RUN_LATE_BATCH" -eq 1 ]; then
  echo "late-batch  : $WORK_DIR/late-batch-output.svg"
fi
if [ "$RUN_GENERATE" -eq 1 ]; then
  echo "gen   : $WORK_DIR/generated.svg"
fi
