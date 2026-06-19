#!/usr/bin/env bash
# Generic native artifact contract gate.
#
# This checks the user-facing invariant we need before Nelix can rely on
# precompiled elisp broadly:
#   - compile-elisp-artifacts can bulk-build adjacent .neln files
#   - --native-policy required proves every top-level defun entered native code
#   - audit-elisp-artifacts can report/reject native coverage gaps
#   - load/eval source commands prefer the adjacent native artifact
#   - eval/exec artifact commands can use generated artifacts without replaying
#     the full artifact command source
#   - native-exec can run a simple compiled defun from the generated artifact
set -euo pipefail

REPO_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
cd "$REPO_ROOT"

BUILD=1
NELISP="${NELISP:-$REPO_ROOT/target/nelisp}"
TMP_DIR="$(mktemp -d)"

cleanup() {
  rm -rf "$TMP_DIR"
}
trap cleanup EXIT

while [ "$#" -gt 0 ]; do
  case "$1" in
    --no-build) BUILD=0; shift ;;
    --nelisp) NELISP="$2"; shift 2 ;;
    *)
      echo "usage: $0 [--no-build] [--nelisp PATH]" >&2
      exit 2
      ;;
  esac
done

if [ "$BUILD" -eq 1 ]; then
  make standalone-reader
fi

if [ ! -x "$NELISP" ]; then
  echo "native_artifact_gate_fail reason=missing-nelisp path=$NELISP" >&2
  exit 1
fi

SRC_DIR="$TMP_DIR/src"
mkdir -p "$SRC_DIR/sub"

cat >"$SRC_DIR/hot.el" <<'EOF'
(defun native-contract-inc (x) (+ x 1))
(defun native-contract-double (x) (* x 2))
(provide 'native-contract-hot)
EOF

cat >"$SRC_DIR/sub/data.el" <<'EOF'
(defvar native-contract-data 42)
(provide 'native-contract-data)
EOF

run_timed() {
  local label="$1"; shift
  local out_file="$TMP_DIR/$label.out"
  local err_file="$TMP_DIR/$label.err"
  local start end rc
  start="$(date +%s%3N)"
  set +e
  "$@" >"$out_file" 2>"$err_file"
  rc=$?
  set -e
  end="$(date +%s%3N)"
  printf 'native_artifact_gate_result label=%s rc=%s ms=%s out=%s\n' \
    "$label" "$rc" "$((end - start))" \
    "$(tr '\n' ' ' <"$out_file" | sed 's/[[:space:]]*$//')"
  if [ "$rc" -ne 0 ]; then
    sed 's/^/native_artifact_gate_stderr /' "$err_file" >&2
    exit "$rc"
  fi
}

expect_out() {
  local label="$1" expected="$2"
  local actual
  actual="$(cat "$TMP_DIR/$label.out")"
  if [ "$actual" != "$expected" ]; then
    printf 'native_artifact_gate_fail label=%s reason=output-mismatch expected=%s actual=%s\n' \
      "$label" "$expected" "$actual" >&2
    exit 1
  fi
}

expect_file() {
  local path="$1"
  if [ ! -f "$path" ]; then
    echo "native_artifact_gate_fail reason=missing-file path=$path" >&2
    exit 1
  fi
}

run_timed bulk_required \
  "$NELISP" compile-elisp-artifacts --kind neln --native-policy required "$SRC_DIR"

expect_out bulk_required "compiled=2 failed=0 kind=neln"
expect_file "$SRC_DIR/hot.el.neln"
expect_file "$SRC_DIR/hot.el.neln.manifest.el"
expect_file "$SRC_DIR/sub/data.el.neln"
expect_file "$SRC_DIR/sub/data.el.neln.manifest.el"

if ! grep -q ':native-policy required' "$SRC_DIR/hot.el.neln.manifest.el"; then
  echo "native_artifact_gate_fail reason=missing-required-policy-manifest" >&2
  exit 1
fi

if ! grep -q ':native t' "$SRC_DIR/hot.el.neln.manifest.el"; then
  echo "native_artifact_gate_fail reason=missing-native-coverage" >&2
  exit 1
fi

run_timed audit_required \
  "$NELISP" audit-elisp-artifacts --required "$SRC_DIR"
if ! grep -q 'artifact_audit_summary status=ok' "$TMP_DIR/audit_required.out"; then
  echo "native_artifact_gate_fail reason=audit-required-summary-not-ok" >&2
  exit 1
fi

run_timed load_source_uses_neln \
  "$NELISP" load-elisp-source "$SRC_DIR/hot.el"
expect_out load_source_uses_neln "native-contract-hot"

run_timed eval_source_uses_neln \
  "$NELISP" eval-elisp-source "$SRC_DIR/hot.el" '(native-contract-inc 41)'
expect_out eval_source_uses_neln "42"

run_timed eval_artifact_direct \
  "$NELISP" eval-elisp-artifact "$SRC_DIR/hot.el.neln" '(native-contract-inc 41)'
expect_out eval_artifact_direct "42"

run_timed exec_artifact_direct \
  "$NELISP" exec-elisp-artifact "$SRC_DIR/hot.el.neln" \
  '(setq native-contract-direct 41)' \
  '(native-contract-inc native-contract-direct)'
expect_out exec_artifact_direct ""

run_timed native_exec \
  "$NELISP" native-exec-elisp-artifact "$SRC_DIR/hot.el.neln" native-contract-inc 41
expect_out native_exec "42"

BAD="$TMP_DIR/bad.el"
cat >"$BAD" <<'EOF'
(defun native-contract-bad (x) (+ x 1))
(provide 'native-contract-bad)
EOF

set +e
"$NELISP" compile-elisp-artifact --kind neln --native-policy required \
  --target wasm32-unknown --input "$BAD" --output "$BAD.neln" \
  >"$TMP_DIR/required_negative.out" 2>"$TMP_DIR/required_negative.err"
bad_rc=$?
set -e
printf 'native_artifact_gate_result label=required_negative rc=%s\n' "$bad_rc"
if [ "$bad_rc" -eq 0 ] || [ -e "$BAD.neln" ]; then
  echo "native_artifact_gate_fail label=required_negative reason=required-policy-did-not-fail" >&2
  exit 1
fi

MISSING="$TMP_DIR/missing.el"
cat >"$MISSING" <<'EOF'
(defun native-contract-missing (x) (+ x 1))
(provide 'native-contract-missing)
EOF

set +e
"$NELISP" audit-elisp-artifacts --required "$MISSING" \
  >"$TMP_DIR/audit_required_missing.out" 2>"$TMP_DIR/audit_required_missing.err"
audit_missing_rc=$?
set -e
printf 'native_artifact_gate_result label=audit_required_missing rc=%s out=%s\n' \
  "$audit_missing_rc" "$(tr '\n' ' ' <"$TMP_DIR/audit_required_missing.out" | sed 's/[[:space:]]*$//')"
if [ "$audit_missing_rc" -eq 0 ]; then
  echo "native_artifact_gate_fail label=audit_required_missing reason=required-audit-did-not-fail" >&2
  exit 1
fi
if ! grep -q 'artifact_audit_summary status=missing' "$TMP_DIR/audit_required_missing.out"; then
  echo "native_artifact_gate_fail label=audit_required_missing reason=missing-required-summary" >&2
  exit 1
fi

echo "native_artifact_gate_result label=native_artifact_gate rc=0"
