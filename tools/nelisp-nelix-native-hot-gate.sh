#!/usr/bin/env bash
# Nelix hot native module gate for Doc 154 Stage D.
#
# This gate proves the current Nelix native hot modules use the common .neln
# artifact contract with required native coverage.  Host Emacs is used for the
# build step because target/nelisp compile-elisp-artifact is still dominated by
# the standalone artifact compiler/reader fixed cost for these files.  The
# resulting artifacts are then audited and executed through target/nelisp.
set -euo pipefail

REPO_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
cd "$REPO_ROOT"

NELIX_REPO="${NELIX_REPO:-$REPO_ROOT/../anvil-pkg}"
NELISP="${NELISP:-$REPO_ROOT/target/nelisp}"
EMACS="${EMACS:-emacs}"
TMP_DIR="$(mktemp -d)"

cleanup() {
  rm -rf "$TMP_DIR"
}
trap cleanup EXIT

while [ "$#" -gt 0 ]; do
  case "$1" in
    --nelix-repo) NELIX_REPO="$2"; shift 2 ;;
    --nelisp) NELISP="$2"; shift 2 ;;
    --emacs) EMACS="$2"; shift 2 ;;
    *)
      echo "usage: $0 [--nelix-repo PATH] [--nelisp PATH] [--emacs EMACS]" >&2
      exit 2
      ;;
  esac
done

if [ ! -x "$NELISP" ]; then
  echo "nelix_native_hot_gate_fail reason=missing-nelisp path=$NELISP" >&2
  exit 1
fi

if [ ! -f "$NELIX_REPO/scripts/nelix-aot-native-subset.el" ]; then
  echo "nelix_native_hot_gate_fail reason=missing-nelix-repo path=$NELIX_REPO" >&2
  exit 1
fi

for tool in cc objcopy; do
  if ! command -v "$tool" >/dev/null 2>&1; then
    echo "nelix_native_hot_gate_fail reason=missing-tool tool=$tool" >&2
    exit 1
  fi
done

SRC_DIR="$TMP_DIR/src"
mkdir -p "$SRC_DIR"
cp "$NELIX_REPO/scripts/nelix-aot-native-cli-proof.el" "$SRC_DIR/"
cp "$NELIX_REPO/scripts/nelix-aot-native-subset.el" "$SRC_DIR/"

CLI_SRC="$SRC_DIR/nelix-aot-native-cli-proof.el"
SUBSET_SRC="$SRC_DIR/nelix-aot-native-subset.el"
CLI_ART="$CLI_SRC.neln"
SUBSET_ART="$SUBSET_SRC.neln"

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
  printf 'nelix_native_hot_gate_result label=%s rc=%s ms=%s out=%s\n' \
    "$label" "$rc" "$((end - start))" \
    "$(tr '\n' ' ' <"$out_file" | sed 's/[[:space:]]*$//')"
  if [ "$rc" -ne 0 ]; then
    sed 's/^/nelix_native_hot_gate_stderr /' "$err_file" >&2
    exit "$rc"
  fi
}

expect_out() {
  local label="$1" expected="$2"
  local actual
  actual="$(cat "$TMP_DIR/$label.out")"
  if [ "$actual" != "$expected" ]; then
    printf 'nelix_native_hot_gate_fail label=%s reason=output-mismatch expected=%s actual=%s\n' \
      "$label" "$expected" "$actual" >&2
    exit 1
  fi
}

expect_grep() {
  local label="$1" pattern="$2"
  if ! grep -Eq "$pattern" "$TMP_DIR/$label.out"; then
    echo "nelix_native_hot_gate_fail label=$label reason=missing-pattern pattern=$pattern" >&2
    sed 's/^/nelix_native_hot_gate_stdout /' "$TMP_DIR/$label.out" >&2
    sed 's/^/nelix_native_hot_gate_stderr /' "$TMP_DIR/$label.err" >&2
    exit 1
  fi
}

verify_required_manifest() {
  local label="$1" manifest="$2"
  if ! grep -q ':native-policy required' "$manifest"; then
    echo "nelix_native_hot_gate_fail label=$label reason=missing-required-policy" >&2
    exit 1
  fi
  if ! grep -q ':native-report' "$manifest"; then
    echo "nelix_native_hot_gate_fail label=$label reason=missing-native-report" >&2
    exit 1
  fi
  if grep -q ':native nil' "$manifest"; then
    echo "nelix_native_hot_gate_fail label=$label reason=native-gap" >&2
    exit 1
  fi
}

compile_required() {
  local source="$1" artifact="$2"
  "$EMACS" -Q --batch \
    -L "$REPO_ROOT/lisp" \
    -L "$REPO_ROOT/src" \
    --eval '(setq load-prefer-newer t)' \
    --eval '(require (quote nelisp-artifact))' \
    --eval "(nelisp-artifact-compile-file \"$source\" \"$artifact\" nil nil nil nil nil (quote neln) (quote required))"
}

run_timed host_compile_cli_required compile_required "$CLI_SRC" "$CLI_ART"
verify_required_manifest cli "$CLI_ART.manifest.el"

run_timed host_compile_subset_required compile_required "$SUBSET_SRC" "$SUBSET_ART"
verify_required_manifest subset "$SUBSET_ART.manifest.el"

run_timed standalone_audit_cli_required \
  timeout 60 "$NELISP" audit-elisp-artifacts --required "$CLI_SRC"
expect_grep standalone_audit_cli_required 'artifact_audit_summary status=ok'
expect_grep standalone_audit_cli_required 'defuns=9'
expect_grep standalone_audit_cli_required 'gaps=0'

line_payload="$(printf 'NELIX-AOT-MANIFEST-V1\ntarget\tmagit\tmagit\npin\tripgrep\ninstalled\tmagit\nend\n')"
run_timed standalone_cli_native_exec \
  timeout 120 "$NELISP" native-exec-elisp-artifact "$CLI_ART" \
  nelix-aot-native-cli-proof-code "$line_payload"
expect_out standalone_cli_native_exec "556"

id_audit_payload="$(printf 'NELIX-AOT-MANIFEST-V1\ntarget-id\t2\t2\ntarget-id\t4\t4\npin-id\t3\ninstalled-id\t2\nend\n')"
run_timed standalone_subset_audit_id_native \
  timeout 120 "$NELISP" native-exec-elisp-artifact "$SUBSET_ART" \
  nelix-aot-native-builder-audit-id-report-proof "$id_audit_payload" ""
expect_out standalone_subset_audit_id_native '"ok\tfalse\npresent\tripgrep\nmissing\tbat\nbackend\tnix\n"'

id_upgrade_payload="$(printf 'NELIX-AOT-MANIFEST-V1\ntarget-id\t1\t1\ntarget-id\t2\t2\ntarget-id\t3\t3\npin-id\t2\ninstalled-id\t1\ninstalled-id\t2\nend\n')"
run_timed standalone_subset_upgrade_id_native \
  timeout 120 "$NELISP" native-exec-elisp-artifact "$SUBSET_ART" \
  nelix-aot-native-builder-upgrade-id-report-proof "$id_upgrade_payload" ""
expect_out standalone_subset_upgrade_id_native '"upgrade\tmagit\npinned\tripgrep\nmissing\tfd\nbackend\tnix\n"'

echo "nelix_native_hot_gate_result label=nelix_native_hot_gate rc=0"
