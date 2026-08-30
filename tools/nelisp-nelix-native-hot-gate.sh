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

NELIX_REPO="${NELIX_REPO:-$REPO_ROOT/../nelix}"
NELISP="${NELISP:-$REPO_ROOT/target/nelisp}"
EMACS="${EMACS:-emacs}"
TMP_DIR="$(mktemp -d)"
CHECKED=0

# `nelisp-ai.sh gate' requires a `GATE-COUNT checked=<n> findings=<n>' line
# on every path out of this script, success or failure -- its absence reads
# as "did not report what it checked", not as a pass.  An EXIT trap covers
# every `exit' call below (including a failing `run_timed'/`expect_*'
# assertion) without repeating the line at each call site.  `$?' inside an
# EXIT trap is the status that triggered it, captured before anything else
# in this handler can change it.
cleanup() {
  status=$?
  findings=0
  [ "$status" -eq 0 ] || findings=1
  printf 'GATE-COUNT checked=%s findings=%s\n' "$CHECKED" "$findings"
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

# Needs a host that can run the configured target; ask the build script's
# own predicate rather than keeping a uname table here (same convention as
# tools/selfhost-test.sh).  This must run BEFORE the missing-nelisp check
# below: on an unrunnable host (2026-08-23 Windows inventory) `target/
# nelisp' is a linux-x86_64 ELF that a bare `[ -x ]' test cannot tell apart
# from a working binary, so without this check the gate reported
# "missing-nelisp" -- a hard FAIL -- for what is really an unrunnable
# target, before ever reaching the (already-correct) `../nelix' absence
# skip below.
set +e
"$EMACS" --batch -Q -L "$REPO_ROOT/lisp" -L "$REPO_ROOT/src" -L "$REPO_ROOT/scripts" \
  -l nelisp-standalone-build \
  --eval '(kill-emacs (if (nelisp-standalone--target-runnable-on-host-p) 0 3))' \
  >/dev/null 2>&1
host_rc=$?
set -e
case "$host_rc" in
  0) ;;
  3)
    echo "GATE-SKIP target ${NELISP_STANDALONE_TARGET:-linux-x86_64} cannot run on host $(uname -s)/$(uname -m)"
    echo "nelix_native_hot_gate_result label=nelix_native_hot_gate rc=0 skipped=1"
    exit 0
    ;;
  *)
    echo "nelix_native_hot_gate_fail reason=cannot-ask-host-runnability rc=$host_rc" >&2
    exit 1
    ;;
esac

if [ ! -x "$NELISP" ]; then
  echo "nelix_native_hot_gate_fail reason=missing-nelisp path=$NELISP" >&2
  exit 1
fi

# Three outcomes, not two.  This used to exit 1 when the sibling checkout is
# absent, which `nelisp-ai.sh gate' can only read as a failure -- so a gate
# that cannot run here looked identical to one that ran and found something.
# GATE-SKIP records it as a reasoned skip, which `verify' accepts for a
# required gate.  "The repo is missing" and "the gate failed" are different
# facts and now print differently.
if [ ! -f "$NELIX_REPO/scripts/nelix-aot-native-subset.el" ]; then
  echo "GATE-SKIP nelix checkout absent (looked for $NELIX_REPO)"
  echo "nelix_native_hot_gate_result label=nelix_native_hot_gate rc=0 skipped=1"
  exit 0
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
  CHECKED=$((CHECKED + 1))
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
expect_grep standalone_audit_cli_required 'defuns=[1-9][0-9]* native=[1-9][0-9]*'
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
# The expected value below must contain real tab/newline bytes, not the
# two-character sequences `\t'/`\n'.  Emacs Lisp `prin1'/`prin1-to-string'
# do not escape embedded tab or newline characters by default (only
# `print-escape-newlines' controls the latter, and it is nil here) -- a
# string prints as a double-quoted literal with its control characters
# left untouched.  Verified against both stock Emacs 30.1 and this
# artifact's actual standalone output on 2026-08-21; a literal
# `\t'/`\n' pair here never matches either.
expect_out standalone_subset_audit_id_native \
  "$(printf '"ok\tfalse\npresent\tripgrep\nmissing\tbat\nbackend\tnix\n"')"

id_upgrade_payload="$(printf 'NELIX-AOT-MANIFEST-V1\ntarget-id\t1\t1\ntarget-id\t2\t2\ntarget-id\t3\t3\npin-id\t2\ninstalled-id\t1\ninstalled-id\t2\nend\n')"
run_timed standalone_subset_upgrade_id_native \
  timeout 120 "$NELISP" native-exec-elisp-artifact "$SUBSET_ART" \
  nelix-aot-native-builder-upgrade-id-report-proof "$id_upgrade_payload" ""
# Same real-byte-vs-escape-sequence fix as standalone_subset_audit_id_native
# above.
expect_out standalone_subset_upgrade_id_native \
  "$(printf '"upgrade\tmagit\npinned\tripgrep\nmissing\tfd\nbackend\tnix\n"')"

echo "nelix_native_hot_gate_result label=nelix_native_hot_gate rc=0"
