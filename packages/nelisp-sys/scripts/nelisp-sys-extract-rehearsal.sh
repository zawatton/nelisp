#!/usr/bin/env bash
# nelisp-sys-extract-rehearsal.sh --- Doc 130 Stage 130.7 extraction rehearsal
# SPDX-License-Identifier: GPL-3.0-or-later
set -euo pipefail

# ---------------------------------------------------------------------------
# Resolve repo root and package dir from the script's own location.
# ---------------------------------------------------------------------------
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PKG_DIR="$(cd "${SCRIPT_DIR}/.." && pwd)"
REPO_ROOT="$(cd "${PKG_DIR}/../.." && pwd)"

echo "nelisp-sys extraction rehearsal"
echo "  package : ${PKG_DIR}"
echo "  repo    : ${REPO_ROOT}"
echo ""

# ---------------------------------------------------------------------------
# Step 1 — Copy the package into target/nelisp-sys-extract/ (dry-run of
# what a real git-filter-branch / subtree extraction would produce).
# ---------------------------------------------------------------------------
DEST="${REPO_ROOT}/target/nelisp-sys-extract"
rm -rf "${DEST}"
cp -r "${PKG_DIR}" "${DEST}"
echo "Copied packages/nelisp-sys/ -> target/nelisp-sys-extract/"

# ---------------------------------------------------------------------------
# Step 2 — Boundary audit: no src/*.el file except the adapter may reference
# private NeLisp backend symbols (Doc 130 extraction criterion 6).
# ---------------------------------------------------------------------------
PRIVATE_PREFIXES=(
    "nelisp-phase47-"
    "nelisp-asm-"
    "nelisp-elf-"
    "nelisp-mach-o-"
    "nelisp-pe-"
    "nelisp-link-"
    "nelisp-crt0"
    "nelisp-sexp-"
    "nelisp-syscall-"
)

SRC_DIR="${PKG_DIR}/src"
ADAPTER_FILE="${SRC_DIR}/nelisp-sys-adapter-nelisp.el"

leak_found=0
leak_report=""

for el in "${SRC_DIR}"/*.el; do
    basename_el="$(basename "${el}")"
    if [ "${basename_el}" = "nelisp-sys-adapter-nelisp.el" ]; then
        continue
    fi
    for prefix in "${PRIVATE_PREFIXES[@]}"; do
        matches="$(grep -n "${prefix}" "${el}" 2>/dev/null || true)"
        if [ -n "${matches}" ]; then
            leak_found=1
            while IFS= read -r line; do
                leak_report="${leak_report}  ${basename_el}:${line} (prefix: ${prefix})"$'\n'
            done <<< "${matches}"
        fi
    done
done

echo ""
if [ "${leak_found}" -eq 0 ]; then
    echo "PASS: no private backend references outside the adapter"
else
    echo "FAIL: private backend references found outside the adapter:"
    echo "${leak_report}"
fi

# ---------------------------------------------------------------------------
# Step 3 — Adapter call budget: count public adapter defuns (criterion 5).
# ---------------------------------------------------------------------------
adapter_count="$(grep -cE '^\(defun nelisp-sys-adapter-' "${ADAPTER_FILE}" 2>/dev/null || true)"
echo "adapter public calls: ${adapter_count} (<20 required)"

# ---------------------------------------------------------------------------
# Exit code
# ---------------------------------------------------------------------------
if [ "${leak_found}" -ne 0 ]; then
    echo ""
    echo "RESULT: FAIL (private leaks detected)"
    exit 1
fi

if [ "${adapter_count}" -ge 20 ]; then
    echo ""
    echo "RESULT: FAIL (adapter call budget exceeded: ${adapter_count} >= 20)"
    exit 1
fi

echo ""
echo "RESULT: PASS"
exit 0
