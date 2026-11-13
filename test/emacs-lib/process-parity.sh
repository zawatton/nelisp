#!/usr/bin/env bash
# process-parity.sh --- GNU Emacs 31.1 vs the NeLisp standalone on process behaviour
#
# Runs process-parity-cases.el on host `emacs -Q --batch' (reference) and on
# the standalone after build/nemacs-bootstrap.el (cwd = repo root, stdin
# </dev/null, ulimit -v 8388608), then compares the CASE rows.
# Usage: process-parity.sh [--only NAME] [--report]
#   (default)    exit 0 iff every row matches
#   --only NAME  run/compare just that case
#   --report     print rows, exit 0 whenever the run completes
# Env: NELISP_BIN (default target/nelisp), BUNDLE, EMACS (default emacs),
#      PARITY_SABOTAGE=1 corrupts one host expectation so the run must fail.

set -uo pipefail
root=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")/../.." && pwd)
cd "$root" || exit 2
bin=${NELISP_BIN:-$root/target/nelisp}
bundle=${BUNDLE:-$root/build/nemacs-bootstrap.el}
emacs=${EMACS:-emacs}
cases=$root/test/emacs-lib/process-parity-cases.el
only=""; report=0
while [[ $# -gt 0 ]]; do
  case $1 in
    --only) only=${2:-}; shift 2 ;;
    --report) report=1; shift ;;
    *) echo "usage: $0 [--only NAME] [--report]" >&2; exit 2 ;;
  esac
done
[[ -x "$bin" && -r "$bundle" ]] || { echo "process-parity: missing binary or bundle" >&2; exit 2; }

work=$(mktemp -d "${TMPDIR:-/tmp}/proc-parity.XXXXXX")
trap 'rm -rf "$work"' EXIT
home=$work/home; mkdir -p "$home"
printf '(setq pp--only %s)\n(load "%s" nil t t)\n(load "%s" nil t t)\n' \
  "$([[ -n $only ]] && printf '"%s"' "$only" || echo nil)" "$bundle" "$cases" > "$work/nl.el"
printf '(setq pp--only %s)\n(load "%s" nil t t)\n' \
  "$([[ -n $only ]] && printf '"%s"' "$only" || echo nil)" "$cases" > "$work/host.el"

( ulimit -v 8388608; HOME=$home timeout 200 "$emacs" -Q --batch -l "$work/host.el" </dev/null 2>/dev/null ) \
  | grep -E '^CASE ' > "$work/host.txt"
( ulimit -v 8388608; HOME=$home timeout 300 "$bin" --load "$work/nl.el" </dev/null 2>&1 ) \
  | grep -E '^CASE ' > "$work/nl.txt"

if [[ ${PARITY_SABOTAGE:-} == 1 ]]; then
  sed -i '0,/^CASE /s/^\(CASE [^ ]*\) .*/\1 "sabotaged"/' "$work/host.txt"
fi

[[ -s "$work/host.txt" ]] || { echo "process-parity: no host output" >&2; exit 2; }

fail=0
while IFS= read -r hrow; do
  name=${hrow#CASE }; name=${name%% *}
  nrow=$(grep -F -m1 "CASE $name " "$work/nl.txt" || true)
  if [[ "$hrow" == "$nrow" ]]; then echo "PASS $name"
  else echo "FAIL $name"; echo "  host:   ${hrow#CASE $name }"; echo "  nelisp: ${nrow#CASE $name }"; fail=1; fi
done < "$work/host.txt"
[[ $(grep -c '^CASE ' "$work/host.txt") == $(grep -c '^CASE ' "$work/nl.txt") ]] || { echo "FAIL row-count"; fail=1; }
[[ $report == 1 ]] && exit 0
exit $fail
