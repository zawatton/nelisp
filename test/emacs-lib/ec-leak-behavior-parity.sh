#!/usr/bin/env bash
# ec-leak-behavior-parity.sh --- GNU Emacs vs NeLisp on the 13 formerly leaking commands
#
# Runs ec-leak-behavior-parity-cases.el on host `emacs -Q --batch' (the
# reference) and on the standalone after the bootstrap bundle, then compares
# the CASE rows.  Exit 0 iff every row matches.
# Env: NELISP_BIN (default target/nelisp), BUNDLE (default build/nemacs-bootstrap.el),
#      EMACS (default emacs), PARITY_SABOTAGE=1 corrupts one host row so the
#      checker must fail (negative control).

set -uo pipefail
root=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")/../.." && pwd)
cd "$root" || exit 2
bin=${NELISP_BIN:-$root/target/nelisp}
bundle=${BUNDLE:-$root/build/nemacs-bootstrap.el}
emacs=${EMACS:-emacs}
cases=$root/test/emacs-lib/ec-leak-behavior-parity-cases.el
[[ -x "$bin" && -r "$bundle" ]] || { echo "parity: missing binary or bundle" >&2; exit 2; }

work=$(mktemp -d "${TMPDIR:-/tmp}/ec-parity.XXXXXX")
trap 'rm -rf "$work"' EXIT
printf '(load "%s" nil t t)\n(load "%s" nil t t)\n' "$bundle" "$cases" > "$work/nl.el"

( ulimit -v 8388608; timeout 200 "$emacs" -Q --batch -l "$cases" </dev/null 2>/dev/null ) \
  | grep -E '^(CASE|MODE) ' > "$work/host.txt"
( ulimit -v 8388608; timeout 400 "$bin" --load "$work/nl.el" </dev/null 2>&1 ) \
  | grep -E '^(CASE|MODE) ' > "$work/nl.txt"

if [[ ${PARITY_SABOTAGE:-} == 1 ]]; then
  sed -i '0,/^CASE forward-paragraph /s/^CASE forward-paragraph .*/CASE forward-paragraph (999 . "sabotaged")/' "$work/host.txt"
fi

[[ -s "$work/host.txt" && -s "$work/nl.txt" ]] || { echo "parity: no output (host=$(wc -l <"$work/host.txt") nelisp=$(wc -l <"$work/nl.txt"))" >&2; exit 2; }

fail=0
while IFS= read -r hrow; do
  name=${hrow#CASE }; name=${name%% *}
  nrow=$(grep -F -m1 "CASE $name " "$work/nl.txt" || true)
  if [[ "$hrow" == MODE* ]]; then continue; fi
  if [[ "$hrow" == "$nrow" ]]; then echo "PASS $name"
  else echo "FAIL $name"; echo "  host:   ${hrow#CASE $name }"; echo "  nelisp: ${nrow#CASE $name }"; fail=1; fi
done < <(grep '^CASE ' "$work/host.txt")
diff <(grep '^MODE ' "$work/host.txt") <(grep '^MODE ' "$work/nl.txt") >/dev/null \
  && echo "PASS font-lock-mode-flag" || { echo "FAIL font-lock-mode-flag"; fail=1; }
[[ $(grep -c '^CASE ' "$work/host.txt") == $(grep -c '^CASE ' "$work/nl.txt") ]] || { echo "FAIL row-count"; fail=1; }
exit $fail
