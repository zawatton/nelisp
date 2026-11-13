#!/usr/bin/env bash
# ec-leak-sweep.sh --- run test/emacs-lib/ec-leak-sweep.el on the standalone
#
# Usage: test/emacs-lib/ec-leak-sweep.sh [--count | --owner | --list]
#   (default)  exit 0 iff the control is detected and there are no leaks
#   --count    print the leak count; exit 0 iff the control is detected
#   --owner    exit 0 iff every buffer constructor returns `nelisp-buffer'
#   --list     print the EC-LEAK rows
# Env:
#   NELISP_BIN  standalone binary (default: target/nelisp)
#   BUNDLE      bootstrap bundle (default: build/nemacs-bootstrap.el;
#               `make -f mk/emacs-lib.mk build-nelisp-bootstrap')
#   EC_SWEEP_OUT  keep the raw output here (default: a temp file)

set -uo pipefail
root=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")/../.." && pwd)
cd "$root" || exit 2
bin=${NELISP_BIN:-$root/target/nelisp}
bundle=${BUNDLE:-$root/build/nemacs-bootstrap.el}
mode=${1:-}
[[ -x "$bin" ]] || { echo "ec-leak-sweep: no binary at $bin" >&2; exit 2; }
[[ -r "$bundle" ]] || { echo "ec-leak-sweep: no bundle at $bundle" >&2; exit 2; }

work=$(mktemp -d "${TMPDIR:-/tmp}/ec-leak-sweep.XXXXXX")
trap 'rm -rf "$work"' EXIT
out=${EC_SWEEP_OUT:-$work/out.txt}

# Every name an emacs-lib install gate may define.
grep -rhoE "install-function-p '[^ )]+" packages/emacs-lib-*/src \
  | sed "s/.*'//" | sort -u > "$work/names.txt"
printf '(load "%s" nil t t)\n(load "%s" nil t t)\n' \
  "$bundle" "$root/test/emacs-lib/ec-leak-sweep.el" > "$work/run.el"
( ulimit -v 8388608; SWEEP_NAMES="$work/names.txt" \
    timeout 540 "$bin" --load "$work/run.el" < /dev/null ) > "$out" 2>&1

summary=$(grep -m1 '^EC-LEAK-SUMMARY ' "$out") \
  || { echo "ec-leak-sweep: run did not finish" >&2; tail -5 "$out" >&2; exit 2; }
leaks=$(sed -E 's/.*leaks=([0-9]+).*/\1/' <<<"$summary")
control_ok=0; grep -q '^EC-LEAK-CONTROL PASS$' "$out" && control_ok=1

case "$mode" in
  --count) echo "$leaks"; [[ $control_ok == 1 ]] ;;
  --owner) grep '^BUFFER-TYPE' "$out"; grep -q '^BUFFER-OWNER PASS$' "$out" ;;
  --list) grep '^EC-LEAK	' "$out"; echo "$summary" ;;
  *) echo "$summary control=$([[ $control_ok == 1 ]] && echo ok || echo BROKEN)"
     [[ $control_ok == 1 && $leaks == 0 ]] ;;
esac
