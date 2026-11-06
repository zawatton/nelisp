#!/bin/sh
# Usage: large-buffer-regex-ratchets.sh [--run | --check]
# --run   run each ratchet (EMACS=emacs-gtk timeout 900 make G) and write
#         tools/ai/evidence/large-buffer-regex-ratchets.txt (one "G rc=N" line each)
# --check pass iff the evidence file lists all ten ratchets with rc=0
ev=tools/ai/evidence/large-buffer-regex-ratchets.txt
gates="parens-check compile emacs-compat ns-gate ns-inventory fallback-inventory substrate-presence-corpus-check doc200-census unsafe-inventory emacs-parity"
case $1 in
  --run) : > "$ev"
    for g in $gates; do
      EMACS=emacs-gtk timeout 900 make "$g" > "${TMPDIR:-/tmp}/lbr-ratchet-$g.log" 2>&1
      echo "$g rc=$?" >> "$ev"
    done ;;
  --check) n=0
    for g in $gates; do grep -q "^$g rc=0\$" "$ev" 2>/dev/null && n=$((n+1)); done
    [ "$n" -eq 10 ] ;;
  *) echo "usage: $0 --run|--check" >&2; exit 2 ;;
esac
