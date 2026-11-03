#!/bin/bash
# Doc 238 B1.1: the worktree carries a per-site dsubr cache (nc_dsubr_fill) and
# the callgrind evidence of the build that carries it beats b21: hash-get1024
# Ir/step below b21 and no dsubr row above b21; the B1 controls pass on it.
# Known red since 2026-11-01: rounds 1-3 (b23/b24/b26) never beat b21 and the
# cache was reverted (user decision), so the first test fails.
D=$(cd "$(dirname "$0")" && pwd); R=$(cd "$D/../../../.." && pwd)
grep -q '(defun nc_dsubr_fill' "$R/lisp/nelisp-cc-fxir-native.el" || exit 1
C=$D/cg-current-per-step.txt; [ -f "$C" ] || exit 1
awk -v b="$D/cg21-per-step.txt" 'BEGIN{while((getline l<b)>0){split(l,a," ");r[a[1]]=a[2]}}
  {if($1=="hash-get1024" && !($2<r[$1])) bad=1; if($1 ~ /hash|string|vector-allocate/ && $2>r[$1]) bad=1}
  END{exit bad}' "$C" || exit 1
[ "$(grep -c '^B1-.* ok=t' "$D/b26-checks.out")" -ge 10 ] && ! grep -q '^B1-.* ok=nil' "$D/b26-checks.out"
