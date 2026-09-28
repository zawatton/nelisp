#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "$0")/.." && pwd)"
binary="${1:-$repo_root/target/nelisp}"
source="(progn
  (defalias 'sfp-alias 'if)
  (let ((if-before (special-form-p 'if))
        (alias-before (special-form-p 'sfp-alias)))
    (fset 'if 'ignore)
    (list (special-form-p 'if)
          if-before
          (special-form-p 'lambda)
          (special-form-p 'when)
          (special-form-p 'car)
          (special-form-p 'sfp-never-defined)
          (special-form-p 7)
          (special-form-p nil)
          alias-before
          (special-form-p 'sfp-alias))))"

host="$(emacs --batch -Q --eval "(princ $source)")"
standalone="$("$binary" --eval "$source")"
if [[ "$host" != "$standalone" ]]; then
  printf 'Host:       %s\nStandalone: %s\n' "$host" "$standalone" >&2
  exit 1
fi
printf 'PASS special-form-p parity: %s\n' "$host"

unbound_predicates="(progn
  (fmakunbound 'if)
  (list (special-form-p 'if) (fboundp 'if)))"
host_unbound_predicates="$(emacs --batch -Q --eval "(princ $unbound_predicates)")"
standalone_unbound_predicates="$("$binary" --eval "$unbound_predicates")"
if [[ "$host_unbound_predicates" != "(nil nil)" || \
      "$standalone_unbound_predicates" != "$host_unbound_predicates" ]]; then
  printf 'fmakunbound predicates: Host=%s Standalone=%s\n' \
    "$host_unbound_predicates" "$standalone_unbound_predicates" >&2
  exit 1
fi
printf 'PASS fmakunbound predicates: %s\n' "$host_unbound_predicates"

# Known gap: standalone syntax dispatch may still execute `if' despite its
# function-cell tombstone. Keep this visible without weakening predicate parity.
host_unbound_eval="$(emacs --batch -Q --eval "(princ (condition-case e (progn (fmakunbound 'if) (eval '(if t 11 22) t)) (void-function 'void-function)))" 2>/dev/null)"
standalone_unbound_eval="$("$binary" --eval "(progn (fmakunbound 'if) (eval '(if t 11 22) t))" 2>/dev/null || true)"
if [[ "$host_unbound_eval" != "void-function" || "$standalone_unbound_eval" != "11" ]]; then
  printf 'Unexpected fmakunbound syntax behavior: Host=%s Standalone=%s\n' \
    "$host_unbound_eval" "$standalone_unbound_eval" >&2
  exit 1
fi
printf 'KNOWN GAP fmakunbound syntax dispatch: Host=%s Standalone=%s\n' \
  "$host_unbound_eval" "$standalone_unbound_eval"
