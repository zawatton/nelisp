#!/bin/sh
# Doc 236 follow-up check: list ns-inventory findings inside
# packages/nl-typecheck and fail on any private-name escape, or on a
# prefix violation other than the documented user-facing `nelisp-typecheck-'
# settings (see docs/design/236-gradual-type-checking.org).
cd "$(dirname "$0")/../.." || exit 2
EMACS=${EMACS:-emacs}
out=$($EMACS --batch -Q -L packages/nl-prelude/src -L packages/nl-ns/src --eval '
(progn
  (load (expand-file-name "tools/nl-ns-inventory.el") nil t)
  (nl-ns-inventory--declare-overrides)
  (let ((fs (nl-ns-check-files (nl-ns-inventory--files))) (n 0))
    (dolist (k (list (quote ns-private-escape) (quote ns-prefix-violation)))
      (dolist (f (nl-ns-findings-of-kind fs k))
        (let ((file (plist-get f :file)) (sym (symbol-name (plist-get f :subject))))
          (when (and (string-prefix-p "packages/nl-typecheck/" file)
                     (not (and (eq k (quote ns-prefix-violation))
                               (string-prefix-p "nelisp-typecheck-" sym))))
            (setq n (1+ n))
            (princ (format "OFFENDER %s %s %s\n" k file sym))))))
    (princ (format "GATE-COUNT findings=%d\n" n))))' 2>&1) || { printf "%s\n" "$out" | grep -vE "^ +(eval|command-line|normal-top|\(progn)" | tail -4 | cut -c1-200; echo "typecheck-ns-escapes: emacs failed"; exit 2; }
printf '%s\n' "$out" | grep -E '^(OFFENDER|GATE-COUNT)'
printf '%s\n' "$out" | grep -q '^GATE-COUNT findings=0$'
