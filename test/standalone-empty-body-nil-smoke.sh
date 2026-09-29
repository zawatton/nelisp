#!/usr/bin/env bash
# Empty-body special forms must answer nil, never the previous sibling
# form's value.  The native special-form dispatchers write their result into
# a result slot that callers reuse across sibling forms, so a path that never
# stores nil for an empty body used to leak the previous sibling's value:
#   (list (list 1) (progn))  =>  ((1) (1))   instead of  ((1) nil)
# Forms covered: progn, let, let*, setq, catch, save-excursion,
# unwind-protect, condition-case (empty handler), when/unless (empty body),
# and if.  Expected values are what GNU Emacs answers (compared live when a
# host emacs is available, and against the literal `nil' regardless).
# Both the interpreted path and host-byte-compiled code run.
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
binary="${NELISP_BIN:-$repo_root/target/nelisp}"
host_emacs="${HOST_EMACS:-emacs}"
tmp_dir="${KEEP_TMP:-$(mktemp -d)}"
[[ -n "${KEEP_TMP:-}" ]] || trap 'rm -rf "$tmp_dir"' EXIT

fail() {
  echo "standalone-empty-body-nil-smoke: FAIL -- $1" >&2
  exit 1
}

if [[ ! -x "$binary" ]]; then
  echo "standalone-empty-body-nil-smoke: missing executable: $binary" >&2
  exit 2
fi

forms="$tmp_dir/forms.txt"
cat >"$forms" <<'FORMS'
(progn)
(progn (progn))
(let ())
(let ((x 1)))
(let* ())
(let* ((x 1)))
(setq)
(catch 'x)
(catch 'x (progn))
(save-excursion)
(unwind-protect nil)
(condition-case nil (signal 'error nil) (error))
(condition-case nil (progn) (error 1))
(when t)
(unless nil)
(if nil 1)
(if t (progn))
(let ((x 1)) (progn))
(eval '(progn))
FORMS

# One process, one condition-case per form, so startup cost is paid once.
lisp_forms="$(awk '{gsub(/\\/,"\\\\"); gsub(/"/,"\\\""); printf "\"%s\" ", $0}' "$forms")"
{
  echo ";;; -*- lexical-binding: t; -*-"
  echo "(defvar eb--forms '($lisp_forms))"
  cat <<'DRIVER_EOF'
(defun eb--run (text)
  (condition-case e
      (eval (car (read-from-string (concat "(list (list 1) " text ")"))) t)
    (error (list 'ERR (car e)))))
(dolist (text eb--forms)
  (princ (format "%s => %S\n" text (eb--run text))))
DRIVER_EOF
} >"$tmp_dir/run.el"

# `--load' echoes the file's last value; keep only the per-form result lines.
timeout 120 "$binary" --load "$tmp_dir/run.el" -- >"$tmp_dir/raw.out" 2>"$tmp_dir/actual.err" \
  || { cat "$tmp_dir/actual.err" >&2; fail "standalone run failed"; }
grep ' => ' "$tmp_dir/raw.out" >"$tmp_dir/actual.out" || true

# Literal expectation: every form answers nil after the (1) sibling.
bad="$(grep -v ' => ((1) nil)$' "$tmp_dir/actual.out" || true)"
[[ -z "$bad" ]] || fail "stale value leaked from an empty body:
$bad"
[[ "$(wc -l <"$tmp_dir/actual.out")" -eq "$(wc -l <"$forms")" ]] \
  || fail "unexpected output line count: $(cat "$tmp_dir/actual.out")"

if command -v "$host_emacs" >/dev/null 2>&1; then
  "$host_emacs" --batch -l "$tmp_dir/run.el" >"$tmp_dir/host.out" 2>/dev/null \
    || fail "host emacs run failed"
  diff -u "$tmp_dir/host.out" "$tmp_dir/actual.out" >&2 \
    || fail "standalone differs from host $host_emacs"

  # Byte-compiled context: code compiled by the host, run by the standalone
  # bytecode VM.
  cat >"$tmp_dir/hostbc.el" <<'HOSTBC_EOF'
;;; -*- lexical-binding: t; -*-
(dolist (f '((progn) (let ()) (let ((x 1))) (setq) (catch 'x) (save-excursion)
             (unwind-protect nil) (condition-case nil (signal 'error nil) (error))))
  (let ((c (byte-compile `(lambda () (list (list 1) ,f)))))
    ;; Emit codes as integers: a printed unibyte string ("\\3012") is
    ;; ambiguous for octal escapes followed by a digit.
    (princ (format "(funcall (make-byte-code 0 (unibyte-string %s) %S %S))\n"
                   (mapconcat #'number-to-string (append (aref c 1) nil) " ")
                   (aref c 2) (aref c 3)))))
HOSTBC_EOF
  "$host_emacs" --batch -l "$tmp_dir/hostbc.el" >"$tmp_dir/bc.txt" 2>/dev/null \
    || fail "host byte-compile failed"
  n=0
  while IFS= read -r expr; do
    n=$((n + 1))
    got="$(timeout 60 "$binary" --eval "$expr" | head -1)"
    [[ "$got" == "((1) nil)"* ]] || fail "byte-compiled case $n answered: $got"
  done <"$tmp_dir/bc.txt"
  [[ "$n" -eq 8 ]] || fail "expected 8 byte-compiled cases, got $n"
else
  echo "standalone-empty-body-nil-smoke: SKIP host comparison + byte-compiled (no $host_emacs)" >&2
fi

echo "standalone-empty-body-nil-smoke: PASS"
