#!/usr/bin/env bash
set -euo pipefail

script_dir="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
repo_root="$(cd -- "$script_dir/.." && pwd)"
audit_script="$repo_root/scripts/real-init-audit.sh"
test_dir="$(mktemp -d)"
trap 'rm -rf -- "$test_dir"' EXIT

helper_body="$test_dir/helper-body.el"
helper_file="$test_dir/helper.el"
dummy_init="$test_dir/dummy-init.el"

awk '
  /^ELISP$/ { if (copying) exit }
  copying { print }
  /done <<'"'"'ELISP'"'"'$/ { copying = 1 }
' "$audit_script" > "$helper_body"

if [[ ! -s "$helper_body" ]]; then
  echo "real-init-audit-trace-test: failed to extract generated helper" >&2
  exit 1
fi
if rg -n ';' "$helper_body"; then
  echo "real-init-audit-trace-test: generated helper contains a semicolon" >&2
  exit 1
fi
if rg -n '\(read-from-string source position\)' "$helper_body"; then
  echo "real-init-audit-trace-test: helper still uses unbounded source reader" >&2
  exit 1
fi

{
  printf '%s' '(progn '
  while IFS= read -r helper_line; do
    printf '%s ' "$helper_line"
  done < "$helper_body"
  printf '%s\n' ' )'
  printf '%s\n' '(defun nl-syscall-read-file (path _start _end) (with-temp-buffer (insert-file-contents path) (buffer-string)))'
  printf '%s\n' '(defun emacs-load--byte-indexed-source (source) source)'
  printf '%s\n' '(defun emacs-load--artifact-source-form-end (source position) (cdr (read-from-string source position)))'
  printf '%s\n' '(defun emacs-load--reader-slice (source start end) (substring source start end))'
  printf '%s\n' '(defun nelisp--load-skip-space-and-comments (source position) (while (and (< position (length source)) (memq (aref source position) (list 32 9 10 13))) (setq position (+ position 1))) position)'
  printf '%s\n' '(setq init-file-had-error nil nemacs-init-file-error nil)'
  printf '%s\n' '(real-init-audit--load-forms-file (getenv "REAL_INIT_AUDIT_TEST_INIT") (quote init))'
} > "$helper_file"

printf '%s\n' '(setq real-init-audit-test-one 1)' > "$dummy_init"
printf '%s\n' '(setq real-init-audit-test-two 2)' >> "$dummy_init"
printf '%s\n' '(error "DUMMY_SECRET_DO_NOT_TRACE")' >> "$dummy_init"

env -u NEMACS_REAL_INIT_TRACE \
  REAL_INIT_AUDIT_TEST_INIT="$dummy_init" \
  emacs -Q --batch -l "$helper_file" > "$test_dir/trace-off.log" 2>&1
REAL_INIT_AUDIT_TEST_INIT="$dummy_init" NEMACS_REAL_INIT_TRACE=1 \
  emacs -Q --batch -l "$helper_file" > "$test_dir/trace-on.log" 2>&1

if rg -q '^NEMACS_REAL_INIT_TRACE ' "$test_dir/trace-off.log"; then
  echo "real-init-audit-trace-test: trace was not disabled by default" >&2
  exit 1
fi

sed -n '/^NEMACS_REAL_INIT_FORM /p' "$test_dir/trace-off.log" \
  | sed 's/ secs=.*/ secs=<elapsed>/' > "$test_dir/forms-off.log"
sed -n '/^NEMACS_REAL_INIT_FORM /p' "$test_dir/trace-on.log" \
  | sed 's/ secs=.*/ secs=<elapsed>/' > "$test_dir/forms-on.log"
diff -u "$test_dir/forms-off.log" "$test_dir/forms-on.log"
cat > "$test_dir/expected-forms.log" <<'EOF'
NEMACS_REAL_INIT_FORM 1 line=1 secs=<elapsed>
NEMACS_REAL_INIT_FORM 2 line=2 secs=<elapsed>
NEMACS_REAL_INIT_FORM 3 line=3 secs=<elapsed>
EOF
diff -u "$test_dir/expected-forms.log" "$test_dir/forms-on.log"

cat > "$test_dir/expected-markers.log" <<'EOF'
NEMACS_REAL_INIT_TRACE SKIP_BEGIN next-index=1 position=0
NEMACS_REAL_INIT_TRACE READ_BEGIN next-index=1 position=0 line=1
NEMACS_REAL_INIT_TRACE EVAL_BEGIN index=1 line=1
NEMACS_REAL_INIT_TRACE SKIP_BEGIN next-index=2 position=33
NEMACS_REAL_INIT_TRACE READ_BEGIN next-index=2 position=34 line=2
NEMACS_REAL_INIT_TRACE EVAL_BEGIN index=2 line=2
NEMACS_REAL_INIT_TRACE SKIP_BEGIN next-index=3 position=67
NEMACS_REAL_INIT_TRACE READ_BEGIN next-index=3 position=68 line=3
NEMACS_REAL_INIT_TRACE EVAL_BEGIN index=3 line=3
NEMACS_REAL_INIT_TRACE SKIP_BEGIN next-index=4 position=103
EOF
sed -n '/^NEMACS_REAL_INIT_TRACE /p' "$test_dir/trace-on.log" \
  > "$test_dir/actual-markers.log"
diff -u "$test_dir/expected-markers.log" "$test_dir/actual-markers.log"

awk '
  /^NEMACS_REAL_INIT_TRACE / { print "TRACE:" $2 }
  /^NEMACS_REAL_INIT_ERROR / { print "ERROR" }
  /^NEMACS_REAL_INIT_FORM / { print "FORM" }
' "$test_dir/trace-on.log" > "$test_dir/actual-record-order.log"
cat > "$test_dir/expected-record-order.log" <<'EOF'
TRACE:SKIP_BEGIN
TRACE:READ_BEGIN
TRACE:EVAL_BEGIN
FORM
TRACE:SKIP_BEGIN
TRACE:READ_BEGIN
TRACE:EVAL_BEGIN
FORM
TRACE:SKIP_BEGIN
TRACE:READ_BEGIN
TRACE:EVAL_BEGIN
ERROR
FORM
TRACE:SKIP_BEGIN
EOF
diff -u "$test_dir/expected-record-order.log" "$test_dir/actual-record-order.log"

if sed -n '/^NEMACS_REAL_INIT_TRACE /p' "$test_dir/trace-on.log" \
    | rg -q 'DUMMY_SECRET_DO_NOT_TRACE'; then
  echo "real-init-audit-trace-test: trace leaked dummy secret" >&2
  exit 1
fi

if [[ "$(wc -l < "$test_dir/forms-on.log")" -ne 3 ]]; then
  echo "real-init-audit-trace-test: expected three existing FORM records" >&2
  exit 1
fi
if [[ "$(rg -c '^NEMACS_REAL_INIT_ERROR ' "$test_dir/trace-on.log")" -ne 1 ]]; then
  echo "real-init-audit-trace-test: expected one handled dummy error" >&2
  exit 1
fi

echo "real-init-audit-trace-test: PASS"
