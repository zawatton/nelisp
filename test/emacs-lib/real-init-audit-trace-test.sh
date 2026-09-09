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
  printf '%s\n' '(setq load-garbage-collect-interval 64 real-init-audit-test-gc-count 0)'
  printf '%s\n' '(fset (quote garbage-collect) (lambda (&optional _full) (setq real-init-audit-test-gc-count (+ real-init-audit-test-gc-count 1))))'
  printf '%s\n' '(setq init-file-had-error nil nemacs-init-file-error nil)'
  printf '%s\n' '(real-init-audit--load-forms-file (getenv "REAL_INIT_AUDIT_TEST_INIT") (quote init))'
  printf '%s\n' '(unless (and (real-init-audit--require-form-p (quote (require trace-test-feature))) (not (real-init-audit--require-form-p (quote (setq trace-test-feature t))))) (error "require form detector regression"))'
  printf '%s\n' '(princ (format "GC_COUNT %d\n" real-init-audit-test-gc-count))'
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

gc_init="$test_dir/gc-init.el"
: > "$gc_init"
for i in $(seq 1 128); do
  printf '(setq real-init-audit-test-value %d)\n' "$i" >> "$gc_init"
done
printf '%s\n' '(require (quote trace-test-feature) nil t)' >> "$gc_init"
REAL_INIT_AUDIT_TEST_INIT="$gc_init" emacs -Q --batch -l "$helper_file" \
  > "$test_dir/gc.log" 2>&1
if ! rg -q '^GC_COUNT 3$' "$test_dir/gc.log"; then
  echo "real-init-audit-trace-test: expected two periodic and one require collection" >&2
  cat "$test_dir/gc.log" >&2
  exit 1
fi

# The production audit feeds one exact source slice through one physical REPL
# wrapper per form.  Exercise that generator and evaluator together with a
# host-only evaluator shim: this checks wrapper count/metadata, multiline
# source preservation, per-wrapper error continuation, and require GC without
# requiring a full standalone build.
fixture_user_dir="$test_dir/user"
mkdir -p "$fixture_user_dir"
cat > "$fixture_user_dir/early-init.el" <<'EOF'
  ; ignored header with leading whitespace
#| outer comment
   #| nested comment |#
|#
(setq real-init-audit-wrapper-early 1) ; trailing comment
(progn
 (setq real-init-audit-wrapper-multi 2)
 (setq real-init-audit-wrapper-multi-continued 3))
EOF
cat > "$fixture_user_dir/init.el" <<'EOF'
(setq real-init-audit-wrapper-before-error 4)
(error "wrapper error is recorded")
(require (quote wrapper-test-feature) nil t)
(setq real-init-audit-wrapper-after-error 5)
(setq real-init-audit-wrapper-char ?─)
EOF
wrapper_file="$test_dir/wrappers.repl"
emacs -Q --batch -l "$repo_root/scripts/real-init-audit-generate.el" -- \
  "$fixture_user_dir" "$wrapper_file" > "$test_dir/generate.log" 2>&1
wrapper_count="$(rg -c '^\(real-init-audit--eval-one ' "$wrapper_file")"
if [[ "$wrapper_count" -ne 7 ]]; then
  echo "real-init-audit-trace-test: expected seven independent wrappers, got $wrapper_count" >&2
  cat "$wrapper_file" >&2
  exit 1
fi
if ! rg -q "early-init\.el\" 'early-init 2 6 \"\(progn\\\\n" "$wrapper_file"; then
  echo "real-init-audit-trace-test: multiline exact source slice was not preserved" >&2
  cat "$wrapper_file" >&2
  exit 1
fi
if [[ "$(wc -l < "$wrapper_file")" -ne 16 ]]; then
  echo "real-init-audit-trace-test: wrapper crossed a physical line boundary" >&2
  cat "$wrapper_file" >&2
  exit 1
fi

wrapper_runner="$test_dir/wrapper-runner.el"
{
  printf '%s' '(progn '
  while IFS= read -r helper_line; do
    printf '%s ' "$helper_line"
  done < "$helper_body"
  printf '%s\n' ')'
  printf '%s\n' '(defun emacs-load--byte-indexed-source (source) source)'
  printf '%s\n' '(defun emacs-load--reader-slice (source start end) (substring source start end))'
  printf '%s\n' '(defun emacs-load--native-read-one (source position _length) (cons (car (read-from-string source position)) (length source)))'
  printf '%s\n' '(defun nelisp--load-rewrite-defalias-form (form) form)'
  printf '%s\n' '(provide (quote nemacs-main))'
  printf '%s\n' '(setq init-file-had-error nil nemacs-init-file-error nil real-init-audit-wrapper-gc-count 0)'
  printf '%s\n' '(fset (quote garbage-collect) (lambda (&optional _full) (setq real-init-audit-wrapper-gc-count (+ real-init-audit-wrapper-gc-count 1))))'
  cat "$wrapper_file"
  printf '%s\n' '(princ (format "WRAPPER_STATE early=%S multi=%S continued=%S before=%S after=%S char=%S gc=%S initialized=%S\n" real-init-audit-wrapper-early real-init-audit-wrapper-multi real-init-audit-wrapper-multi-continued real-init-audit-wrapper-before-error real-init-audit-wrapper-after-error real-init-audit-wrapper-char real-init-audit-wrapper-gc-count nemacs-initialized))'
} > "$wrapper_runner"
emacs -Q --batch -l "$wrapper_runner" > "$test_dir/wrapper-run.log" 2>&1
if [[ "$(rg -c '^NEMACS_REAL_INIT_BOUNDARY ' "$test_dir/wrapper-run.log")" -ne 7 ]]; then
  echo "real-init-audit-trace-test: boundary metadata count mismatch" >&2
  cat "$test_dir/wrapper-run.log" >&2
  exit 1
fi
if [[ "$(rg -c '^NEMACS_REAL_INIT_ERROR ' "$test_dir/wrapper-run.log")" -ne 1 ]]; then
  echo "real-init-audit-trace-test: wrapper error was not recorded exactly once" >&2
  cat "$test_dir/wrapper-run.log" >&2
  exit 1
fi
if ! rg -q '^AUDIT_DONE$' "$test_dir/wrapper-run.log"; then
  echo "real-init-audit-trace-test: finish wrapper did not emit AUDIT_DONE" >&2
  cat "$test_dir/wrapper-run.log" >&2
  exit 1
fi
if ! rg -q '^WRAPPER_STATE early=1 multi=2 continued=3 before=4 after=5 char=9472 gc=1 initialized=t$' "$test_dir/wrapper-run.log"; then
  echo "real-init-audit-trace-test: wrapper state/error continuation/GC mismatch" >&2
  cat "$test_dir/wrapper-run.log" >&2
  exit 1
fi

echo "real-init-audit-trace-test: PASS"
