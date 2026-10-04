#!/usr/bin/env bash
# Lifecycle and GNU Emacs parity checks for demand-driven case-table setup.
set -u
here=$(cd "$(dirname "$0")/../.." && pwd)
bin=${NELISP_BIN:-$here/target/nelisp}
host=${EMACS:-emacs}
bundle=${NELISP_BUNDLE:-$here/build/nemacs-bootstrap.el}
mode=${1:-}
cases="$here/test/nelisp-emacs-lib/case-table-lazy-cases.el"
fail() { printf 'case-table-lazy: FAIL: %s\n' "$*" >&2; exit 1; }
[ -x "$bin" ] || fail "standalone binary missing: $bin"
[ -f "$bundle" ] || fail "bootstrap bundle missing: $bundle"
[ "$($host --version 2>/dev/null | head -1)" = 'GNU Emacs 31.1' ] || fail 'GNU baseline must be Emacs 31.1'
work=$(mktemp -d) || fail 'mktemp failed'
artifact_dir=${NELISP_CASE_TABLE_ARTIFACT_DIR:-}
trap 'if [ -n "$artifact_dir" ]; then mkdir -p "$artifact_dir" && cp -a "$work/." "$artifact_dir/"; fi; rm -rf "$work"' EXIT

emit_reset_and_load() {
  local source=$1
  cat <<'ELISP'
(load "BUNDLE" nil t)
;; Reload the facade from a fresh unbound state so this probe observes its
;; initialization lifecycle even though the full bootstrap loaded it earlier.
(dolist (symbol '(case-table--standard case-table--current
                  case-table--standard-initialized))
  (when (boundp symbol) (makunbound symbol)))
ELISP
  printf '(load "%s" nil t)\n' "$source"
}

case "$mode" in
  --lifecycle|--control)
    source="$here/packages/nelisp-emacs-foundation/src/case-table.el"
    if [ "$mode" = --control ]; then
      [ $# -eq 2 ] || fail '--control requires the retained eager source path'
      [ -f "$2" ] || fail "control source missing: $2"
      source=$(cd "$(dirname "$2")" && pwd)/$(basename "$2")
    fi
    { emit_reset_and_load "$source" | sed "s|BUNDLE|$bundle|"; cat <<'ELISP'
(let* ((table case-table--standard)
       (initialized case-table--standard-initialized)
       (before (char-table-range table ?A))
       (classify (lambda (ready mapping)
                   (cond
                    ((and (not ready) (integerp mapping) (= mapping ?A)) 'lazy)
                    ((and ready (integerp mapping) (= mapping ?a)) 'eager)
                    (t 'invalid))))
       (assert-lazy (lambda (actual)
                      (unless (eq actual 'lazy)
                        (error "CASE-TABLE-LIFECYCLE-EXPECTED-LAZY: state=%s"
                               actual))
                      t))
       (state (funcall classify initialized before)))
  (princ (format "CASE-TABLE-STATE:%s:%S:%S\n" state initialized before))
  (funcall assert-lazy state)
  ;; Mutate only this disposable probe state. The same lifecycle contract
  ;; must reject an initialized table whose mapping does not match GNU.
  (let ((saved-ready case-table--standard-initialized)
        (saved-mapping before))
    (unwind-protect
        (progn
          (setq case-table--standard-initialized t)
          (set-char-table-range table ?A ?x)
          (let ((mutant (funcall classify case-table--standard-initialized
                                  (char-table-range table ?A))))
            (unless (eq mutant 'invalid)
              (error "CASE-TABLE-NEGATIVE-CONTROL-CLASSIFIED:%s" mutant))
            (unless (condition-case nil
                        (progn (funcall assert-lazy mutant) nil)
                      (error t))
              (error "CASE-TABLE-NEGATIVE-CONTROL-NOT-REJECTED")))
          (princ "CASE-TABLE-NEGATIVE-CONTROL:rejected-invalid\n"))
      (setq case-table--standard-initialized saved-ready)
      (set-char-table-range table ?A saved-mapping)))
  ;; The first public getter is demand. It must preserve table identity and
  ;; install the GNU mapping, while subsequent mutations remain authoritative.
  (let ((same (eq table (standard-case-table))))
    (unless (and same (= (char-table-range table ?A) ?a)
                 (= (char-table-range table ?Z) ?z))
      (error "standard case table failed to initialize on demand"))
    (set-char-table-range table ?Q ?x)
    (unless (= (char-table-range (standard-case-table) ?Q) ?x)
      (error "standard case table identity or mutation was lost")))
  (princ "CASE-TABLE-DEMAND:ok\n"))
ELISP
    } > "$work/probe.el"
    if [ "$mode" = --lifecycle ]; then
      (cd "$here" && timeout 80 "$bin" --load "$work/probe.el" >"$work/out" 2>"$work/err") || fail "standalone lifecycle execution failed: $(tail -c 300 "$work/err")"
      [ ! -s "$work/err" ] || fail "unexpected standalone lifecycle stderr: $(tail -c 300 "$work/err")"
      grep -q '^CASE-TABLE-STATE:lazy:nil:65$' "$work/out" || fail 'standard table was not pristine and uninitialized before demand'
      grep -q '^CASE-TABLE-NEGATIVE-CONTROL:rejected-invalid$' "$work/out" || fail 'initialized-but-wrong mapping was not rejected'
      grep -q '^CASE-TABLE-DEMAND:ok$' "$work/out" || fail 'demand use did not complete'
    else
      set +e
      (cd "$here" && timeout 80 "$bin" --load "$work/probe.el" >"$work/out" 2>"$work/err")
      rc=$?
      set -e
      [ "$rc" -ne 0 ] || fail 'retained eager source unexpectedly passed the identical lazy lifecycle assertion'
      grep -q '^CASE-TABLE-STATE:eager:t:97$' "$work/out" || fail 'control source did not reach the eager lifecycle state'
      grep -q 'CASE-TABLE-LIFECYCLE-EXPECTED-LAZY: state=eager' "$work/err" || fail 'control failed for a reason other than the lazy lifecycle assertion'
    fi
    printf 'case-table-lazy: PASS (%s)\n' "$mode"
    ;;
  --parity)
    [ -f "$cases" ] || fail 'missing parity case file'
    driver="$work/parity.el"
    generator="$work/emit-driver.el"
    cat > "$generator" <<'ELISP'
;;; -*- lexical-binding: t; -*-
(let* ((cases (getenv "CASE_TABLE_CASES"))
       (driver (getenv "CASE_TABLE_DRIVER"))
       (count-file (getenv "CASE_TABLE_COUNT_FILE"))
       (forms nil)
       (print-length nil)
       (print-level nil))
  (unless (and cases driver count-file)
    (error "case-table parity emitter paths are missing"))
  (with-temp-buffer
    (insert-file-contents cases)
    (emacs-lisp-mode)
    (check-parens)
    (goto-char (point-min))
    (while (progn (forward-comment (point-max)) (not (eobp)))
      (push (read (current-buffer)) forms)))
  (setq forms (nreverse forms))
  (let* ((emitted forms)
         (mutation (getenv "CASE_TABLE_EMITTER_MUTATION"))
         (expected nil)
         (print-length nil)
         (print-level nil))
    (cond ((equal mutation "drop") (setq emitted (cdr forms)))
          ((equal mutation "duplicate") (setq emitted (append forms (list (car forms)))))
          ((equal mutation "reorder") (setq emitted (reverse (copy-sequence forms)))))
    (dolist (form forms)
      (push (list 'prin1
                  (list 'condition-case 'err
                        (list 'eval (list 'quote form))
                        (list 'error (list 'list (list 'quote 'ERR) 'err))))
            expected)
      (push '(terpri) expected))
    (setq expected (nreverse expected))
    (setq expected (append expected (list '(princ "CASE-TABLE-END\n"))))
    (with-temp-file driver
      (insert ";;; -*- lexical-binding: t; -*-\n")
      (dolist (form emitted)
        (prin1 (list 'prin1
                     (list 'condition-case 'err
                           (list 'eval (list 'quote form))
                           (list 'error (list 'list (list 'quote 'ERR) 'err))))
               (current-buffer))
        (insert "\n(terpri)\n"))
      (insert "(princ \"CASE-TABLE-END\\n\")\n"))
    ;; Re-read the emitted program and compare every quoted AST and its order.
    (let ((actual nil))
      (with-temp-buffer
        (insert-file-contents driver)
        (emacs-lisp-mode)
        (check-parens)
        (goto-char (point-min))
        (while (progn (forward-comment (point-max)) (not (eobp)))
          (push (read (current-buffer)) actual)))
      (unless (and (= (length emitted) (length forms))
                   (equal (nreverse actual) expected))
        (error "generated driver AST/count validation failed")))
    (with-temp-file count-file (insert (number-to-string (length forms)) "\n"))))
ELISP
    CASE_TABLE_CASES="$cases" CASE_TABLE_DRIVER="$driver" \
      CASE_TABLE_COUNT_FILE="$work/form-count" \
      timeout 30 "$host" -Q --batch -l "$generator" >"$work/generator.out" 2>"$work/generator.err" \
      || fail "GNU parity driver generation failed: $(tail -c 300 "$work/generator.err")"
    [ ! -s "$work/generator.err" ] || fail 'unexpected GNU generator stderr'
    form_count=$(cat "$work/form-count")
    [ "$form_count" -eq 4 ] || fail "GNU emitter parsed unexpected form count: $form_count"
    (cd "$work" && sha256sum parity.el | awk '{print $1}') > "$work/generated-driver.sha256"
    sha256sum "$cases" | awk '{print $1}' > "$work/cases.sha256"
    sha256sum "$bundle" | awk '{print $1}' > "$work/bundle.sha256"
    printf 'source: PASS (%s forms)\n' "$form_count" > "$work/generator-controls.txt"
    cat "$cases" > "$work/trailing-comments-cases.el"
    printf '\n; trailing comment one\n; trailing comment two\n' >> "$work/trailing-comments-cases.el"
    CASE_TABLE_CASES="$work/trailing-comments-cases.el" \
      CASE_TABLE_DRIVER="$work/trailing-comments-driver.el" \
      CASE_TABLE_COUNT_FILE="$work/trailing-comments-count" timeout 30 "$host" -Q --batch -l "$generator" \
      >"$work/trailing-comments.out" 2>"$work/trailing-comments.err" \
      || fail 'GNU emitter rejected valid trailing comments'
    [ "$(cat "$work/trailing-comments-count")" -eq "$form_count" ] \
      && cmp -s "$driver" "$work/trailing-comments-driver.el" \
      || fail 'GNU emitter changed ASTs or count with trailing comments'
    printf 'trailing-comments: PASS (2 comments, identical AST/count)\n' >> "$work/generator-controls.txt"
    # The direct quoted-AST driver must preserve the prior GNU observations.
    timeout 30 "$host" -Q --batch -l "$driver" > "$work/host.out" 2> "$work/host.err" \
      || fail "GNU parity driver failed: $(tail -c 300 "$work/host.err")"
    [ ! -s "$work/host.err" ] || fail 'unexpected GNU parity stderr'
    # Generator controls use GNU Emacs only; they do not start the native runtime.
    cat > "$work/error-cases.el" <<'ELISP'
(signal 'wrong-type-argument '(case-table-p not-a-case-table))
ELISP
    CASE_TABLE_CASES="$work/error-cases.el" CASE_TABLE_DRIVER="$work/error-driver.el" \
      CASE_TABLE_COUNT_FILE="$work/error-count" timeout 30 "$host" -Q --batch -l "$generator" \
      >"$work/error-generator.out" 2>"$work/error-generator.err" \
      || fail 'GNU error-fixture generation failed'
    timeout 30 "$host" -Q --batch -l "$work/error-driver.el" >"$work/error.out" 2>"$work/error.err" \
      || fail 'GNU error-fixture driver failed'
    [ "$(head -1 "$work/error.out")" = '(ERR (wrong-type-argument case-table-p not-a-case-table))' ] \
      && [ "$(tail -1 "$work/error.out")" = CASE-TABLE-END ] \
      && [ "$(wc -l < "$work/error.out")" -eq 2 ] \
      || fail 'GNU error fixture lost condition data'
    printf 'error-data: PASS\n' >> "$work/generator-controls.txt"
    printf '(list 1\n' > "$work/incomplete-cases.el"
    if CASE_TABLE_CASES="$work/incomplete-cases.el" CASE_TABLE_DRIVER="$work/incomplete-driver.el" \
      CASE_TABLE_COUNT_FILE="$work/incomplete-count" timeout 30 "$host" -Q --batch -l "$generator" \
      >"$work/incomplete.out" 2>"$work/incomplete.err"; then
      fail 'GNU emitter accepted an incomplete final form'
    fi
    [ ! -e "$work/incomplete-count" ] || fail 'GNU emitter counted incomplete source'
    printf 'incomplete-source: PASS (hard failure)\n' >> "$work/generator-controls.txt"
    cat "$cases" > "$work/sentinel-incomplete-cases.el"
    printf '\nCASE_TABLE_LAZY_PARITY_END_20261003\n(\n' >> "$work/sentinel-incomplete-cases.el"
    if CASE_TABLE_CASES="$work/sentinel-incomplete-cases.el" \
      CASE_TABLE_DRIVER="$work/sentinel-incomplete-driver.el" \
      CASE_TABLE_COUNT_FILE="$work/sentinel-incomplete-count" timeout 30 "$host" -Q --batch -l "$generator" \
      >"$work/sentinel-incomplete.out" 2>"$work/sentinel-incomplete.err"; then
      fail 'GNU emitter accepted a reserved sentinel followed by incomplete syntax'
    fi
    [ ! -e "$work/sentinel-incomplete-count" ] \
      || fail 'GNU emitter counted malformed syntax after a reserved sentinel'
    printf 'reserved-sentinel-trailing-syntax: PASS (hard failure)\n' >> "$work/generator-controls.txt"
    for mutation in drop duplicate reorder; do
      if CASE_TABLE_CASES="$cases" CASE_TABLE_DRIVER="$work/mutated-$mutation.el" \
        CASE_TABLE_COUNT_FILE="$work/mutated-$mutation.count" CASE_TABLE_EMITTER_MUTATION="$mutation" \
        timeout 30 "$host" -Q --batch -l "$generator" \
        >"$work/mutated-$mutation.out" 2>"$work/mutated-$mutation.err"; then
        fail "GNU emitter accepted $mutation AST mutation"
      fi
      grep -q 'generated driver AST/count validation failed' "$work/mutated-$mutation.err" \
        || fail "GNU emitter rejected $mutation for an unexpected reason"
      printf '%s: PASS (rejected)\n' "$mutation" >> "$work/generator-controls.txt"
    done
    printf '(load "%s" nil t)\n(load "%s" nil t)\n' "$bundle" "$driver" > "$work/run.el"
    (cd "$here" && timeout 80 "$bin" --load "$work/run.el") > "$work/native.out" 2> "$work/native.err" \
      || fail "standalone parity driver failed: $(tail -c 300 "$work/native.err")"
    [ ! -s "$work/native.err" ] || fail "unexpected standalone parity stderr: $(tail -c 300 "$work/native.err")"
    awk '1; /^CASE-TABLE-END$/ { exit }' "$work/host.out" > "$work/host.transcript"
    awk '1; /^CASE-TABLE-END$/ { exit }' "$work/native.out" > "$work/native.transcript"
    expected_rows=$((form_count + 1))
    [ "$(wc -l < "$work/host.transcript")" -eq "$expected_rows" ] || fail 'GNU transcript ended early or has an unexpected row count'
    [ "$(wc -l < "$work/native.transcript")" -eq "$expected_rows" ] || fail 'standalone transcript ended early or has an unexpected row count'
    [ "$(tail -1 "$work/host.transcript")" = CASE-TABLE-END ] || fail 'GNU completion marker missing'
    [ "$(tail -1 "$work/native.transcript")" = CASE-TABLE-END ] || fail 'standalone completion marker missing'
    cmp -s "$work/host.transcript" "$work/native.transcript" || {
      diff -u "$work/host.transcript" "$work/native.transcript" | head -60 >&2
      fail 'GNU and standalone case-table transcripts differ'
    }
    printf 'case-table-lazy: PASS (--parity, %s rows match GNU Emacs 31.1)\n' "$((expected_rows - 1))"
    ;;
  --bundle)
    driver="$work/use.el"
    cat > "$driver" <<'ELISP'
(let ((table (standard-case-table)))
  (unless (and (= (char-table-range table ?A) ?a)
               (= (char-table-range table ?Z) ?z))
    (error "integrated bootstrap standard case table mismatch")))
(with-temp-buffer
  (let ((table (copy-case-table (standard-case-table))))
    (set-case-table table)
    (unless (eq (current-case-table) table)
      (error "integrated bootstrap failed to install current case table"))))
(princ "CASE-TABLE-BUNDLE-USE:ok\n")
ELISP
    printf '(load "%s" nil t)\n(princ (format "CASE-TABLE-BOOTSTRAP-DEMAND:%%S\\n" case-table--standard-initialized))\n(load "%s" nil t)\n' "$bundle" "$driver" > "$work/run.el"
    log=${NELISP_CASE_TABLE_BUNDLE_LOG:-${TMPDIR:-/tmp}/case-table-lazy-bundle-$(date +%s).log}
    start=$(date +%s%N)
    (cd "$here" && timeout 50 "$bin" --load "$work/run.el") >"$log" 2>&1 || fail "integrated bootstrap/use exceeded 50 seconds or failed (log: $log)"
    end=$(date +%s%N)
    elapsed=$(awk -v s="$start" -v e="$end" 'BEGIN { printf "%.3f", (e-s)/1000000000 }')
    grep -q '^CASE-TABLE-BUNDLE-USE:ok$' "$log" || fail "bundle case-table use did not complete (log: $log)"
    demand=$(sed -n 's/^CASE-TABLE-BOOTSTRAP-DEMAND:\(.*\)$/\1/p' "$log" | tail -1)
    [ "$demand" = t ] || [ "$demand" = nil ] || fail "bootstrap demand phase marker missing (log: $log)"
    printf 'case-table-lazy: PASS (--bundle, %ss, standard mappings initialized during bootstrap: %s, log %s)\n' "$elapsed" "$demand" "$log"
    ;;
  *) fail 'usage: --lifecycle | --control PATH | --parity | --bundle' ;;
esac
