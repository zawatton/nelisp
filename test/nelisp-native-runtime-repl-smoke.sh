#!/bin/sh
set -eu
root=$(CDPATH= cd -- "$(dirname "$0")/.." && pwd)
bin=${NELISP_BIN:-$root/target/nelisp-runtime-reload}
dir=$(mktemp -d "${TMPDIR:-/tmp}/nelisp-native-runtime-repl-smoke.XXXXXX")
trap 'if [ "${NELISP_SMOKE_KEEP:-0}" != 1 ]; then rm -rf "$dir"; fi' EXIT
echo "artifacts=$dir"
sha=$(sha256sum "$bin" | awk '{print $1}')
# Build an isolated second generation.  Only this fixture is changed: the
# canonical exported source remains untouched.
NELISP_B_SOURCE="$dir/generation-b.el" NELISP_B_ARTIFACT="$dir/generation-b.nelr" \
NELISP_B_SHA="$sha" NELISP_ROOT="$root" \
  emacs --batch -Q -L "$root/lisp" -L "$root/src" -L "$root/scripts" \
  --eval '(setq load-prefer-newer t)' \
  --eval '(progn
            (require (quote nelisp-runtime-reload-build))
            (nelisp-runtime-reload-export-source (getenv "NELISP_B_SOURCE")))'
export NELISP_B_SOURCE="$dir/generation-b.el" NELISP_B_ARTIFACT="$dir/generation-b.nelr" NELISP_B_SHA="$sha"
emacs --batch -Q -L "$root/lisp" -L "$root/src" -L "$root/scripts" \
  --eval '(setq load-prefer-newer t)' \
  --eval '(progn
            (require (quote nelisp-runtime-reload-abi))
            (require (quote nelisp-native-load))
            (let* ((path (getenv "NELISP_B_SOURCE"))
                   (forms (nelisp-native-load--raw-source-forms path))
                   (definition (cl-find (quote nl_gc_debt_pct) forms :key (function cadr))))
              (unless definition (error "Missing GC percentage function"))
              (setcdr (cddr definition)
                (quote ((if (= (ptr-read-u64 (data-addr nl_gc_stats) 48) 0)
                            (nl_gc_probe_pct)
                          (ptr-read-u64 (data-addr nl_gc_stats) 48)))))
              (push (quote (defun nl_gc_probe_pct () 301)) forms)
              (with-temp-file path
                (let ((print-length nil) (print-level nil))
                  (dolist (form forms) (prin1 form (current-buffer)) (insert "\n")))))
            (nelisp-native-load-raw-v2-compile-file
             (getenv "NELISP_B_SOURCE") (getenv "NELISP_B_ARTIFACT")
             "generation-b" (getenv "NELISP_B_SHA")))' >/dev/null
cat >"$dir/invalid.el" <<'EOF'
(defun nelisp-native-runtime-invalid (x) (+ x 1))
#z
EOF
cat >"$dir/fixture.el" <<EOF
(progn
(load "$root/lisp/nelisp-runtime-development.el")
(setq smoke-retained (list "retained" (vector 17 29)))
(let ((s (nelisp-runtime-reload-status))) (unless (eq (plist-get s :status) 'ready) (error "runtime unavailable: %S" s)))
(setq smoke-before (nelisp-runtime-reload-status))
(setq smoke-install (nelisp-runtime-rebuild-and-reload "$root"))
(setq smoke-after (nelisp-runtime-reload-status))
(unless (eq (plist-get smoke-install :status) 'published) (error "v2 publish failed: %S" smoke-install))
(unless (> (plist-get smoke-after :generation) (plist-get smoke-before :generation)) (error "generation did not advance: %S" smoke-install))
(unless (and (plist-get smoke-after :state) (plist-get smoke-after :generation)) (error "state missing: %S" smoke-after))
(nelisp--write-stdout-bytes (format "GC_V2_PUBLISHED_GEN_%d\\n" (plist-get smoke-after :generation)))
(setq smoke-stats (nelisp-native-load--raw-symbol-addr "nl_gc_stats"))
(setq smoke-floor (ptr-read-u64 smoke-stats 40))
(setq smoke-percent (ptr-read-u64 smoke-stats 48))
(ptr-write-u64 smoke-stats 40 1)
(ptr-write-u64 smoke-stats 48 0)
(garbage-collect)
(unless (= (ptr-read-u64 smoke-stats 8) (/ (* (ptr-read-u64 smoke-stats 16) 300) 100)) (error "A collector threshold mismatch"))
(setq smoke-b-alloc (nelisp-native-load-raw-artifact "$dir/generation-b.nelr" "nl_alloc_bytes_uncheck" (getenv "NELISP_B_SHA")))
(setq smoke-b-gc (copy-sequence smoke-b-alloc))
(setq smoke-b-gc (plist-put smoke-b-gc :entry-name "nl_gc_collect_recorded_mark_sweep_body"))
(setq smoke-b-gc (plist-put smoke-b-gc :entry (nelisp-native-load-raw-export-address smoke-b-alloc "nl_gc_collect_recorded_mark_sweep_body")))
(setq smoke-b-gc (plist-put smoke-b-gc :arity 1))
(setq smoke-b-install (nelisp-native-load-raw-install smoke-b-alloc smoke-b-gc))
(unless (eq (plist-get smoke-b-install :status) 'published) (error "generation B publish failed: %S" smoke-b-install))
(garbage-collect)
(unless (= (ptr-read-u64 smoke-stats 8) (/ (* (ptr-read-u64 smoke-stats 16) 301) 100)) (error "B collector did not use its new private helper"))
(unless (equal smoke-retained '("retained" [17 29])) (error "Retained state changed"))
(nelisp--write-stdout-bytes "GC_V2_GENERATION_B_HELPER_301\\n")
(setq smoke-restore (nelisp-runtime-reload-restore-originals))
(unless (eq (plist-get smoke-restore :status) 'published) (error "restore failed: %S" smoke-restore))
(garbage-collect)
(unless (= (ptr-read-u64 smoke-stats 8) (/ (* (ptr-read-u64 smoke-stats 16) 300) 100)) (error "Original collector behavior was not restored"))
(ptr-write-u64 smoke-stats 40 smoke-floor)
(ptr-write-u64 smoke-stats 48 smoke-percent)
(nelisp--write-stdout-bytes (format "GC_V2_RESTORED_GEN_%d\\n" (plist-get smoke-restore :generation)))
(setq smoke-before-invalid (nelisp-runtime-reload-status))
(setq smoke-invalid (nelisp-runtime-reload-source-file "$dir/invalid.el" "nl_alloc_bytes_uncheck" "nl_gc_collect_recorded_mark_sweep_body" "invalid-v2"))
(setq smoke-after-invalid (nelisp-runtime-reload-status))
(unless (memq (plist-get smoke-invalid :status) '(error rejected)) (error "invalid source accepted: %S" smoke-invalid))
(unless (= (plist-get smoke-before-invalid :generation) (plist-get smoke-after-invalid :generation)) (error "invalid changed generation: %S" smoke-invalid))
(nelisp--write-stdout-bytes "GC_V2_INVALID_PRESERVED\\n")
)
EOF
export NELISP_NATIVE_SMOKE_DIR="$dir"
printf '%s\n' '(load (expand-file-name "fixture.el" (getenv "NELISP_NATIVE_SMOKE_DIR")))' '(exit)' >"$dir/input.el"
set +e
NELISP_BIN="$bin" "$root/tools/ai/nelisp-ai.sh" repl --no-prompt <"$dir/input.el" >"$dir/out" 2>"$dir/err"
status=$?
set -e
[ "$status" -eq 0 ]
if [ -s "$dir/err" ]; then
    cat "$dir/err" >&2
    exit 1
fi
grep -Eq '^GC_V2_PUBLISHED_GEN_[0-9]+$' "$dir/out"
grep -Eq '^GC_V2_RESTORED_GEN_[0-9]+$' "$dir/out"
grep -Fxq 'GC_V2_INVALID_PRESERVED' "$dir/out"
grep -Fxq 'GC_V2_GENERATION_B_HELPER_301' "$dir/out"
echo 'nelisp-native-runtime-repl-smoke: PASS'
