#!/bin/sh
set -eu
root=$(CDPATH= cd -- "$(dirname "$0")/.." && pwd)
bin=${NELISP_BIN:-$root/target/nelisp}
dir=$(mktemp -d "${TMPDIR:-/tmp}/nelisp-repl-code-smoke.XXXXXX")
trap 'rm -rf "$dir"' EXIT
cat >"$dir/source.el" <<'EOF'
(defun repl-code-smoke-fn (x) (+ x 1))
EOF
cat >"$dir/source-b.el" <<'EOF'
(defun repl-code-smoke-fn (x) (+ x 10))
EOF
cat >"$dir/invalid.el" <<'EOF'
(defun repl-code-smoke-fn (x) (+ x 2))
#z
EOF
cat >"$dir/input.el" <<EOF
(load "$root/lisp/nelisp-repl-code.el")
(load "$root/lisp/nelisp-repl-code.el")
(setq code-r1 (nelisp-artifact-reload-source-file "$dir/source.el" "smoke-v1"))
(unless (eq (plist-get code-r1 :status) 'ok) (error "reload A failed: %S" code-r1))
(setq code-i1 (nelisp-repl-code-info 'repl-code-smoke-fn))
(unless (and (eq (plist-get code-i1 :status) :current) (= (plist-get code-i1 :reload-generation) 1) (= (repl-code-smoke-fn 1) 2)) (error "info A failed: %S" code-i1))
(setq code-r2 (nelisp-artifact-reload-source-file "$dir/source-b.el" "smoke-v2"))
(setq code-i2 (nelisp-repl-code-info 'repl-code-smoke-fn))
(unless (and (eq (plist-get code-r2 :status) 'ok) (eq (plist-get code-i2 :status) :current) (> (plist-get code-i2 :reload-generation) (plist-get code-i1 :reload-generation)) (not (equal (plist-get code-i1 :source-sha256) (plist-get code-i2 :source-sha256))) (equal (plist-get code-i2 :source-span) (plist-get code-i1 :source-span)) (= (repl-code-smoke-fn 1) 11)) (error "info B failed: %S" code-i2))
(setq code-before-invalid code-i2)
(setq code-rbad (nelisp-artifact-reload-source-file "$dir/invalid.el" "smoke-invalid"))
(setq code-ibad (nelisp-repl-code-info 'repl-code-smoke-fn))
(unless (and (eq (plist-get code-rbad :status) 'error) (equal (plist-get code-before-invalid :source-sha256) (plist-get code-ibad :source-sha256)) (= (plist-get code-before-invalid :reload-generation) (plist-get code-ibad :reload-generation))) (error "invalid changed info: %S" code-rbad))
(fset 'repl-code-smoke-fn (lambda (x) (+ x 9)))
(unless (eq (plist-get (nelisp-repl-code-info 'repl-code-smoke-fn) :status) :stale) (error "fset was not stale"))
(nelisp--write-stdout-bytes "REPL_CODE_INFO_SMOKE_PASS\\n")
(exit)
EOF
set +e
NELISP_BIN="$bin" "$root/tools/ai/nelisp-ai.sh" repl --no-prompt <"$dir/input.el" >"$dir/out" 2>"$dir/err"
status=$?
set -e
[ "$status" -eq 0 ]
! grep -Eq 'uncaught error|backtrace \(' "$dir/err"
grep -Fxq REPL_CODE_INFO_SMOKE_PASS "$dir/out"
