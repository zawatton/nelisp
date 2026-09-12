#!/bin/sh
# nelisp-repl-reload-smoke.sh --- publish one module's defuns in a live session
#
# `nelisp-artifact-reload-source-file' refuses a file that is not all `defun'.
# `nelisp-repl-reload-defuns' points the same strict machinery at an ordinary
# module: it stages the module's defuns and NAMES the rest.  The property that
# matters is what does NOT happen -- the module's `require' is not re-run and
# its `defvar' does not overwrite the value the live session is holding.
#
# One standalone session: load the module, give its variable a session value,
# rewrite the file (new function body AND a new `defvar' initializer), publish
# the defuns, and check that the code changed while the variable did not.

set -eu

smoke_script_dir=$(cd "$(dirname "$0")" && pwd)
smoke_root=$(cd "$smoke_script_dir/.." && pwd)
cd "$smoke_root"

smoke_dir=$(mktemp -d "${TMPDIR:-/tmp}/nelisp-repl-reload-smoke.XXXXXX")
smoke_keep=${NELISP_REPL_RELOAD_SMOKE_KEEP:-0}
smoke_status=0
cleanup_smoke() {
    smoke_status=$?
    trap - EXIT
    if [ "$smoke_keep" = 1 ]; then
        printf 'smoke evidence retained: %s\n' "$smoke_dir" >&2
    else
        rm -rf "$smoke_dir"
    fi
    exit "$smoke_status"
}
trap cleanup_smoke EXIT

cat > "$smoke_dir/reload-smoke-dep.el" <<'EOF'
(defun nelisp-repl-reload-smoke--dep () 'dep)
(provide 'reload-smoke-dep)
EOF

cat > "$smoke_dir/reload-smoke-module.el" <<'EOF'
;;; reload-smoke-module.el --- ordinary module  -*- lexical-binding: t; -*-
(require 'reload-smoke-dep)

(defvar nelisp-repl-reload-smoke--var 1)

(defun nelisp-repl-reload-smoke--fn (x)
  (+ x nelisp-repl-reload-smoke--var))

(provide 'reload-smoke-module)
EOF

smoke_session="$smoke_dir/session.el"
smoke_out="$smoke_dir/session.out"

cat > "$smoke_session" <<EOF
(setq load-path (cons "$smoke_dir" load-path))
(require 'nelisp-repl-reload)
(load "$smoke_dir/reload-smoke-module.el")

(princ
 (format "V1 %S dep=%S\n"
         (nelisp-repl-reload-smoke--fn 5)
         (fboundp 'nelisp-repl-reload-smoke--dep)))

;; The session gives the module's variable a value of its own.
(setq nelisp-repl-reload-smoke--var 10)
(princ (format "SESSION-VAR %S\n" nelisp-repl-reload-smoke--var))

;; Now "edit" the module: new body, and a defvar initializer that must NOT
;; be executed by the publication.
(with-temp-file "$smoke_dir/reload-smoke-module.el"
  (insert ";;; reload-smoke-module.el --- ordinary module  -*- lexical-binding: t; -*-\n"
          "(require 'reload-smoke-dep)\n"
          "\n"
          "(defvar nelisp-repl-reload-smoke--var 99)\n"
          "\n"
          "(defun nelisp-repl-reload-smoke--fn (x)\n"
          "  (* x nelisp-repl-reload-smoke--var))\n"
          "\n"
          "(provide 'reload-smoke-module)\n"))

(setq smoke-result
      (nelisp-repl-reload-defuns "$smoke_dir/reload-smoke-module.el"))

(princ (format "STATUS %S\n" (plist-get smoke-result :status)))
(princ (format "PUBLISHED %S\n" (plist-get smoke-result :published)))
(princ (format "SELECTED %S\n" (plist-get smoke-result :selected)))
(princ (format "SKIPPED %S\n" (mapcar #'car (plist-get smoke-result :skipped))))
(princ (format "DECLARED %S\n" (plist-get smoke-result :declared)))
(princ (format "V2 %S\n" (nelisp-repl-reload-smoke--fn 5)))
(princ (format "VAR-AFTER %S\n" nelisp-repl-reload-smoke--var))
(exit)
EOF

tools/ai/nelisp-ai.sh repl --no-prompt --script "$smoke_session" \
    < /dev/null > "$smoke_out" 2>&1 || true

smoke_require() {
    if ! grep -q -- "$1" "$smoke_out"; then
        printf 'nelisp-repl-reload-smoke: missing %s\n' "$1" >&2
        printf -- '--- session output ---\n' >&2
        cat "$smoke_out" >&2
        exit 1
    fi
}

smoke_require 'V1 6 dep=t'
smoke_require 'SESSION-VAR 10'
smoke_require 'STATUS ok'
smoke_require 'PUBLISHED (nelisp-repl-reload-smoke--fn)'
smoke_require 'SELECTED (nelisp-repl-reload-smoke--fn)'
smoke_require 'SKIPPED (require defvar provide)'
smoke_require 'DECLARED (nelisp-repl-reload-smoke--var)'
# New body, old variable: the code was replaced, the load-time forms were not
# re-run, and the session's own value survived.
smoke_require 'V2 50'
smoke_require 'VAR-AFTER 10'

printf 'nelisp-repl-reload-smoke: ok\n'
