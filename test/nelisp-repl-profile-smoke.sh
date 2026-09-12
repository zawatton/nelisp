#!/bin/sh
# nelisp-repl-profile-smoke.sh --- the profiler in the standalone, where it is for
#
# The host ERT cases grade this module under an Emacs that has `advice.el',
# `mapatoms' and a fast call path.  The standalone has none of those -- which
# is why the module exists -- so the behaviour that matters is checked here:
# shims install and come off in the real runtime, wrapped functions still
# answer, a recursive nest is one span rather than N overlapping ones, and the
# per-call overhead is a measured number rather than an assumption.

set -eu

smoke_script_dir=$(cd "$(dirname "$0")" && pwd)
smoke_root=$(cd "$smoke_script_dir/.." && pwd)
cd "$smoke_root"

smoke_dir=$(mktemp -d "${TMPDIR:-/tmp}/nelisp-repl-profile-smoke.XXXXXX")
trap 'rm -rf "$smoke_dir"' EXIT

smoke_input="$smoke_dir/session.el"
smoke_out="$smoke_dir/session.out"

cat > "$smoke_input" <<'EOF'
(require 'nelisp-repl-profile)

(defun profile-smoke--leaf (x) (+ x 1))
(defun profile-smoke--mid (x) (profile-smoke--leaf (profile-smoke--leaf x)))
(defun profile-smoke--rec (n) (if (<= n 0) 0 (profile-smoke--rec (- n 1))))

(princ (format "INSTALLED %S\n"
               (nelisp-repl-profile-instrument
                '(profile-smoke--leaf profile-smoke--mid profile-smoke--rec
                  profile-smoke--absent))))

(let ((i 0))
  (while (< i 50) (profile-smoke--mid i) (setq i (1+ i))))
(profile-smoke--rec 100)

(princ (format "VALUES %S %S %S\n"
               (profile-smoke--mid 10)
               (profile-smoke--leaf 10)
               (profile-smoke--rec 3)))

(dolist (row (nelisp-repl-profile-report))
  (princ (format "ROW %s calls=%d\n"
                 (plist-get row :name) (plist-get row :calls))))

(princ (format "OVERHEAD-POSITIVE %S\n"
               (> (nelisp-repl-profile-overhead-seconds 200) 0)))
(princ (format "RESTORED %S\n"
               (plist-get (nelisp-repl-profile-restore) :restored)))
(princ (format "AFTER-RESTORE %S %S\n"
               (profile-smoke--mid 10) (profile-smoke--leaf 10)))
(exit)
EOF

tools/ai/nelisp-ai.sh repl --no-prompt --no-print --script "$smoke_input" \
    < /dev/null > "$smoke_out" 2>&1 || true

smoke_require() {
    if ! grep -q -- "$1" "$smoke_out"; then
        printf 'nelisp-repl-profile-smoke: missing %s\n' "$1" >&2
        cat "$smoke_out" >&2
        exit 1
    fi
}

# A name that is not defined is not reported as installed.
smoke_require 'INSTALLED (profile-smoke--leaf profile-smoke--mid profile-smoke--rec)'
# Wrapping must not change what the functions answer, before or after restore.
smoke_require 'VALUES 12 11 0'
smoke_require 'AFTER-RESTORE 12 11'
# 50 calls to mid, two leaf calls inside each plus the direct one in VALUES.
smoke_require 'ROW profile-smoke--mid calls=51'
smoke_require 'ROW profile-smoke--leaf calls=103'
# 101 entries for the nest of 100 (n=100..0), plus 4 for the nest of 3.
smoke_require 'ROW profile-smoke--rec calls=105'
smoke_require 'OVERHEAD-POSITIVE t'
smoke_require 'RESTORED (profile-smoke--leaf profile-smoke--mid profile-smoke--rec)'

printf 'nelisp-repl-profile-smoke: ok\n'
