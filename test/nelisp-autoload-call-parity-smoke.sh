#!/bin/sh
# nelisp-autoload-call-parity-smoke.sh --- GNU autoload call semantics
# (JIT ledger S6.9/S6.11: bytecomp reaches byte-opt.el only through
# `(autoload ...)' function cells).
#
# GNU eval.c `Fautoload_do_load' semantics: calling a symbol whose function
# cell is `(autoload FILE DOC INTERACTIVE TYPE)' loads FILE and re-dispatches
# on the new definition (direct call, funcall, apply, mapcar); `macroexpand'
# and macro calls load macro autoloads; a FILE that does not define the
# symbol, or a missing FILE, signals instead of returning nil.  `fboundp'
# and `symbol-function' must keep seeing the raw autoload object.
#
# Compares host GNU Emacs against the standalone binary on the same script.
# Usage: NELISP_BIN=target/nelisp-s6bo sh test/nelisp-autoload-call-parity-smoke.sh
# Optional: EMACS_BIN (default "emacs").
set -eu
script_dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
repo=${NELISP_ROOT:-$(CDPATH= cd -- "$script_dir/.." && pwd)}
binary=${NELISP_BIN:-$repo/target/nelisp}
emacs_bin=${EMACS_BIN:-emacs}
if [ ! -x "$binary" ]; then
    echo "GATE-SKIP NELISP_BIN is not executable: $binary"
    exit 0
fi
work=$(mktemp -d "${TMPDIR:-/tmp}/nelisp-autoload-smoke.XXXXXX")
trap 'rm -rf "$work"' EXIT
cat > "$work/lib.el" <<'LIB'
;;; lib.el --- autoload smoke library  -*- lexical-binding: t -*-
(defun nl-al-fn (x) (* x 2))
(defmacro nl-al-mac (x) (list '+ x 1))
(defun nl-al-fn2 (x) (1+ x))
(provide 'nl-al-lib)
LIB
cat > "$work/probe.el" <<PROBE
;;; probe.el  -*- lexical-binding: t -*-
(defmacro nl-p (form) \`(princ (format "%S\\n" (condition-case e \${form} (error (list 'ERR (car e)))))))
(autoload 'nl-al-fn "$work/lib.el")
(autoload 'nl-al-mac "$work/lib.el" nil nil 'macro)
(autoload 'nl-al-fn2 "$work/lib.el")
(autoload 'nl-al-bad "$work/lib.el")
(autoload 'nl-al-miss "$work/no-such-lib.el")
(autoload 'nl-al-unused "$work/lib.el")
(nl-p (list (fboundp 'nl-al-unused) (car (symbol-function 'nl-al-unused)) (autoloadp (symbol-function 'nl-al-unused))))
(nl-p (nl-al-fn 4))
(nl-p (funcall 'nl-al-fn2 5))
(nl-p (list (macroexpand '(nl-al-mac 3)) (nl-al-mac 3)))
(nl-p (mapcar 'nl-al-fn '(1 2)))
(nl-p (apply 'nl-al-fn '(6)))
(nl-p (nl-al-bad))
(nl-p (nl-al-miss))
(nl-p (car (symbol-function 'nl-al-miss)))
PROBE
sed 's/\${form}/,form/' "$work/probe.el" > "$work/probe2.el"
host_out=$("$emacs_bin" --batch -Q -l "$work/probe2.el" 2>/dev/null)
vm_err="$work/vm.err"
vm_out=$("$binary" --load "$work/probe2.el" -- 2>"$vm_err") || { echo "FAIL: standalone errored"; cat "$vm_err" >&2; exit 1; }
# `--load' echoes the last form's value after the probe lines; keep the 9 probes.
vm_out=$(printf '%s\n' "$vm_out" | head -n 9)
# The host prints `nl-al-bad' as (ERR error); both sides must agree exactly.
if [ "$host_out" != "$vm_out" ]; then
    echo "FAIL: host/standalone autoload call parity"
    echo "--- host"; echo "$host_out"; echo "--- standalone"; echo "$vm_out"
    exit 1
fi
expected='(t autoload t)
8
6
((+ 3 1) 4)
(2 4)
12
(ERR error)
(ERR file-missing)
autoload'
if [ "$host_out" != "$expected" ]; then
    echo "FAIL: shared output differs from pinned expectation"
    echo "$host_out"
    exit 1
fi
echo "PASS autoload call parity (9 probes)"
