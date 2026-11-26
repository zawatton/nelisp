#!/usr/bin/env bash
# Run after make -f mk/emacs-lib.mk bake-runtime-dump.
set -euo pipefail
root=$(cd "$(dirname "$0")/../.." && pwd -P)
binary=${NELISP_BIN:-$root/target/nelisp}
base=${NEMACS_RUNTIME_DUMP:-$root/build/nemacs-runtime.dump}
cpu=${NEMACS_TEST_CPU:-5,8}
work=$(mktemp -d "$root/target/user-snapshot-test.XXXXXXXX")
export HOME=$work/home NELISP_HOME=$root TMPDIR=$work
export NEMACS_NELISP=$binary
unset EMACSLOADPATH NEMACS_USER_EMACS_DIRECTORY
ulimit -v 8388608
ulimit -s unlimited
profile=$HOME/.emacs.d
snapshot=$work/snapshot
mkdir -p "$profile/pkg" "$profile/first"
ln -s "$base" "$work/base.dump"
export NEMACS_RUNTIME_DUMP=$work/base.dump
tool=$root/tools/dev/nemacs-user-snapshot.py
run() { timeout --kill-after=5 60 taskset -c "$cpu" "$@" </dev/null; }
bake() {
    run "$tool" bake --binary "$binary" --base "$NEMACS_RUNTIME_DUMP" \
        --snapshot "$snapshot" --init-directory "$profile" \
        --load-path "$profile/first" --load-path "$profile/pkg" --require "$1"
}
check() {
    run "$tool" run --binary "$binary" --base "$NEMACS_RUNTIME_DUMP" \
        --snapshot "$snapshot" --init-directory "$profile" -- --eval \
        "(progn (princ (if (featurep 'nuss-lib) \"PRELOADED\" \"BASE\")) (terpri))"
}

cat >"$profile/early-init.el" <<'LISP'
(defvar nuss-events nil)
(push 'early nuss-events)
(add-hook 'before-init-hook (lambda () (push 'before nuss-events)))
LISP
cat >"$profile/init.el" <<'LISP'
(add-to-list 'load-path (expand-file-name "pkg" user-emacs-directory))
(require 'nuss-lib)
(push 'init nuss-events)
(setq nuss-setting 'configured)
(add-hook 'after-init-hook (lambda () (push 'after nuss-events)))
LISP
cat >"$profile/pkg/nuss-lib.el" <<'LISP'
;;; nuss-lib.el -*- lexical-binding: t; -*-
(defvar nuss-loads 0)
(setq nuss-loads (1+ nuss-loads))
(defvar nuss-dump-hooks 0)
(add-hook 'after-pdump-load-hook (lambda () (setq nuss-dump-hooks (1+ nuss-dump-hooks))))
(defcustom nuss-setting 'default "Fixture setting." :type 'symbol)
(defvar nuss-map (make-sparse-keymap))
(define-key nuss-map (kbd "C-c s") 'nuss-command)
(defun nuss-command () (interactive) nuss-setting)
(provide 'nuss-lib)
LISP
cat >"$work/action.el" <<'LISP'
(push 'load-action nuss-events)
LISP
probe='(progn
  (let* ((file (symbol-file (quote nuss-command) (quote defun)))
         (history (assoc file load-history)))
    (princ (format "\nREPLAY %S\n" (list nuss-events nuss-loads (featurep (quote nuss-lib))
      (and (member (cons (quote provide) (quote nuss-lib)) history) t)
      (nuss-command) (lookup-key nuss-map (kbd "C-c s")))))
    (princ (format "DUMP-HOOKS %S\n" nuss-dump-hooks)))
  (kill-emacs 0))'

bake nuss-lib >"$work/bake.out" 2>"$work/bake.err"
check >"$work/selected.out" 2>"$work/selected.err"
rg -Fx PRELOADED "$work/selected.out"
test ! -s "$work/selected.err"
for mode in plain snapshot; do
    extra=()
    if [[ $mode == snapshot ]]; then extra=(--user-snapshot "$snapshot"); fi
    run "$root/bin/nemacs" "${extra[@]}" -nw --no-site-file --no-splash \
        --init-directory "$profile" -l "$work/action.el" --eval "$probe" \
        >"$work/$mode.out" 2>"$work/$mode.err"
done
expected='REPLAY ((load-action after init before early) 1 t t configured nuss-command)'
rg -Fx "$expected" "$work/plain.out"
rg -Fx "$expected" "$work/snapshot.out"
rg -Fx 'DUMP-HOOKS 0' "$work/plain.out"
rg -Fx 'DUMP-HOOKS 1' "$work/snapshot.out"

reject() {
    local expected=$1
    check >"$work/fallback.out" 2>"$work/fallback.err"
    rg -Fx BASE "$work/fallback.out"
    rg -F -- "$expected" "$work/fallback.err"
}

# Prove that a preferred candidate created after baking invalidates the heap.
cp "$profile/pkg/nuss-lib.el" "$profile/first/nuss-lib.el"
reject "$profile/first/nuss-lib.el"
rm "$profile/first/nuss-lib.el"
check >"$work/unshadowed.out" 2>"$work/unshadowed.err"
rg -Fx PRELOADED "$work/unshadowed.out"
test ! -s "$work/unshadowed.err"

cp "$snapshot/manifest.json" "$work/before.json"
for kind in timer process network descriptor buffer frame; do
    case $kind in
      timer) form='(run-with-idle-timer 60 nil (lambda () nil))'; reason=run-with-idle-timer ;;
      process) form='(start-process "nuss" nil "/bin/true")'; reason=start-process ;;
      network) form='(make-network-process :name "nuss" :server t :service t)'; reason=make-network-process ;;
      descriptor) form='(nelisp--syscall-path-int 2 "/dev/null" 0)'; reason='left open descriptors' ;;
      buffer) form='(with-current-buffer (get-buffer-create "nuss-file") (setq buffer-file-name "/nuss-test"))'; reason='visiting, indirect or modified buffer' ;;
      frame) form='(set-frame-parameter nil (quote nuss-test-frame) t)'; reason='changed frame or window state' ;;
    esac
    printf '%s\n(provide (quote nuss-bad))\n' "$form" >"$profile/pkg/nuss-bad.el"
    if bake nuss-bad >"$work/bad-$kind.out" 2>"$work/bad-$kind.err"; then
        printf 'Unsafe bake succeeded: %s\n' "$kind" >&2
        exit 1
    fi
    rg -F "$reason" "$work/bad-$kind.err"
    cmp "$snapshot/manifest.json" "$work/before.json"
done
rm "$profile/pkg/nuss-bad.el"

# A same-size rewrite with the mtime restored is still stale.
python3 - "$profile/pkg/nuss-lib.el" <<'PY'
from pathlib import Path
import os
import sys

path = Path(sys.argv[1])
before = path.stat()
path.write_bytes(path.read_bytes().replace(b"Fixture setting.", b"Changed setting."))
os.utime(path, ns=(before.st_atime_ns, before.st_mtime_ns))
PY
reject "$profile/pkg/nuss-lib.el"
bake nuss-lib >"$work/rebake.out" 2>"$work/rebake.err"
printf '\n;; init changed\n' >>"$profile/init.el"
reject "$profile/init.el"
bake nuss-lib >"$work/final-bake.out" 2>"$work/final-bake.err"
heap=$(python3 -c 'import json,sys; print(json.load(open(sys.argv[1]))["heap"])' "$snapshot/manifest.json")
chmod u+w "$snapshot/$heap"
python3 - "$snapshot/$heap" <<'PYTHON'
import sys
with open(sys.argv[1], "r+b") as heap:
    first = heap.read(1)
    heap.seek(0)
    heap.write(bytes([first[0] ^ 1]))
PYTHON
chmod u-w "$snapshot/$heap"
reject 'heap digest mismatch'
cp -p "$base" "$work/other-base.dump"
ln -sfn "$work/other-base.dump" "$NEMACS_RUNTIME_DUMP"
reject "$NEMACS_RUNTIME_DUMP"
printf 'User snapshot evidence in %s\n' "$work"
