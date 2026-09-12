#!/bin/sh
# nelisp-arena-dump-live-session-smoke.sh --- is the session whole after an in-place dump?
#
# `nelisp--arena-dump-image-stream' writes the heap in place and restores it,
# so the process it was called from keeps running on the same arena.  On
# 2026-09-12, while checkpointing a real-init audit, the form AFTER a
# checkpoint failed with `void-variable' and every helper after that read as
# `void-function' -- the dump had been taken and the live session was not
# whole.  The cause was found elsewhere (boundary reclamation handing back a
# bump region without zeroing it) and fixed; this pins the symptom so the fix
# cannot quietly come undone.
#
# What it checks, after a dump of a few hundred symbols' worth of heap:
#   - every symbol is still fbound / bound
#   - every one still answers its own value (not another symbol's)
#   - allocation still works afterwards (a restore that left the bump cursor
#     wrong shows up here rather than in the symbol sweep)
#   - ordinary evaluation and a builtin call still work
#
# It also runs a NEGATIVE CONTROL.  A smoke that only ever sees a healthy
# session cannot tell "nothing is broken" from "the check cannot see
# breakage": the control run damages one symbol on purpose after the dump and
# the smoke fails if the check still reports a clean session.

set -eu

smoke_script_dir=$(cd "$(dirname "$0")" && pwd)
smoke_root=$(cd "$smoke_script_dir/.." && pwd)
cd "$smoke_root"

nelisp_bin=./target/nelisp
[ -x "$nelisp_bin" ] || nelisp_bin=./target/nelisp.exe
if [ ! -x "$nelisp_bin" ]; then
    printf 'nelisp-arena-dump-live-session-smoke: no standalone binary\n' >&2
    exit 1
fi

smoke_dir=$(mktemp -d "${TMPDIR:-/tmp}/nelisp-arena-dump-live.XXXXXX")
smoke_status=0
cleanup_smoke() {
    smoke_status=$?
    trap - EXIT
    rm -rf "$smoke_dir"
    exit "$smoke_status"
}
trap cleanup_smoke EXIT

cat > "$smoke_dir/dump-live.el" <<'EOF'
(defvar adl-n 300)

(let ((i 0))
  (while (< i adl-n)
    (fset (intern (format "adl-fn-%d" i)) (let ((k i)) (lambda () k)))
    (set (intern (format "adl-var-%d" i)) i)
    (setq i (1+ i))))

(defun adl-check (label)
  (let ((i 0) (bad-fn 0) (bad-var 0) (wrong 0))
    (while (< i adl-n)
      (let ((fn (intern (format "adl-fn-%d" i)))
            (vr (intern (format "adl-var-%d" i))))
        (if (not (fboundp fn))
            (setq bad-fn (1+ bad-fn))
          (let ((v (condition-case nil (funcall fn) (error 'ERR))))
            (unless (equal v i) (setq wrong (1+ wrong)))))
        (if (not (boundp vr))
            (setq bad-var (1+ bad-var))
          (unless (equal (symbol-value vr) i) (setq wrong (1+ wrong)))))
      (setq i (1+ i)))
    (princ (format "ADL %-7s void-fn=%d void-var=%d wrong-value=%d of %d\n"
                   label bad-fn bad-var wrong adl-n))))

(adl-check "before")

(princ (format "ADL dump=%S\n"
               (condition-case e
                   (if (nelisp--arena-dump-image-stream
                        (concat (or (getenv "TMPDIR") "/tmp") "/adl-probe.nlri"))
                       'ok 'nil)
                 (error (list 'err (car e))))))

;; The negative control damages the session the way the defect did, so the
;; check below has something real to catch.
(when (getenv "NELISP_ARENA_DUMP_LIVE_BREAK")
  (fmakunbound (intern "adl-fn-7")))

(adl-check "after")
(princ (format "ADL fresh-alloc=%S\n" (length (make-list 1000 'x))))
(adl-check "after2")
(princ (format "ADL eval=%S\n"
               (condition-case e (let ((z 41)) (1+ z)) (error (list 'err (car e))))))
(princ (format "ADL builtin=%S\n"
               (condition-case e (length (list 1 2 3)) (error (list 'err (car e))))))
(princ "ADL done\n")
(exit)
EOF

TMPDIR="$smoke_dir" "$nelisp_bin" --load "$smoke_dir/dump-live.el" \
    > "$smoke_dir/out" 2>&1 || true

smoke_require() {
    if ! grep -q -- "$1" "$smoke_dir/out"; then
        printf 'nelisp-arena-dump-live-session-smoke: missing %s\n' "$1" >&2
        printf -- '--- session output ---\n' >&2
        cat "$smoke_dir/out" >&2
        exit 1
    fi
}

smoke_require 'ADL before  void-fn=0 void-var=0 wrong-value=0 of 300'
smoke_require 'ADL dump=ok'
smoke_require 'ADL after   void-fn=0 void-var=0 wrong-value=0 of 300'
smoke_require 'ADL fresh-alloc=1000'
smoke_require 'ADL after2  void-fn=0 void-var=0 wrong-value=0 of 300'
smoke_require 'ADL eval=42'
smoke_require 'ADL builtin=3'
smoke_require 'ADL done'

# Negative control: the same run with one symbol deliberately unbound after
# the dump must NOT come out clean.  If it does, the check above is blind and
# its green means nothing.
TMPDIR="$smoke_dir" NELISP_ARENA_DUMP_LIVE_BREAK=1 \
    "$nelisp_bin" --load "$smoke_dir/dump-live.el" \
    > "$smoke_dir/out-control" 2>&1 || true

if grep -q 'ADL after   void-fn=0 void-var=0 wrong-value=0 of 300' "$smoke_dir/out-control"; then
    printf 'nelisp-arena-dump-live-session-smoke: the control run came out clean --\n' >&2
    printf 'the check cannot see a damaged session, so its green proves nothing\n' >&2
    cat "$smoke_dir/out-control" >&2
    exit 1
fi
if ! grep -q 'ADL after   void-fn=1' "$smoke_dir/out-control"; then
    printf 'nelisp-arena-dump-live-session-smoke: the control did not report the\n' >&2
    printf 'one damaged symbol it was told to break\n' >&2
    cat "$smoke_dir/out-control" >&2
    exit 1
fi

printf 'nelisp-arena-dump-live-session-smoke: ok (with negative control)\n'
