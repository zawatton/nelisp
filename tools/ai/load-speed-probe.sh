#!/bin/sh
# load-speed-probe.sh MODE --- probes behind tools/ai/load-speed-progress.org.
#
# Modes (exit 0 = criterion holds):
#   vendor-ratio     `load' of the largest loadable vendored .el is at most
#                    1.2x a read+eval loop over the same file (median of 3)
#   semantics        GNU `load' semantics, diffed against GNU Emacs
#   elc              a GNU-byte-compiled .elc loads and behaves as in GNU Emacs
#   ndtw-ratio       the same ratio on newDTW game data (needs NDTW_ROOT)
#   ndtw-scenarios   newDTW owner-check frames equal GNU's; runtime loads <= 8 s
#
# Environment: NELISP (binary, default target/nelisp.exe or target/nelisp),
# EMACS (GNU Emacs, default `emacs'), NDTW_ROOT (newDTW checkout),
# VENDOR_FILE (override the vendored file), RATIO_MAX (default 1.2).
set -u
here=$(cd "$(dirname "$0")" && pwd)
root=$(cd "$here/../.." && pwd)
cd "$root" || exit 2

mode=${1:-}
EMACS=${EMACS:-emacs}
RATIO_MAX=${RATIO_MAX:-1.2}
if [ -z "${NELISP:-}" ]; then
  if [ -x target/nelisp.exe ]; then NELISP=$root/target/nelisp.exe
  else NELISP=$root/target/nelisp; fi
fi
[ -x "$NELISP" ] || { echo "no nelisp binary at $NELISP" >&2; exit 2; }
tmp=$(mktemp -d "${TMPDIR:-/tmp}/load-speed-probe.XXXXXX") || exit 2
[ -n "${PROBE_KEEP:-}" ] && echo "keeping $tmp" >&2 || trap 'rm -rf "$tmp"' EXIT

nl() { # run nelisp with the 8 GiB address-space cap and a timeout
  t=$1; shift
  (ulimit -v 8388608 2>/dev/null; timeout "$t" "$NELISP" "$@")
}

# Timing driver.  Env: LF (file), MODE (load|eval), LPRE (files loaded first,
# ';'-separated).  Prints "<mode> <seconds>" on success.
cat > "$tmp/time.el" <<'EOF'
(let* ((f (getenv "LF")) (mode (getenv "MODE")) (pre (getenv "LPRE"))
       (lex (with-temp-buffer
              (insert-file-contents f nil 0 400)
              (and (re-search-forward "lexical-binding: *t" nil t) t))))
  (dolist (p (and pre (split-string pre ";" t))) (load p nil t))
  (let ((t0 (float-time)))
    (if (equal mode "load")
        (load f nil t)
      (with-temp-buffer
        (insert-file-contents f)
        (condition-case nil
            (while t (eval (read (current-buffer)) lex))
          (end-of-file nil))))
    (princ (format "RESULT %s %.3f\n" mode (- (float-time) t0)))))
EOF

secs() { # secs FILE MODE [PRE] -> seconds on stdout, empty on failure
  out=$(LF=$1 MODE=$2 LPRE=${3:-} nl 580 --load "$tmp/time.el" 2>&1) || return 1
  echo "$out" | sed -n 's/^RESULT [a-z]* \([0-9.]*\)$/\1/p' | head -1
}

median3() { printf '%s\n' "$1" "$2" "$3" | sort -n | sed -n 2p; }

ratio_of() { # ratio_of FILE [PRE]: median load / median eval, sets L E R
  f=$1; pre=${2:-}
  l1=$(secs "$f" load "$pre"); e1=$(secs "$f" eval "$pre")
  [ -n "$l1" ] && [ -n "$e1" ] || return 1
  l2=$(secs "$f" load "$pre"); e2=$(secs "$f" eval "$pre")
  l3=$(secs "$f" load "$pre"); e3=$(secs "$f" eval "$pre")
  L=$(median3 "$l1" "${l2:-$l1}" "${l3:-$l1}"); E=$(median3 "$e1" "${e2:-$e1}" "${e3:-$e1}")
  R=$(awk -v l="$L" -v e="$E" 'BEGIN{ if (e < 0.05) e = 0.05; printf "%.2f", l / e }')
}

check_ratio() { # check_ratio LABEL FILE [PRE]
  if ratio_of "$2" "${3:-}"; then
    echo "$1: load ${L}s eval-loop ${E}s ratio ${R} (max $RATIO_MAX)"
    awk -v r="$R" -v m="$RATIO_MAX" 'BEGIN{exit !(r <= m)}'
  else
    echo "$1: could not time $2" >&2; return 1
  fi
}

case "$mode" in
vendor-ratio)
  if [ -n "${VENDOR_FILE:-}" ]; then pick=$VENDOR_FILE; else
    pick=
    for c in $(find vendor -name '*.el' -size +40k -printf '%s %p\n' | sort -rn | head -40 | cut -d' ' -f2); do
      a=$(secs "$c" load); b=$(secs "$c" eval)
      if [ -n "$a" ] && [ -n "$b" ]; then pick=$c; break; fi
    done
  fi
  [ -n "$pick" ] || { echo "no vendored file loads cleanly" >&2; exit 1; }
  check_ratio "vendor $pick" "$pick"
  ;;

semantics)
  d=$tmp/sem; mkdir -p "$d"
  cat > "$d/inner.el" <<'EOF'
;;; inner.el --- nested  -*- lexical-binding: t -*-
(setq sem-inner-lfn (file-name-nondirectory load-file-name))
(defun sem-inner-fn () 1)
(provide 'sem-inner)
EOF
  cat > "$d/lex.el" <<'EOF'
;;; lex.el  -*- lexical-binding: t -*-
(setq sem-lex-flag lexical-binding)
(setq sem-lex-closure (let ((x 5)) (funcall (lambda () x))))
EOF
  cat > "$d/dyn.el" <<'EOF'
;;; dyn.el  -*- lexical-binding: nil -*-
(setq sem-dyn-flag lexical-binding)
EOF
  cat > "$d/main.el" <<'EOF'
;;; main.el  -*- lexical-binding: t -*-
(setq sem-lfn (file-name-nondirectory load-file-name))
(setq sem-dir-ok (equal (file-name-directory load-file-name)
                        (file-name-directory (expand-file-name load-file-name))))
(setq sem-dollar (and (stringp #$) (file-name-nondirectory #$)))
(defmacro sem-twice (x) `(* 2 ,x))
(defun sem-use-macro () (sem-twice 21))
(progn (defmacro sem-thrice (x) `(* 3 ,x)) (defun sem-use-macro2 () (sem-thrice 5)))
(defconst sem-etc (eval-when-compile (* 6 7)))
(defconst sem-eac (eval-and-compile (+ 1 2)))
(setq sem-before-nested load-file-name)
(load (expand-file-name "inner.el" (file-name-directory load-file-name)) nil t)
(setq sem-after-nested-same (equal sem-before-nested load-file-name))
(defvar sem-cond-trim (cond (nil 1) (t 2)))
(setq sem-setq-pairs (let (a b) (setq a 1 b 2) (list a b)))
(setq sem-funcall (funcall #'+ 1 2))
(setq sem-hook-ran nil)
EOF
  cat > "$d/driver.el" <<'EOF'
(let* ((dir (getenv "SEM_DIR"))
       (main (expand-file-name "main.el" dir)))
  (defvar sem-after-log nil)
  (eval-after-load main (lambda () (push 'eval-after-load sem-after-log)))
  (push (lambda (f) (push (file-name-nondirectory f) sem-after-log)) after-load-functions)
  (princ (format "noerror-missing=%S\n" (load (expand-file-name "nope.el" dir) t t)))
  (princ (format "missing-error=%S\n"
                 (condition-case e (load (expand-file-name "nope.el" dir) nil t) (error (car e)))))
  (princ (format "load-ret=%S\n" (load main nil t)))
  (princ (format "lfn=%S dir-ok=%S\n" sem-lfn sem-dir-ok))
  ;; Known gap, identical before this work: `#$' reads as nil in source.
  (message "known-gap #$=%S" sem-dollar)
  (princ (format "lfn-after=%S\n" (and load-file-name (file-name-nondirectory load-file-name))))
  (princ (format "macro-in-file=%S %S\n" (sem-use-macro) (sem-use-macro2)))
  (princ (format "eval-when-compile=%S eval-and-compile=%S\n" sem-etc sem-eac))
  (princ (format "nested=%S inner-lfn=%S featurep=%S\n" sem-after-nested-same sem-inner-lfn (featurep 'sem-inner)))
  (princ (format "cond=%S setq-pairs=%S funcall=%S\n" sem-cond-trim sem-setq-pairs sem-funcall))
  (princ (format "after-log=%S\n" (reverse sem-after-log)))
  (let ((entry (assoc main load-history)))
    (princ (format "load-history=%S\n" (and entry t))))
  (load (expand-file-name "lex.el" dir) nil t)
  (load (expand-file-name "dyn.el" dir) nil t)
  (princ (format "lex=%S closure=%S\n" sem-lex-flag sem-lex-closure))
  ;; Known gap, identical before this work: a `lexical-binding: nil' cookie is ignored.
  (message "known-gap lexical-binding-nil-cookie=%S" sem-dyn-flag)
  (kill-emacs 0))
EOF
  SEM_DIR=$d nl 120 --load "$d/driver.el" > "$tmp/sem-nelisp.txt" 2> "$tmp/sem-nelisp.err"
  SEM_DIR=$d "$EMACS" -Q --batch -l "$d/driver.el" > "$tmp/sem-gnu.txt" 2> "$tmp/sem-gnu.err"
  tr -d '\r' < "$tmp/sem-gnu.txt" > "$tmp/sem-gnu.n"; tr -d '\r' < "$tmp/sem-nelisp.txt" > "$tmp/sem-nelisp.n"
  if diff "$tmp/sem-gnu.n" "$tmp/sem-nelisp.n" > "$tmp/sem.diff"; then
    echo "semantics: NeLisp equals GNU ($(wc -l < "$tmp/sem-gnu.txt") lines)"
  else
    echo "semantics: differences (< GNU, > NeLisp)"; head -20 "$tmp/sem.diff"; head -3 "$tmp/sem-nelisp.err"; exit 1
  fi
  ;;

elc)
  d=$tmp/elc; mkdir -p "$d"
  cat > "$d/lib.el" <<'EOF'
;;; lib.el --- elc probe  -*- lexical-binding: t -*-
(defvar elc-state nil "State variable.")
(defun elc-add (a b)
  "Add A and B; ドキュメント with multibyte text."
  (+ a b))
(defun elc-shared-1 () (concat "a shared constant string that the compiler may share" "x"))
(defun elc-shared-2 () (concat "a shared constant string that the compiler may share" "y"))
(defun elc-closure (n) (let ((k n)) (lambda (x) (+ x k))))
(defun elc-loop (n) (let ((s 0)) (dotimes (i n) (setq s (+ s i))) s))
(defun elc-cond (x) (cond ((stringp x) 's) ((numberp x) 'n) (t 'o)))
(defun elc-table () (let ((h (make-hash-table :test 'eq))) (puthash 'a 1 h) (puthash 'b 2 h) (gethash 'b h)))
(defun elc-case (x) (pcase x (`(,a . ,b) (list b a)) ('sym 'matched) (_ 'none)))
(defun elc-opt (a &optional b &rest c) (list a b c))
(defconst elc-const (vector 1 2 3))
(provide 'elc-lib)
EOF
  "$EMACS" -Q --batch -f batch-byte-compile "$d/lib.el" > "$tmp/elc-compile.log" 2>&1 || { echo "elc: GNU compile failed" >&2; cat "$tmp/elc-compile.log" >&2; exit 2; }
  [ -f "$d/lib.elc" ] || { echo "elc: no lib.elc" >&2; exit 2; }
  cat > "$d/driver.el" <<'EOF'
(load (expand-file-name "lib.elc" (getenv "ELC_DIR")) nil t)
(princ (format "%S\n" (list (elc-add 1 2) (elc-shared-1) (elc-shared-2)
                            (funcall (elc-closure 10) 5) (elc-loop 10)
                            (mapcar #'elc-cond '("s" 1 sym)) (elc-table)
                            (elc-case '(1 . 2)) (elc-case 'sym) (elc-case 3)
                            (elc-opt 1) (elc-opt 1 2 3 4) elc-const
                            (featurep 'elc-lib)
                            (byte-code-function-p (symbol-function 'elc-add)))))
(kill-emacs 0)
EOF
  ELC_DIR=$d nl 120 --load "$d/driver.el" > "$tmp/elc-nelisp.txt" 2> "$tmp/elc-nelisp.err"
  ELC_DIR=$d "$EMACS" -Q --batch -l "$d/driver.el" > "$tmp/elc-gnu.txt" 2> "$tmp/elc-gnu.err"
  tr -d '\r' < "$tmp/elc-gnu.txt" > "$tmp/elc-gnu.n"; tr -d '\r' < "$tmp/elc-nelisp.txt" > "$tmp/elc-nelisp.n"
  diff "$tmp/elc-gnu.n" "$tmp/elc-nelisp.n" > "$tmp/elc.diff" || { echo "elc: differs"; cat "$tmp/elc.diff"; head -3 "$tmp/elc-nelisp.err"; exit 1; }
  # Labelled (#N=) strings shared between byte-code literals are what GNU
  # emits for large files; check that shape directly as well.
  printf '(setq r1 (quote (#[0 #1="\\300" [a] 3] #[0 #1# [b] 3])))\n(princ (format "%%S\\n" r1))\n' > "$d/label.el"
  nl 60 --load "$d/label.el" > "$tmp/label.txt" 2>&1 || { echo "elc: label form failed"; head -3 "$tmp/label.txt"; exit 1; }
  echo "elc: lib.elc loads and matches GNU; shared-code-string form reads"
  ;;

ndtw-ratio)
  [ -n "${NDTW_ROOT:-}" ] || { echo "NDTW_ROOT not set" >&2; exit 2; }
  rt=$NDTW_ROOT/nelisp_runtime
  rc=0
  check_ratio "gamedata-state.el" "$rt/gamedata-state.el" || rc=1
  check_ratio "gamedata-conditional.el" "$rt/gamedata-conditional.el" "$rt/game-runner.el" || rc=1
  exit $rc
  ;;

ndtw-scenarios)
  [ -n "${NDTW_ROOT:-}" ] || { echo "NDTW_ROOT not set" >&2; exit 2; }
  rt=$NDTW_ROOT/nelisp_runtime
  ids=death-list,enc-115,enc-157,enc-175,enc-179,enc-32,enc-52,shop-126,shop-302
  mkdir -p "$tmp/out-nl" "$tmp/out-gnu"
  ( cd "$NDTW_ROOT" && OC_SCENARIOS=$ids OC_OUT=$tmp/out-nl nl 580 --load nelisp_runtime/owner-check/scenarios.el >/dev/null 2>&1 )
  ( cd "$NDTW_ROOT" && OC_SCENARIOS=$ids OC_OUT=$tmp/out-gnu "$EMACS" -Q --batch -l nelisp_runtime/owner-check/scenarios.el >/dev/null 2>&1 )
  n=0; bad=0
  for f in "$tmp"/out-gnu/*-frame.json; do
    [ -f "$f" ] || continue
    n=$((n+1)); b=$(basename "$f")
    cmp -s "$f" "$tmp/out-nl/$b" || { bad=$((bad+1)); echo "frame differs: $b"; }
  done
  [ "$n" -ge 9 ] || { echo "ndtw-scenarios: only $n GNU frames" >&2; exit 1; }
  echo "ndtw-scenarios: $n frames, $bad differ"
  [ "$bad" -eq 0 ] || exit 1
  cat > "$tmp/all.el" <<'EOF'
(let ((t0 (float-time)) (rt (getenv "RT")))
  (dolist (f '("game-runner.el" "gamedata-simple.el" "gamedata-conditional.el" "gamedata-state.el"))
    (load (expand-file-name f rt) nil t))
  (princ (format "RESULT all %.3f\n" (- (float-time) t0))))
EOF
  t=$(RT=$rt nl 580 --load "$tmp/all.el" 2>&1 | sed -n 's/^RESULT all \([0-9.]*\)$/\1/p' | head -1)
  [ -n "$t" ] || { echo "ndtw-scenarios: runtime load failed" >&2; exit 1; }
  echo "ndtw-scenarios: runtime load ${t}s (max 8)"
  awk -v t="$t" 'BEGIN{exit !(t <= 8)}'
  ;;

*)
  echo "usage: $0 vendor-ratio|semantics|elc|ndtw-ratio|ndtw-scenarios" >&2
  exit 2
  ;;
esac
