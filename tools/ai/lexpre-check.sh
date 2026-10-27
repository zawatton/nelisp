#!/bin/sh
# lexpre ledger checks.  usage: lexpre-check.sh MODE [ARGS]  (run from the repo root)
set -u
ROOT=$(cd "$(dirname "$0")/../.." && pwd); cd "$ROOT" || exit 1
GNU=${GNU_EMACS:-/usr/local/bin/emacs}
mode=${1:-}; shift
run_corpus() { # corpus bin outfile
  cat > target/lexpre/drv/run-corpus.el <<'EOF'
;;; -*- lexical-binding: t; -*-
(let ((file (getenv "LEXPRE_CORPUS")))
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (let ((i 0) (form nil))
      (while (progn (setq form (condition-case nil (read (current-buffer)) (end-of-file 'lexpre-eof)))
                    (not (eq form 'lexpre-eof)))
        (setq i (1+ i))
        (princ (format "%d %S\n" i
                       (condition-case e (list 'ok (eval form t)) (error (list 'err (car e)))))))))
  (kill-emacs 0))
EOF
  if [ "$2" = gnu ]; then
    (ulimit -v 8388608; LEXPRE_CORPUS="$1" timeout 120 "$GNU" -Q --batch -l target/lexpre/drv/run-corpus.el > "$3" 2>/dev/null)
  else
    (ulimit -v 8388608; LEXPRE_CORPUS="$1" timeout 120 "$2" --load target/lexpre/drv/run-corpus.el -- > "$3" 2>/dev/null)
  fi
}
compare() { # corpus bin tag
  mkdir -p target/lexpre/logs
  run_corpus "$1" gnu target/lexpre/logs/$3-gnu.txt || return 1
  run_corpus "$1" "$2" target/lexpre/logs/$3-rt.txt || return 1
  [ -s target/lexpre/logs/$3-gnu.txt ] || { echo "empty GNU output"; return 1; }
  if diff target/lexpre/logs/$3-gnu.txt target/lexpre/logs/$3-rt.txt > target/lexpre/logs/$3-diff.txt; then
    echo "$3 identical: $(wc -l < target/lexpre/logs/$3-gnu.txt) forms"
  else
    echo "$3 DIFFERS"; head -20 target/lexpre/logs/$3-diff.txt; return 1
  fi
  # negative control: a seeded wrong expectation must be caught
  sed '1s/.*/1 (ok SEEDED-WRONG)/' target/lexpre/logs/$3-gnu.txt > target/lexpre/logs/$3-mutant.txt
  if diff -q target/lexpre/logs/$3-mutant.txt target/lexpre/logs/$3-rt.txt > /dev/null; then
    echo "negative control NOT caught"; return 1
  fi
  echo "$3 negative control caught"
}
case "$mode" in
  offline)
    SC=${TMPDIR:-/tmp}/lexpre-offline.$$; mkdir -p "$SC"
    git show HEAD:scripts/nelisp-prelude-bytecode.el > "$SC/seed.el" || exit 1
    cp "$SC/seed.el" scripts/nelisp-prelude-bytecode-lexpre-seed.tmp
    red=$(timeout 250 emacs -Q --batch -l tools/ai/lexpre-offline.el --eval '(lexpre-offline "scripts/nelisp-prelude-bytecode-lexpre-seed.tmp" default-directory)' 2>/dev/null | grep '^CELL identity')
    rm -f scripts/nelisp-prelude-bytecode-lexpre-seed.tmp
    out=$(timeout 250 emacs -Q --batch -l tools/ai/lexpre-offline.el --eval '(lexpre-offline "scripts/nelisp-prelude-bytecode.el" default-directory)' 2>/dev/null)
    rm -rf "$SC"
    echo "seed:  $red"; echo "$out" | grep '^CELL identity'
    [ "$red" = "CELL identity arglist='(x)" ] || exit 1
    echo "$out" | grep -q "^CELL identity arglist='257$" || exit 1
    echo "$out" | grep -q "^CELL string-to-vector arglist='(s)$" || exit 1  # unusable lexical keeps the dynamic cell
    echo "$out" | grep -q "^ROW identity adopt" || exit 1
    # the guard: a special-named argument and a closure body stay dynamic
    emacs -Q --batch --eval '(progn (load (expand-file-name "scripts/nelisp-prelude-bytecode.el") nil t) (setq nelisp-prelude-bytecode-repo-root default-directory) (let ((nelisp-prelude-bytecode-lexical-defuns (quote (f g h)))) (princ (format "%S\n" (list (nelisp-prelude-bytecode--lexical-eligible-p (quote f) (quote (case-fold-search)) (quote (1))) (nelisp-prelude-bytecode--lexical-eligible-p (quote g) (quote (x)) (quote ((lambda (y) y)))) (nelisp-prelude-bytecode--lexical-eligible-p (quote h) (quote (x)) (quote (x))) (nelisp-prelude-bytecode--lexical-eligible-p (quote zz) (quote (x)) (quote (x))))))))' 2>/dev/null | grep -qx "(nil nil t nil)" || { echo "guard check failed"; exit 1; }
    echo "offline ok" ;;
  lexcorpus) compare tools/ai/lexpre-lex-corpus.el "$1" lex ;;
  setcorpus) compare tools/ai/lexpre-set-corpus.el "$1" set ;;
  cells)
    cat > target/lexpre/drv/cells.el <<'EOF'
(dolist (s '(identity purecopy car-safe cdr-safe int-to-string decoded-time-second decoded-time-minute proper-list-p max-char))
  (let ((f (symbol-function s)))
    (princ (format "CELL %s %s %S\n" s (if (byte-code-function-p f) "bytecode" "other")
                   (and (byte-code-function-p f) (aref f 0))))))
(kill-emacs 0)
EOF
    (ulimit -v 8388608; timeout 100 "$1" --load target/lexpre/drv/cells.el -- > target/lexpre/logs/cells.txt 2>&1)
    cat target/lexpre/logs/cells.txt
    grep -q "^CELL identity bytecode 257$" target/lexpre/logs/cells.txt && grep -q "^CELL car-safe bytecode 257$" target/lexpre/logs/cells.txt ;;
  ir)
    # ir ROW LIMIT: reads target/lexpre/cg/lexpre-ir.txt written by the callgrind run
    v=$(awk -v r="$1" '$1==r {print $2}' target/lexpre/cg/lexpre-ir.txt | tail -1)
    echo "row $1 Ir/trip=$v limit=$2"; [ -n "$v" ] && [ "$v" -le "$2" ] ;;
  setcost)
    sh target/lexpre/cg/setcost.sh > target/lexpre/logs/setcost.txt 2>&1; cat target/lexpre/logs/setcost.txt
    grep -q "^SETCOST ok" target/lexpre/logs/setcost.txt ;;
  timing)
    python3 target/lexpre/timing/s4check.py > target/lexpre/logs/timing.txt 2>&1; cat target/lexpre/logs/timing.txt
    grep -q "^TIMING ok" target/lexpre/logs/timing.txt ;;
  *) echo "unknown mode"; exit 2 ;;
esac
