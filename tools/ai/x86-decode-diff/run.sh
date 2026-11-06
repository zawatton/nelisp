#!/usr/bin/env bash
# Usage: run.sh EVIDENCE_DIR
# Dump capstone's and the Elisp decoder's view of every function range in an
# evidence directory and diff them.  Prints DIFF-CLEAN or the first differences.
# Environment: EMACS (default emacs-31.1), WORK (default: a fresh temp dir).
here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)" || exit 1
root="$(cd "$here/../../.." && pwd)" || exit 1
dir="${1:?usage: run.sh EVIDENCE_DIR}"
emacs_bin="${EMACS:-emacs-31.1}"
work="${WORK:-$(mktemp -d)}"
mkdir -p "$work" || exit 1
ulimit -v 8388608
python3 "$here/dump-capstone.py" "$dir" "$work/capstone.tsv" "$work/ranges.tsv" || exit 1
(cd "$root" && "$emacs_bin" --batch -Q -L lisp -l "$here/dump-elisp.el" \
  compiled "$work/ranges.tsv" "$work/elisp.tsv") 2>"$work/elisp.log" || { tail -5 "$work/elisp.log"; exit 1; }
tail -1 "$work/elisp.log"
total="$(wc -l <"$work/capstone.tsv")"
if cmp -s "$work/capstone.tsv" "$work/elisp.tsv"; then
  echo "DIFF-CLEAN ($total lines, $(grep -c RANGE-MISMATCH "$work/capstone.tsv") incomplete ranges) $dir"
else
  diff "$work/capstone.tsv" "$work/elisp.tsv" >"$work/diff.txt"
  echo "DIFF: $(grep -c '^[<>]' "$work/diff.txt") differing lines of $total ($work/diff.txt)"
  head -20 "$work/diff.txt"
  exit 1
fi
