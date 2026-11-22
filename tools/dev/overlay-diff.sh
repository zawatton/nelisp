#!/usr/bin/env bash
# overlay-diff.sh: run one fixed corpus on (a) base binary + overlay and (b) a new
# binary, normalise, diff.  Exit 0 identical, 1 any difference, 2 usage/run failure.
#
#   overlay-diff.sh --lane LANE --base-home DIR --base-dump FILE [options]
#     --lane DIR         lane worktree: source of the overlay files, the compiler and
#                        the AOT cases
#     --base-home DIR    checkout whose target/nelisp is the BASE binary (a)
#     --base-dump FILE   flat dump baked from that binary (see README; fast boot)
#     --overlay F        overlay .el, repeatable, loaded in order into (a) (default:
#                        every T2 file of the lane vs --base-sha, via overlay-manifest)
#     --base-sha SHA     base for the default overlay (default 881964d35)
#     --new-home DIR --new-dump FILE | --new-nlri
#                        (b): a different binary + flat dump, or its
#                        build/nemacs-runtime.nlri.  Omitted: (b) = (a) again, same
#                        overlay (self-diff)
#     --corpus LIST      comma list of syn,gg,aot (default all)
#     --out DIR          keep outputs (default: a temp dir)
#     --cpus LIST        taskset cpus (default 6,22)
#     --inject-row       negative control: add one row to (b)'s normalised output
# Corpus: syn = tools/dev/overlay-corpus/syntax-parity.el R rows; gg = gg-driver.el
# checksums (timings dropped); aot = the AOT lisp-arith differential cases compiled by
# the lane's compiler and run natively in the image.  Output rows are compared as text.
# Verdicts license "correct and worth batching", never "integrated" or a perf number.
set -uo pipefail
here=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
corp=$here/overlay-corpus
lane= bhome= bdump= nhome= ndump= nnlri=0 out= cpus=6,22 corpus=syn,gg,aot inject=0 bsha=881964d35
overlays=()
die() { echo "overlay-diff: $*" >&2; exit 2; }
while [ $# -gt 0 ]; do case $1 in
  --lane) lane=$2; shift 2;; --base-home) bhome=$2; shift 2;; --base-dump) bdump=$2; shift 2;;
  --overlay) overlays+=("$2"); shift 2;; --base-sha) bsha=$2; shift 2;;
  --new-home) nhome=$2; shift 2;; --new-dump) ndump=$2; shift 2;; --new-nlri) nnlri=1; shift;;
  --corpus) corpus=$2; shift 2;; --out) out=$2; shift 2;; --cpus) cpus=$2; shift 2;;
  --inject-row) inject=1; shift;; *) die "unknown option $1";; esac; done
[ -n "$lane" ] && [ -n "$bhome" ] && [ -n "$bdump" ] || die "need --lane --base-home --base-dump"
lane=$(cd "$lane" && pwd); bhome=$(cd "$bhome" && pwd)
[ -x "$bhome/target/nelisp" ] && [ -r "$bdump" ] || die "base binary or dump missing"
case $cpus in *11*|*13*|*14*|*15*) die "cpus must not include 11,13,14,15";; esac
[ -n "$out" ] || out=$(mktemp -d); mkdir -p "$out"
self=0; [ -z "$nhome" ] && self=1
if [ $self = 0 ]; then nhome=$(cd "$nhome" && pwd); [ -x "$nhome/target/nelisp" ] || die "new binary missing"
  [ $nnlri = 1 ] || [ -r "$ndump" ] || die "need --new-dump or --new-nlri"; fi

if [ ${#overlays[@]} -eq 0 ]; then
  while IFS=$'\t' read -r _ tier file _; do
    [ "$tier" = T2 ] && overlays+=("$lane/$file")
  done < <("$here/overlay-manifest" --repo "$lane" --base "$bsha" --lane "x=$lane" 2>/dev/null)
fi
loads=""; for f in "${overlays[@]}"; do [ -r "$f" ] || die "overlay $f unreadable"
  loads="$loads(load \"$f\" nil t t) "; done

# run MODE HOME DUMP OVERLAYLOADS FORM -> stdout (stderr to $out/run.err)
run() {
  local mode=$1 home=$2 dump=$3 pre=$4 form=$5
  ( cd "$home"; ulimit -v 8388608
    if [ "$mode" = flat ]; then
      NELISP_HOME=$home exec timeout 900 taskset -c "$cpus" target/nelisp --cold-load-from "$dump" \
        --eval "(progn (nemacs-runtime-dump-reinit) $pre $form)" </dev/null
    else
      NELISP_HOME=$home exec timeout 900 taskset -c "$cpus" target/nelisp exec-runtime-image \
        build/nemacs-runtime.nlri "(progn $pre $form)" </dev/null
    fi ) 2>>"$out/run.err"
}
norm_gg() { grep -E '^(GG |GG-DONE)' | awk '/^GG /{ $4=""; } {print}'; }

if echo ",$corpus," | grep -q ,aot,; then
  ( ulimit -v 8388608; timeout 600 taskset -c "$cpus" emacs -Q --batch -L "$lane/lisp" -L "$lane/src" \
      -l "$corp/aot-run.el" --eval "(overlay-aot-prepare \"$lane\" \"$out/aot\")" </dev/null ) >"$out/aot-prep.log" 2>&1 \
    || die "AOT corpus compile failed (see $out/aot-prep.log)"
fi

collect() { # side mode home dump pre
  local side=$1 mode=$2 home=$3 dump=$4 pre=$5 o=$out/$1.txt
  : >"$o"
  if echo ",$corpus," | grep -q ,syn,; then
    run "$mode" "$home" "$dump" "$pre" "(load \"$corp/syntax-parity.el\" nil t t)" | grep -E '^(R |SP-DONE)' | sed 's/^/syn /' >>"$o"; fi
  if echo ",$corpus," | grep -q ,gg,; then
    run "$mode" "$home" "$dump" "$pre" "(progn (load \"$corp/gg-driver.el\" nil t t) (gg-run))" | norm_gg | sed 's/^/gg /' >>"$o"; fi
  if echo ",$corpus," | grep -q ,aot,; then
    run "$mode" "$home" "$dump" "$pre" "(load \"$out/aot/run.el\" nil t t)" | grep -E '^(AOT |AOT-DONE)' >>"$o"; fi
}
collect a flat "$bhome" "$bdump" "$loads"
if [ $self = 1 ]; then collect b flat "$bhome" "$bdump" "$loads"
elif [ $nnlri = 1 ]; then collect b nlri "$nhome" "" ""
else collect b flat "$nhome" "$ndump" ""; fi
[ $inject = 1 ] && echo "syn R injected-difference (negative control)" >>"$out/b.txt"

for s in a b; do
  for c in syn gg aot; do
    echo ",$corpus," | grep -q ",$c," && ! grep -q -E "^($c |AOT-DONE)" "$out/$s.txt" && \
      die "side $s produced no $c rows (run failed; see $out/run.err)"
  done
done
echo "rows: a=$(wc -l <"$out/a.txt") b=$(wc -l <"$out/b.txt") overlay=${#overlays[@]} file(s) out=$out"
if diff "$out/a.txt" "$out/b.txt" >"$out/diff.txt"; then echo "OVERLAY-DIFF CLEAN"; exit 0; fi
echo "OVERLAY-DIFF DIFFERENT ($(grep -c '^[<>]' "$out/diff.txt") lines)"; head -20 "$out/diff.txt"; exit 1
