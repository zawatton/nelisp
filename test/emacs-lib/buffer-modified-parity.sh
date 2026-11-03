#!/usr/bin/env bash
# Exact GNU Emacs 31.1 / standalone comparison of per-buffer modification state.
#
# Usage: buffer-modified-parity.sh [--cases FILE] [--bundle FILE]
#                                  [--candidate DIR] [--expect-defect SHA256]
#   --cases FILE           case file, one form per line (default: the first group)
#   --bundle FILE          bootstrap bundle to load (default build/nemacs-bootstrap.el)
#   --candidate DIR        hot-load DIR/files-standalone-buffer.el,
#                          DIR/emacs-cc-fileio-2.el and, when present,
#                          DIR/with-silent-modifications.el over the bundle
#   --expect-defect SHA    pass only when the standalone transcript differs from
#                          GNU and has exactly this SHA256 (predecessor control)
# Environment: NELISP_BIN (standalone binary), EMACS (GNU Emacs 31.1 host).
set -u
here=$(cd "$(dirname "$0")/../.." && pwd)
cases="$here/test/nelisp-emacs-lib/buffer-modified-parity-cases.el"
bin=${NELISP_BIN:-$here/target/nelisp}
host=${EMACS:-emacs}
bundle="$here/build/nemacs-bootstrap.el"
candidate=""
defect=""
fail() { printf 'buffer-modified-parity: FAIL: %s\n' "$*" >&2; exit 1; }
while [ $# -gt 0 ]; do
  case "$1" in
    --cases) [ $# -ge 2 ] || fail "--cases needs a value"; cases=$(cd "$(dirname "$2")" && pwd)/$(basename "$2"); shift 2 ;;
    --bundle) [ $# -ge 2 ] || fail "--bundle needs a value"; bundle=$(cd "$(dirname "$2")" && pwd)/$(basename "$2"); shift 2 ;;
    --candidate) [ $# -ge 2 ] || fail "--candidate needs a value"; candidate=$(cd "$2" && pwd) || fail "bad candidate dir"; shift 2 ;;
    --expect-defect) [ $# -ge 2 ] || fail "--expect-defect needs a value"; defect=$2; shift 2 ;;
    *) fail "unknown option $1" ;;
  esac
done
[ -f "$bin" ] && [ -x "$bin" ] || fail "standalone binary missing: $bin"
[ -f "$bundle" ] || fail "bundle missing: $bundle"
[ -f "$cases" ] || fail "cases missing: $cases"
"$host" --version 2>/dev/null | head -1 | grep -q '^GNU Emacs 31\.1' || fail "GNU baseline must be Emacs 31.1"
work=$(mktemp -d) || fail "mktemp"
trap 'rm -rf "$work"' EXIT
count=$(grep -c '^(' "$cases")
[ "$count" -gt 0 ] || fail "no cases"
{
  printf ';;; -*- lexical-binding: t; -*-\n'
  grep '^(' "$cases" | while IFS= read -r form; do
    printf "(prin1 (condition-case err %s (error (cons 'ERR err)))) (terpri)\n" "$form"
  done
  printf '(princ "P-DONE") (terpri)\n'
} > "$work/driver.el"
{
  printf '(load %s nil t)\n' "\"$bundle\""
  if [ -n "$candidate" ]; then
    printf "(fmakunbound 'recent-auto-save-p)\n(fmakunbound 'set-buffer-auto-saved)\n"
    [ ! -f "$candidate/with-silent-modifications.el" ] \
      || printf '(load %s nil t)\n' "\"$candidate/with-silent-modifications.el\""
    printf '(load %s nil t)\n' "\"$candidate/files-standalone-buffer.el\""
    printf '(load %s nil t)\n' "\"$candidate/emacs-cc-fileio-2.el\""
  fi
  printf '(load %s nil t)\n' "\"$work/driver.el\""
} > "$work/run.el"
timeout 60 "$host" -Q --batch -l "$work/driver.el" > "$work/host.out" 2> "$work/host.err" \
  || fail "host run failed: $(tail -c 300 "$work/host.err")"
[ "$(wc -l < "$work/host.out")" -eq $((count + 1)) ] && [ "$(tail -1 "$work/host.out")" = P-DONE ] \
  || fail "host transcript incomplete"
(cd "$here" && timeout 100 "$bin" --load "$work/run.el" > "$work/native.out" 2> "$work/native.err")
rc=$?
# `--load' echoes the value of the last form after the transcript; compare
# the protocol up to and including its P-DONE terminator only.
sed -i '/^P-DONE$/q' "$work/native.out"
sum=$(sha256sum < "$work/native.out" | awk '{print $1}')
if [ -n "$defect" ]; then
  cmp -s "$work/host.out" "$work/native.out" && fail "predecessor unexpectedly matches GNU"
  [ "$sum" = "$defect" ] || fail "predecessor transcript $sum is not the recorded defect"
  printf 'buffer-modified-parity: PASS (recorded defect reproduced, %s rows)\n' "$count"
  exit 0
fi
if [ "$rc" -ne 0 ] || ! cmp -s "$work/host.out" "$work/native.out"; then
  printf 'standalone rc=%s sha256=%s\n' "$rc" "$sum" >&2
  diff <(paste -d '|' <(grep '^(' "$cases"; echo P-DONE) "$work/host.out") \
       <(paste -d '|' <(grep '^(' "$cases"; echo P-DONE) "$work/native.out") | cut -c1-400 | head -80 >&2
  tail -c 400 "$work/native.err" >&2
  fail "transcripts differ"
fi
printf 'buffer-modified-parity: PASS (%s rows match GNU)\n' "$count"
