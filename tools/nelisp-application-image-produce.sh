#!/bin/bash
# Doc 240 D1: produce a saved application image from a generated native driver.
# Usage: nelisp-application-image-produce.sh BINARY DRIVER ROW OUT_IMAGE
# Writes OUT_IMAGE, OUT_IMAGE.main.el and OUT_IMAGE.manifest, or nothing at all.
# The driver's own preparation forms run unchanged (pins, preflight, install,
# registration checks); the heap is dumped only after all of them passed.
#
# Pre-dump compilation (Doc 240 S3): the producing process runs with
# NELISP_FX_DUMP=all in its clean env.  The prelude's dump wrapper
# (nelisp--arena-dump-image-stream) then calls nelisp--fx-warm-for-dump, which
# runs nelisp--fx-register-prelude: it compiles every function that is still
# pending (defined, never called) and records the compiled body in
# nelisp--fx-table, keyed by closure body.  Function cells are not replaced
# (verify's eq check on the recorded cells is unaffected).  Without it the
# driver helpers compile on first call in every process that loads the image.
# The prelude wrapper swallows warming errors ((condition-case nil ... (error 0))),
# so the producer does not rely on it: its wrapper.el calls
# nelisp--fx-warm-for-dump itself before the record and dump (errors are
# loud), then asserts that each of bench-input/bench-check-input/bench-validate defined
# by the preparation has a
# compiled (not `none') record in nelisp--fx-table and are no longer in
# nelisp--fx-pending, and that nothing at all is pending.  Any failure aborts
# the production and leaves no image.
set -u
[ $# -eq 4 ] || { echo "usage: $0 BINARY DRIVER ROW OUT_IMAGE" >&2; exit 2; }
BIN=$1; DRV=$2; ROW=$3; OUT=$4
HERE=$(cd "$(dirname "$0")" && pwd) || exit 1
LIB=$HERE/nelisp-application-image.el
[ -x "$BIN" ] || { echo "produce: binary not executable: $BIN" >&2; exit 1; }
[ -r "$DRV" ] || { echo "produce: driver unreadable: $DRV" >&2; exit 1; }
[ -r "$LIB" ] || { echo "produce: missing $LIB" >&2; exit 1; }
mkdir -p "$(dirname "$OUT")" || exit 1
OUT=$(cd "$(dirname "$OUT")" && pwd)/$(basename "$OUT")
DRV=$(cd "$(dirname "$DRV")" && pwd)/$(basename "$DRV")
BIN=$(cd "$(dirname "$BIN")" && pwd)/$(basename "$BIN")
rm -f "$OUT" "$OUT.tmp" "$OUT.main.el" "$OUT.manifest" "$OUT.manifest.tmp"
WORK=$(mktemp -d "$OUT.work.XXXXXX") || exit 1
fail() {
  echo "produce: FAILED: $*" >&2
  rm -rf "$WORK"; rm -f "$OUT" "$OUT.tmp" "$OUT.main.el" "$OUT.manifest" "$OUT.manifest.tmp"
  exit 1
}
BSHA=$(sha256sum "$BIN" | cut -d' ' -f1) || fail "sha256 of binary"

# Split DRIVER with a string/comment/char-literal aware top-level reader.
# Shape is asserted: the last form is (kill-emacs 0), the one before it is the
# benchmark main (let* ((row (getenv "BENCH_ROW")) ...); everything before the
# main is preparation and is kept byte-for-byte.  A driver that loads a
# sibling bootstrap (nested row) gets that file copied next to the prep file
# so that its load-file-name relative lookup resolves unchanged.
# Pins: the quoted `pins' binding is cut out of the preparation text (or the
# bootstrap text) as raw source; exactly one occurrence must exist.
python3 -I - "$DRV" "$ROW" "$WORK" "$OUT" "$LIB" "$BSHA" <<'PY' || fail "driver split"
import sys, re, os, hashlib
drv, row, work, out, lib, bsha = sys.argv[1:]
text = open(drv, encoding="utf-8").read()

def forms(s, first_only=False):
    """Top-level form (start, end) offsets; strings, comments, ?x literals aware."""
    res, i, n, depth, start = [], 0, len(s), 0, None
    while i < n:
        c = s[i]
        if c == ";":
            while i < n and s[i] != "\n": i += 1
            continue
        if c == '"':
            if depth == 0: raise SystemExit("top-level string")
            i += 1
            while s[i] != '"': i += 2 if s[i] == "\\" else 1
            i += 1; continue
        if c == "?" and depth > 0 and i > 0 and s[i-1] in " \t\n('":
            i += 3 if s[i+1] == "\\" else 2; continue
        if c == "(":
            if depth == 0: start = i
            depth += 1
        elif c == ")":
            depth -= 1
            if depth < 0: raise SystemExit("unbalanced )")
            if depth == 0:
                res.append((start, i + 1))
                if first_only: return res
        elif depth == 0 and not c.isspace():
            raise SystemExit("top-level atom at %d" % i)
        i += 1
    if depth: raise SystemExit("unbalanced (")
    return res

fs = forms(text)
src = [text[a:b] for a, b in fs]
assert src[-1] == "(kill-emacs 0)", "last form is not (kill-emacs 0)"
assert src[-2].startswith('(let* ((row (getenv "BENCH_ROW"))'), "main form not found"
bootstrap = "runtime-bootstrap-on.el" in text
want = 11 if bootstrap else 20
assert len(fs) == want, "unexpected driver shape: %d forms, expected %d" % (len(fs), want)
# cross-check against the column-0 split used by the S0 probe
col0 = [l for l in text.split("\n") if l.startswith("(")]
assert len(col0) == len(fs), "column-0 form count disagrees with reader"
first = text[:fs[0][0]]
assert "lexical-binding" in first.split("\n")[0], "no lexical-binding cookie"
cookie = first.split("\n")[0] + "\n"
prep_body = text[fs[0][0]:fs[-2][0]]
main_body = text[fs[-2][0]:]
prep_text = first + prep_body
open(work + "/prep.el", "w", encoding="utf-8").write(prep_text)
open(out + ".main.el", "w", encoding="utf-8").write(
    cookie + "(nelisp-application-image-verify)\n" + main_body)

pin_src = prep_text
boot_sha = None
if bootstrap:
    bpath = os.path.join(os.path.dirname(drv), "runtime-bootstrap-on.el")
    btext = open(bpath, encoding="utf-8").read()
    open(work + "/runtime-bootstrap-on.el", "w", encoding="utf-8").write(btext)
    boot_sha = hashlib.sha256(btext.encode()).hexdigest()
    pin_src = btext
hits = [m.end() for m in re.finditer(r"\(pins '", pin_src)]
assert len(hits) == 1, "expected exactly one (pins '...) binding, got %d" % len(hits)
a, b = forms(pin_src[hits[0]:], True)[0]
pins = pin_src[hits[0] + a: hits[0] + b]
assert pins.startswith("((:") and pins.count("(:") >= 5, "pins list looks wrong"

rows = [row]
for m in re.finditer(r"\(member row '\(((?:\s*\"[^\"]*\")+)\s*\)\)", prep_text):
    cand = re.findall(r'"([^"]*)"', m.group(1))
    if row in cand: rows = cand
def q(s): return '"' + s.replace("\\", "\\\\").replace('"', '\\"') + '"'
meta = "(list :driver-sha256 %s :prep-sha256 %s :row %s :rows '(%s) :pins '%s)" % (
    q(hashlib.sha256(text.encode()).hexdigest()),
    q(hashlib.sha256(prep_text.encode()).hexdigest()), q(row),
    " ".join(q(r) for r in rows), pins)
warm = '''(unless (and (fboundp 'nelisp--fx-active-p) (nelisp--fx-active-p))
  (error "Application image: fx VM inactive, nothing would be pre-compiled"))
(unless (equal (getenv "NELISP_FX_DUMP") "all")
  (error "Application image: NELISP_FX_DUMP=all missing in the producing process"))
(nelisp--fx-warm-for-dump)
(garbage-collect)
(let ((checked 0))
  (dolist (name '(bench-input bench-check-input bench-validate))
    ;; A nested-row driver defines these helpers in its main form, not in the
    ;; preparation; a helper absent here has nothing to pre-compile.
    (when (fboundp name)
      (let* ((fn (symbol-function name))
             (body (and (eq (car-safe fn) 'closure) (nthcdr 3 fn)))
             (rec (and body (gethash body nelisp--fx-table))))
        (unless body (error "Application image: helper %S is not an interpreted closure" name))
        (unless (and rec (not (eq rec 'none)))
          (error "Application image: helper %S not compiled before the dump (record %S)" name rec))
        (when (gethash body nelisp--fx-pending)
          (error "Application image: helper %S still pending" name))
        (setq checked (1+ checked)))))
  (princ (format "WARM-CHECKED %d\n" checked)))
(unless (= 0 (hash-table-count nelisp--fx-pending))
  (error "Application image: %d functions still pending before the dump"
         (hash-table-count nelisp--fx-pending)))
'''
wrapper = (';;; -*- lexical-binding: t; -*-\n(load %s nil t)\n(load %s nil t)\n%s'
           '(princ (format "DUMPED %%d\\n" (nelisp-application-image-record-and-dump %s %s)))\n(kill-emacs 0)\n'
           % (q(lib), q(work + "/prep.el"), warm, q(out), meta))
open(work + "/wrapper.el", "w", encoding="utf-8").write(wrapper)
open(work + "/info", "w").write("rows %s\nbootstrap-sha256 %s\nprep-sha256 %s\n" % (
    " ".join(rows), boot_sha or "none", hashlib.sha256(prep_text.encode()).hexdigest()))
PY

ENVV=(HOME="$HOME" PATH="$PATH" BENCH_ROW="$ROW" BENCH_ENGINE=nelisp NELISP_FX_DUMP=all
      NELISP_PRODUCER_IMAGE_SHA256="$BSHA" TMPDIR="$WORK" XDG_CACHE_HOME="$WORK/cache")
[ -n "${GNU_EMACS_BIN:-}" ] && ENVV+=(GNU_EMACS_BIN="$GNU_EMACS_BIN")
[ -n "${V8_FIXTURE_MODULE:-}" ] && ENVV+=(V8_FIXTURE_MODULE="$V8_FIXTURE_MODULE")
mkdir -p "$WORK/cache"
(ulimit -v 8388608; env -i "${ENVV[@]}" timeout "${PRODUCE_TIMEOUT:-600}" "$BIN" --load "$WORK/wrapper.el") \
  >"$WORK/stdout" 2>"$WORK/stderr"
RC=$?
if [ $RC -ne 0 ]; then head -n 4 "$WORK/stderr" >&2; tail -n 3 "$WORK/stdout" >&2; fail "binary exited $RC"; fi
grep -q '^DUMPED [1-9][0-9]*$' "$WORK/stdout" || fail "no DUMPED line"
[ -s "$OUT" ] && [ ! -e "$OUT.tmp" ] || fail "image missing or .tmp left"
DUMPED=$(sed -n 's/^DUMPED //p' "$WORK/stdout")
[ "$DUMPED" = "$(stat -c %s "$OUT")" ] || fail "dumped byte count differs from file size"
{
  echo "format nelisp-app-image-v1"
  echo "image-sha256 $(sha256sum "$OUT" | cut -d' ' -f1)"
  echo "binary-sha256 $BSHA"
  echo "driver-sha256 $(sha256sum "$DRV" | cut -d' ' -f1)"
  echo "row $ROW"
  echo "main-sha256 $(sha256sum "$OUT.main.el" | cut -d' ' -f1)"
  sed -n 's/^rows /rows /p; s/^prep-sha256 /prep-sha256 /p; s/^bootstrap-sha256 /bootstrap-sha256 /p' "$WORK/info"
} >"$OUT.manifest.tmp" || fail "manifest write"
mv "$OUT.manifest.tmp" "$OUT.manifest" || fail "manifest rename"
rm -rf "$WORK"
echo "produce: ok $OUT ($DUMPED bytes)"
