#!/bin/sh
# Compile one genuine GNU .eln and read its inert metadata via the public loader.
set -eu

SCRIPT_DIR=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
ROOT=${NELISP_ROOT:-$(CDPATH= cd -- "$SCRIPT_DIR/.." && pwd)}
SOURCE_ROOT=${NELISP_ELN_METADATA_SOURCE_ROOT:-$ROOT}
LOADER_ROOT=${NELISP_ELN_METADATA_LOADER_SOURCE_ROOT:-$ROOT}
EMACS_BIN=${EMACS_BIN:-emacs}
NELISP_BIN=${NELISP_BIN:-"$ROOT/target/nelisp"}
CACHE_ROOT=${XDG_CACHE_HOME:-$HOME/.cache}
mkdir -p "$CACHE_ROOT"
OUTDIR=$(mktemp -d "$CACHE_ROOT/nelisp-eln-metadata.XXXXXX")
export OUTDIR
cleanup() {
  status=$?
  if test "$status" -eq 0; then
    rm -rf "$OUTDIR"
  else
    echo "ELN metadata smoke failed; diagnostics preserved in $OUTDIR" >&2
  fi
}
trap cleanup EXIT

cat > "$OUTDIR/metadata-fixture.el" <<'EL'
;; -*- lexical-binding: t; -*-
(defun nelisp-eln-metadata-smoke-doc ()
  "Metadata documentation with Unicode: 雪 café."
  nil)
(defun nelisp-eln-metadata-smoke-uninterned-relocations ()
  [#:eln-reader-x #:eln-reader-x #1=#:eln-reader-y #1#])
EL
cat > "$OUTDIR/host-driver.el" <<'EL'
(require 'comp)
(let* ((directory (getenv "OUTDIR"))
       (source (expand-file-name "metadata-fixture.el" directory))
       (output (expand-file-name "metadata-fixture.eln" directory)))
  (unless (stringp (native-compile source output))
    (error "native-compile did not produce a .eln"))
  (load output nil t)
  (unless (native-comp-function-p
           (symbol-function 'nelisp-eln-metadata-smoke-uninterned-relocations))
    (error "fixture function did not load as native code"))
  (let* ((relocations (nelisp-eln-metadata-smoke-uninterned-relocations)))
    (unless (and (vectorp relocations) (= (length relocations) 4)
                 (symbolp (aref relocations 0))
                 (symbolp (aref relocations 1))
                 (symbolp (aref relocations 2))
                 (symbolp (aref relocations 3))
                 (equal (symbol-name (aref relocations 0)) "eln-reader-x")
                 (equal (symbol-name (aref relocations 1)) "eln-reader-x")
                 (not (eq (aref relocations 0) (aref relocations 1)))
                 (equal (symbol-name (aref relocations 2)) "eln-reader-y")
                 (equal (symbol-name (aref relocations 3)) "eln-reader-y")
                 (eq (aref relocations 2) (aref relocations 3))
                 (null (intern-soft "eln-reader-x"))
                 (null (intern-soft "eln-reader-y")))
      (error "GNU native fixture lost uninterned-symbol identity")))
  (princ (format "HOST_ABI_HASH=%s\n" comp-abi-hash)))
EL

"$EMACS_BIN" -Q --batch -l "$OUTDIR/host-driver.el" \
  >"$OUTDIR/host.stdout" 2>"$OUTDIR/host.stderr"
grep -F 'HOST_ABI_HASH=ba35c031' "$OUTDIR/host.stdout" >/dev/null || {
  cat "$OUTDIR/host.stdout" "$OUTDIR/host.stderr" >&2
  echo "GNU compiler did not produce the pinned ABI hash" >&2
  exit 1
}
[ -s "$OUTDIR/metadata-fixture.eln" ] || {
  echo "GNU compiler produced no .eln" >&2
  exit 1
}

if (cd "$ROOT" && NELISP_ROOT="$ROOT" \
      NELISP_ELN_METADATA_SOURCE_ROOT="$SOURCE_ROOT" \
      NELISP_ELN_METADATA_LOADER_SOURCE_ROOT="$LOADER_ROOT" \
      NELISP_ELN_METADATA_ELN="$OUTDIR/metadata-fixture.eln" \
      "$NELISP_BIN" -L "$SOURCE_ROOT/lisp" \
        -L "$SOURCE_ROOT/packages/nl-ffi/src" \
        --load "$SCRIPT_DIR/nelisp-eln-metadata-smoke.el") \
      >"$OUTDIR/nelisp.stdout" 2>"$OUTDIR/nelisp.stderr"
then
  :
else
  status=$?
  cat "$OUTDIR/nelisp.stdout" "$OUTDIR/nelisp.stderr" >&2
  exit "$status"
fi
expected=$(printf '%s\n%s\n%s' \
  'NELISP-ELN-METADATA-PASS' \
  '"NELISP-ELN-METADATA-PASS' \
  '"')
if test "$(cat "$OUTDIR/nelisp.stdout")" != "$expected" \
   || test -s "$OUTDIR/nelisp.stderr"; then
  cat "$OUTDIR/nelisp.stdout" "$OUTDIR/nelisp.stderr" >&2
  exit 1
fi
cat "$OUTDIR/nelisp.stdout"
