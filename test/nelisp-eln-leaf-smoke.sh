#!/bin/sh
# Build one genuine GNU Emacs .eln, then check four scalar leaves in Host and
# standalone through nl-ffi. This does not call top_level_run or claim full
# .eln runtime compatibility.
set -eu

SCRIPT_DIR=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
ROOT=${NELISP_ROOT:-$(CDPATH= cd -- "$SCRIPT_DIR/.." && pwd)}
EMACS_BIN=${EMACS_BIN:-emacs}
NELISP_BIN=${NELISP_BIN:-"$ROOT/target/nelisp"}
CACHE_ROOT=${XDG_CACHE_HOME:-$HOME/.cache}
mkdir -p "$CACHE_ROOT"
OUTDIR=$(mktemp -d "$CACHE_ROOT/nelisp-eln-leaf.XXXXXX")
export OUTDIR

case "$EMACS_BIN" in
  */*) [ -x "$EMACS_BIN" ] || { echo "EMACS_BIN is not executable: $EMACS_BIN" >&2; exit 2; } ;;
  *) command -v "$EMACS_BIN" >/dev/null 2>&1 || { echo "EMACS_BIN was not found: $EMACS_BIN" >&2; exit 2; } ;;
esac
if [ ! -x "$NELISP_BIN" ]; then
  echo "ELN leaf smoke requires executable EMACS_BIN and NELISP_BIN" >&2
  exit 2
fi

cat > "$OUTDIR/eln-leaf.el" <<'EL'
;; -*- lexical-binding: t; -*-
(defun nelisp-eln-leaf-smoke-nil () nil)
(defun nelisp-eln-leaf-smoke-zero () 0)
(defun nelisp-eln-leaf-smoke-minus-one () -1)
(defun nelisp-eln-leaf-smoke-seventeen () 17)
EL

  cat > "$OUTDIR/host-driver.el" <<'EL'
;; -*- lexical-binding: t; -*-
(require 'comp)
(let* ((directory (getenv "OUTDIR"))
       (source (expand-file-name "eln-leaf.el" directory))
       (output (expand-file-name "eln-leaf.eln" directory)))
  (princ (format "HOST_EMACS_VERSION=%s\n" emacs-version))
  (princ (format "HOST_NATIVE_VERSION_DIR=%s\n" comp-native-version-dir))
  (princ (format "HOST_ABI_HASH=%s\n" comp-abi-hash))
  (unless (stringp (native-compile source output))
    (error "native-compile did not return an artifact path"))
  ;; Load the exact output that will be handed to NeLisp, then require that
  ;; the installed function is native-compiled and returns the expected value.
  (load output nil nil t)
  (unless (and
           (native-comp-function-p (symbol-function 'nelisp-eln-leaf-smoke-nil))
           (native-comp-function-p (symbol-function 'nelisp-eln-leaf-smoke-zero))
           (native-comp-function-p (symbol-function 'nelisp-eln-leaf-smoke-minus-one))
           (native-comp-function-p (symbol-function 'nelisp-eln-leaf-smoke-seventeen))
           (null (nelisp-eln-leaf-smoke-nil))
           (= (nelisp-eln-leaf-smoke-zero) 0)
           (= (nelisp-eln-leaf-smoke-minus-one) -1)
           (= (nelisp-eln-leaf-smoke-seventeen) 17))
    (error "Host did not execute all generated native scalar functions"))
  (princ "HOST_ELN_NATIVE_EXECUTION=nil,0,-1,17\n"))
EL

"$EMACS_BIN" --batch -Q -l "$OUTDIR/host-driver.el" > "$OUTDIR/host.log" 2>&1
cat "$OUTDIR/host.log"
HOST_ABI_HASH=$(sed -n 's/^HOST_ABI_HASH=//p' "$OUTDIR/host.log")
[ -n "$HOST_ABI_HASH" ] || { echo "Host ABI hash was not reported" >&2; exit 1; }
grep -Fx 'HOST_ELN_NATIVE_EXECUTION=nil,0,-1,17' "$OUTDIR/host.log" >/dev/null || {
  echo "Host did not confirm native execution" >&2
  exit 1
}
[ -s "$OUTDIR/eln-leaf.eln" ] || { echo "Host did not produce .eln" >&2; exit 1; }
HOST_ELN_SHA256=$(sha256sum "$OUTDIR/eln-leaf.eln" | awk '{print $1}')

for suffix in nil zero minus_one seventeen; do
  var="ELN_SYMBOL_$(printf '%s' "$suffix" | tr '[:lower:]' '[:upper:]')"
  symbol=$(nm -D -S --defined-only "$OUTDIR/eln-leaf.eln" |
    awk -v suffix="_nelisp_eln_leaf_smoke_${suffix}_0" '$3 == "T" && $4 ~ suffix "$" { print $4 }')
  [ -n "$symbol" ] || { echo "Exported scalar leaf not found: $suffix" >&2; exit 1; }
  [ "$(printf '%s\n' "$symbol" | wc -l)" -eq 1 ] || {
    echo "Expected exactly one exported scalar leaf: $suffix" >&2
    exit 1
  }
  eval "$var=\$symbol"
  export "$var"
done
export HOST_ABI_HASH

cat > "$OUTDIR/standalone-driver.el" <<'EL'
(require 'nl-ffi)
(require 'nelisp-eln-abi)

(defun nelisp-eln-leaf-smoke--check (name symbol expected)
  (eval (list 'ffi:defun 'nelisp-eln-leaf-smoke--call symbol (vector :uint64)))
  (let ((raw (nelisp-eln-leaf-smoke--call)))
    (unless (nelisp-eln-leaf-smoke--valid-raw-p raw expected)
      (error "Scalar leaf %s returned an invalid raw word: %S" name raw))
    raw))

(defun nelisp-eln-leaf-smoke--valid-raw-p (raw expected)
  (let ((kind (nelisp-eln-abi-classify-word raw)))
    (and (eq kind (if (null expected) 'nil 'fixnum))
         (equal (nelisp-eln-abi-decode-immediate raw) expected))))

(let* ((artifact (getenv "OUTDIR"))
       (path (expand-file-name "eln-leaf.eln" artifact))
       (hash (getenv "HOST_ABI_HASH"))
       (metadata (list :producer-version "31.1" :producer-abi-hash hash
                       :elf-class 64 :byte-order 'little :machine 'x86_64
                       :word-bits 64 :gctypebits 3 :use-lsb-tag t)))
  (unless (nelisp-eln-abi-producer-profile-matches-p metadata)
    (error "Host .eln producer profile does not match measured GNU profile"))
  ;; Use the public API end-to-end. On this static reader, ffi:library falls
  ;; back to the existing pure-Elisp loader when dlopen is unavailable.
  (ffi:library path)
   (let* ((handle (nl-ffi-library-handle path))
          (hash-address (nl-ffi-loader-symbol handle "freloc_hash_blob"))
          (hash-length (and (> hash-address 0) (ptr-read-u64 hash-address 0)))
          (serialized (and hash-length (make-string hash-length 0))))
    (unless (and (nl-ffi-loader-handle-p handle)
                 (= hash-length (+ (length hash) 3)))
      (error "Missing/inconsistent .eln ABI hash blob"))
    (dotimes (i hash-length)
      (aset serialized i (ptr-read-u8 hash-address (+ 8 i))))
    (unless (and (= (aref serialized 0) 34)
                 (= (aref serialized (- hash-length 2)) 34)
                 (= (aref serialized (1- hash-length)) 0)
                 (string= (substring serialized 1 (- hash-length 2)) hash))
      (error "GNU .eln ABI hash does not match Host Emacs"))
    ;; Each public FFI call uses a zero-argument unsigned raw result. The ABI
    ;; codec rejects pointer results before any attempt to decode them.
    (let ((raw-nil (nelisp-eln-leaf-smoke--check "nil" (getenv "ELN_SYMBOL_NIL") nil))
          (raw-zero (nelisp-eln-leaf-smoke--check "zero" (getenv "ELN_SYMBOL_ZERO") 0))
          (raw-minus-one (nelisp-eln-leaf-smoke--check "minus-one" (getenv "ELN_SYMBOL_MINUS_ONE") -1))
          (raw-seventeen (nelisp-eln-leaf-smoke--check "seventeen" (getenv "ELN_SYMBOL_SEVENTEEN") 17)))
      ;; Negative verifier probes: a mismatched expectation and a non-fixnum
      ;; raw tag must both reject; neither probes nor rewrites the ELF.
      (when (nelisp-eln-leaf-smoke--valid-raw-p raw-seventeen 18)
        (error "Wrong return expectation was accepted"))
      (when (nelisp-eln-leaf-smoke--valid-raw-p 0 17)
        (error "Wrong GNU return tag was accepted"))
      (eval (list 'ffi:defun 'nelisp-eln-leaf-smoke--missing
                  "nelisp_eln_smoke_symbol_that_does_not_exist"
                  (vector :uint64)))
      (unless (condition-case nil
                  (progn (nelisp-eln-leaf-smoke--missing) nil)
                (nl-ffi-unresolved-symbol t))
        (error "Missing symbol did not fail before invocation"))
      (unless (condition-case nil
                  (progn (nelisp-eln-leaf-smoke--call 1) nil)
                (nl-ffi-wrong-arity t))
        (error "Wrong arity was not rejected"))
      (princ (format "ELN_LEAF_PASS runtime=%s abi=%s raw=nil:%s,zero:%s,minus-one:%s,seventeen:%s\n"
                     (if (boundp 'nelisp--cli-version)
                         nelisp--cli-version "unknown")
                     hash raw-nil raw-zero raw-minus-one raw-seventeen)))))
EL

(cd "$ROOT" && NELISP_ROOT="$ROOT" \
  OUTDIR="$OUTDIR" \
  HOST_ABI_HASH="$HOST_ABI_HASH" \
  "$NELISP_BIN" -L "$ROOT/lisp" -L "$ROOT/packages/nl-ffi/src" \
    --load "$OUTDIR/standalone-driver.el") \
  > "$OUTDIR/standalone.stdout" 2> "$OUTDIR/standalone.stderr"
if [ -s "$OUTDIR/standalone.stderr" ]; then
  cat "$OUTDIR/standalone.stderr" >&2
  echo "Standalone stderr must be empty" >&2
  exit 1
fi
grep -F "ELN_LEAF_PASS " "$OUTDIR/standalone.stdout"
POST_ELN_SHA256=$(sha256sum "$OUTDIR/eln-leaf.eln" | awk '{print $1}')
[ "$POST_ELN_SHA256" = "$HOST_ELN_SHA256" ] || {
  echo ".eln bytes changed after standalone execution" >&2
  exit 1
}
echo "HOST_ELN_SHA256=$HOST_ELN_SHA256"

echo "EMACS_BIN=$EMACS_BIN"
"$EMACS_BIN" --version | head -1
echo "NELISP_BIN=$NELISP_BIN"
file "$NELISP_BIN"
sha256sum "$NELISP_BIN" "$ROOT/packages/nl-ffi/src/nl-ffi.el" \
  "$ROOT/packages/nl-ffi/src/nl-ffi-loader.el" \
  "$OUTDIR/eln-leaf.el" "$OUTDIR/eln-leaf.eln"
echo "ARTIFACT_DIR=$OUTDIR"
