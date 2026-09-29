#!/bin/sh
# Ledger S4.6 (Doc 207): non-local exits through native->native->VM and
# VM->native->VM chains with non-tail genuine GNU native frames.
#
# 1. Host GNU Emacs 31.1 byte-compiles the VM frames
#    (test/nelisp-eln-nonlocal-chain-vm.el) once; both runtimes run exactly
#    that byte code.
# 2. The driver runs on host GNU Emacs 31.1, which loads the genuine
#    gnu-chain.eln and gnu-increment.eln natively: the reference transcript.
# 3. The driver runs on the NeLisp binary, which admits the same artifacts
#    through ordinary `load' (trust model C, unchanged artifacts) and also
#    checks owner/root/binding baselines and callback suppression.
# 4. The two `S46 ' transcripts must be identical, both drivers must pass,
#    NeLisp stderr must be empty, and the artifacts must be unchanged.
set -eu

script_dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
repo=${NELISP_ROOT:-$(CDPATH= cd -- "$script_dir/.." && pwd)}
binary=${NELISP_BIN:-$repo/target/nelisp}
host_emacs=${NELISP_S46_HOST_EMACS:-emacs}
wrapper_emacs=${EMACS_BIN:-emacs}
cache_root=${NELISP_ELN_CACHE_ROOT:-$HOME/.cache}
chain_eln=${NELISP_S46_CHAIN_ELN:-$cache_root/tmp/s46-chain-artifact/overlay/eln/31.1-ba35c031/gnu-chain.eln}
increment_eln=${NELISP_S46_INCREMENT_ELN:-$cache_root/tmp/eln-gnu-arithmetic-probe/overlay/eln/31.1-ba35c031/gnu-increment-77ce8652-cfc975a9.eln}
chain_sha=f06b6249a26dea523833148e41c091c4142ea23a311345865f6104692bfb8c17
increment_sha=d6d4fd243f9317be052e745ad1eae46afc2c629c2e671da2bd3a944867102bdd
driver=$script_dir/nelisp-eln-nonlocal-chain-driver.el
vm_source=$script_dir/nelisp-eln-nonlocal-chain-vm.el
out_dir=$(mktemp -d "${TMPDIR:-/tmp}/nelisp-eln-s46.XXXXXX")
keep=${NELISP_ELN_KEEP_ARTIFACTS:-0}
trap 'status=$?; if [ "$status" -eq 0 ] && [ "$keep" != 1 ]; then rm -rf "$out_dir"; else echo "ARTIFACT_DIR=$out_dir" >&2; fi' EXIT HUP INT TERM

if [ ! -x "$binary" ]; then
    echo "NELISP_BIN is not executable: $binary" >&2
    exit 2
fi
. "$script_dir/lib/nelisp-boot-args.sh"
nl_cold_image_setup "$binary" || exit 1
for input in "$chain_eln" "$increment_eln" "$driver" "$vm_source"; do
    if [ ! -r "$input" ]; then
        echo "S4.6 input is not readable: $input" >&2
        exit 2
    fi
done
if [ "$(sha256sum "$chain_eln" | cut -d ' ' -f 1)" != "$chain_sha" ] || \
   [ "$(sha256sum "$increment_eln" | cut -d ' ' -f 1)" != "$increment_sha" ]; then
    echo "S4.6 GNU artifacts do not match their pinned hashes" >&2
    exit 2
fi
case "$("$host_emacs" --version 2>/dev/null | head -n 1)" in
    "GNU Emacs 31.1"*) ;;
    *) echo "NELISP_S46_HOST_EMACS must be GNU Emacs 31.1 (ABI ba35c031)" >&2; exit 2 ;;
esac

export NELISP_ROOT=$repo
export NELISP_ELN_SYSTEM_LOADER_SOURCE_ROOT=$repo
export NELISP_ELN_SYSTEM_LOADER_FFI_ROOT=${NELISP_ELN_SYSTEM_LOADER_FFI_ROOT:-$repo}
export NELISP_S46_CHAIN_ELN=$chain_eln
export NELISP_S46_INCREMENT_ELN=$increment_eln
export NELISP_S46_VM_BYTECODE=$out_dir/vm-bytecode.el
cd "$repo"

# 1. VM frames: GNU 31.1 byte code, printed readably for both readers.
"$host_emacs" --batch -Q --eval "
(progn (require 'bytecomp) (setq byte-compile-warnings nil lexical-binding t))" --eval "
(let ((print-escape-newlines t) (print-escape-nonascii t)
      (print-escape-control-characters t) (print-length nil) (print-level nil)
      (forms nil))
  (with-temp-buffer
    (insert-file-contents \"$vm_source\")
    (goto-char (point-min))
    (condition-case nil
        (while t (push (read (current-buffer)) forms))
      (end-of-file nil)))
  (with-temp-file \"$out_dir/vm-bytecode.el\"
    (insert \";;; -*- lexical-binding: t; -*-\n\")
    (dolist (form (nreverse forms))
      (pcase form
        (\`(defvar ,name ,value) (prin1 (list 'defvar name value) (current-buffer)))
        (\`(defun ,name . ,_)
         (progn
           (let ((compiled (byte-compile (list 'lambda (nth 2 form)
                                               (cons 'progn (nthcdr 4 form))))))
             (unless (byte-code-function-p compiled)
               (error \"not compiled: %S\" name))
             (prin1 (list 'defalias (list 'quote name) compiled)
                    (current-buffer))))))
      (insert \"\n\"))))" >"$out_dir/compile.stdout" 2>"$out_dir/compile.stderr" || {
    cat "$out_dir/compile.stderr" >&2; exit 1; }

# 2. GNU reference transcript.
"$host_emacs" --batch -Q --load "$driver" >"$out_dir/gnu.stdout" 2>"$out_dir/gnu.stderr" || {
    cat "$out_dir/gnu.stdout"; cat "$out_dir/gnu.stderr" >&2; exit 1; }
grep -Fx 'S46-DRIVER-PASS gnu' "$out_dir/gnu.stdout" >/dev/null || {
    cat "$out_dir/gnu.stdout"; echo "S4.6 GNU reference run failed" >&2; exit 1; }

# 3. NeLisp run through the ordinary load path.
wrapper=$out_dir/load-wrapper.el
"$wrapper_emacs" --batch -Q -L scripts -L lisp --eval \
    '(progn (defvar nelisp-standalone--repo-root (file-name-as-directory default-directory)) (dolist (name (list "nelisp-standalone--core-bytecode-src" "nelisp-standalone--after-load-runtime-src")) (with-temp-buffer (insert-file-contents "scripts/nelisp-standalone-build.el") (goto-char (point-min)) (unless (search-forward (concat "(defun " name) nil t) (error "source generator not found: %s" name)) (goto-char (match-beginning 0)) (eval (read (current-buffer))))) (princ (nelisp-standalone--after-load-runtime-src)))' \
    >"$wrapper" 2>"$out_dir/wrapper.stderr" || {
    cat "$out_dir/wrapper.stderr" >&2; exit 1; }
lflags=
for d in packages/*/src; do lflags="$lflags -L $d"; done
# shellcheck disable=SC2086
"$binary" ${NL_COLD_IMAGE_PATH:+--cold-load-from "$NL_COLD_IMAGE_PATH"} -L "$repo/lisp" -L "$repo/src" $lflags \
    --load "$repo/lisp/nelisp-eln-native-subr.el" \
    --load "$repo/lisp/nelisp-eln-registration.el" \
    --load "$wrapper" --load "$driver" \
    >"$out_dir/nelisp.stdout" 2>"$out_dir/nelisp.stderr" || {
    cat "$out_dir/nelisp.stdout"; cat "$out_dir/nelisp.stderr" >&2; exit 1; }
if [ -s "$out_dir/nelisp.stderr" ] || \
   ! grep -Fx 'S46-DRIVER-PASS nelisp' "$out_dir/nelisp.stdout" >/dev/null; then
    cat "$out_dir/nelisp.stdout"; cat "$out_dir/nelisp.stderr" >&2
    echo "S4.6 NeLisp run failed" >&2
    exit 1
fi

# 4. Identical transcripts, and a non-trivial matrix.
grep '^S46 ' "$out_dir/gnu.stdout" >"$out_dir/gnu.transcript"
grep '^S46 ' "$out_dir/nelisp.stdout" >"$out_dir/nelisp.transcript"
if ! cmp -s "$out_dir/gnu.transcript" "$out_dir/nelisp.transcript"; then
    diff "$out_dir/gnu.transcript" "$out_dir/nelisp.transcript" >&2 || true
    echo "S4.6 NeLisp transcript differs from GNU Emacs 31.1" >&2
    exit 1
fi
lines=$(wc -l <"$out_dir/nelisp.transcript")
if [ "$lines" -ne 19 ]; then
    echo "S4.6 transcript has $lines scenarios, expected 19" >&2
    exit 1
fi
if [ "$(sha256sum "$chain_eln" | cut -d ' ' -f 1)" != "$chain_sha" ] || \
   [ "$(sha256sum "$increment_eln" | cut -d ' ' -f 1)" != "$increment_sha" ]; then
    echo "S4.6 run changed a GNU artifact" >&2
    exit 1
fi
printf 'NELISP-ELN-S46-NONLOCAL-CHAIN-PASS %s\n' "$lines"
