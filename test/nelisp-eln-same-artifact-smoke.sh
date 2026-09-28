#!/bin/sh
set -eu

script_dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
repo=${NELISP_ROOT:-$(CDPATH= cd -- "$script_dir/.." && pwd)}
binary=${NELISP_BIN:-$repo/target/nelisp}
emacs_bin=${EMACS_BIN:-emacs}
ffi_root=${NELISP_ELN_SYSTEM_LOADER_FFI_ROOT:-$repo}
shared_lisp=${NELISP_SHARED_LISP:-$repo/lisp}
out_dir=$(mktemp -d "${TMPDIR:-/tmp}/nelisp-eln-same-artifact.XXXXXX")
eln=$out_dir/same-artifact.eln
identity_eln=$out_dir/same-artifact-identity.eln
provided_load_wrapper=${NELISP_ELN_LOAD_WRAPPER:-}
normal_load_wrapper=${provided_load_wrapper:-$out_dir/generated-wrapper.el}
keep_artifacts=${NELISP_ELN_KEEP_ARTIFACTS:-0}
function_name=nelisp-eln-same-artifact-fixture
function_hex=$(printf '%s' "$function_name" | od -An -tx1 | tr -d ' \n')
function_c_name=F${function_hex}_nelisp_eln_same_artifact_fixture_0
identity_function_name=nelisp-eln-same-artifact-identity
identity_function_hex=$(printf '%s' "$identity_function_name" | od -An -tx1 | tr -d ' \n')
identity_function_c_name=F${identity_function_hex}_nelisp_eln_same_artifact_identity_0
export NELISP_ROOT=$repo
export NELISP_ELN_OUT=$eln
export NELISP_ELN_ARG1_OUT=$identity_eln
export NELISP_ELN_SYSTEM_LOADER_SOURCE_ROOT=$repo
export NELISP_ELN_SYSTEM_LOADER_FFI_ROOT=$ffi_root
export NELISP_ELN_SYSTEM_LOADER_ELN=$eln
export NELISP_ELN_ARG1_ELN=$identity_eln
cd "$repo"
trap 'status=$?; if [ "$status" -eq 0 ] && [ "$keep_artifacts" != 1 ]; then rm -rf "$out_dir"; else echo "ARTIFACT_DIR=$out_dir" >&2; fi' EXIT HUP INT TERM

if [ ! -x "$binary" ]; then
    echo "NELISP_BIN is not executable: $binary" >&2
    exit 2
fi
if [ -z "$provided_load_wrapper" ]; then
    # The generator also bakes the adopted core-module byte-code
    # (nelisp-standalone--core-bytecode-src), so that defun and the repo
    # root it reads are evaluated too; -L scripts resolves the transform.
    if ! "$emacs_bin" --batch -Q -L scripts -L lisp --eval \
        '(progn (defvar nelisp-standalone--repo-root (file-name-as-directory default-directory)) (dolist (name (list "nelisp-standalone--core-bytecode-src" "nelisp-standalone--after-load-runtime-src")) (with-temp-buffer (insert-file-contents "scripts/nelisp-standalone-build.el") (goto-char (point-min)) (unless (search-forward (concat "(defun " name) nil t) (error "source generator not found: %s" name)) (goto-char (match-beginning 0)) (eval (read (current-buffer))))) (princ (nelisp-standalone--after-load-runtime-src)))' \
        >"$normal_load_wrapper" 2>"$out_dir/wrapper.stderr"; then
        cat "$out_dir/wrapper.stderr" >&2
        exit 1
    fi
fi
export NELISP_ELN_LOAD_WRAPPER=$normal_load_wrapper
if [ ! -r "$normal_load_wrapper" ]; then
    echo "ELN load wrapper is not readable: $normal_load_wrapper" >&2
    exit 2
fi
if [ "${NELISP_ELN_GNU_INCREMENT:-0}" = 1 ]; then
    gnu_input=${NELISP_ELN_GNU_INPUT:-}
    gnu_op=${NELISP_ELN_GNU_OP:-increment}
    case "$gnu_op" in
        increment)
            gnu_label=INCREMENT; gnu_results='18,2305843009213693952,1.5'
            gnu_nelisp_pattern='NELISP_INCREMENT_RAW_ENTRIES=5_HELPER_CALLBACKS=4_OVERFLOW_GC=PASS_ERROR_ARITY=PASS_CAPTURED_IMPORT=PASS'
            ;;
        decrement)
            gnu_label=DECREMENT; gnu_results='17,-2305843009213693953,0.5'
            gnu_nelisp_pattern='NELISP_DECREMENT_RAW_ENTRIES=5_HELPER_CALLBACKS=4_UNDERFLOW_GC=PASS_ERROR_ARITY=PASS_CAPTURED_IMPORT=PASS'
            ;;
        zerop)
            # zerop's only indirect call is a non-tail MANY-convention CALL
            # (stage S6), not a tail-JMP arithmetic fast path, so there is no
            # fixnum bound/overflow concept and no builtin-rebinding phase to
            # exercise; the driver skips that phase for this op.  All five
            # results, including the three `t' returns, round-trip through
            # the object codec (RESULT_ENCODE=5_5).
            gnu_label=ZEROP; gnu_results='t,t,t,nil'
            gnu_nelisp_pattern='NELISP_ZEROP_ADMISSION=PASS_DESCRIPTOR=PASS_LOGIC_MATCH=5_5_ARGC_GUARD=PASS_RESULT_ENCODE=5_5_REBINDING=skipped'
            ;;
        *) echo "NELISP_ELN_GNU_OP must be increment, decrement, or zerop" >&2; exit 2 ;;
    esac
    increment_lisp=${NELISP_ELN_INCREMENT_LISP:-$repo/lisp}
    increment_driver=$script_dir/nelisp-eln-increment-same-artifact-driver.el
    if [ -z "$gnu_input" ] || [ ! -r "$gnu_input" ]; then
        echo "NELISP_ELN_GNU_INPUT must name a readable GNU-produced artifact" >&2
        exit 2
    fi
    if [ ! -r "$increment_lisp/nelisp-eln-registration.el" ] || \
       [ ! -r "$increment_lisp/nelisp-eln-native-subr.el" ] || \
       [ ! -r "$increment_driver" ]; then
        echo "GNU increment smoke source files are incomplete" >&2
        exit 2
    fi
    export NELISP_ELN_SYSTEM_LOADER_ELN=$gnu_input
    export NELISP_ELN_GNU_INPUT=$gnu_input
    export NELISP_ELN_GNU_OP=$gnu_op
    gnu_before=$(sha256sum "$gnu_input" | cut -d ' ' -f 1)
    {
        printf 'NELISP_ELN_GNU_INPUT=%s\n' "$gnu_input"
        printf 'GNU_ARTIFACT_SHA256=%s\n' "$gnu_before"
        printf 'GNU_OPERATION=%s\n' "$gnu_op"
        printf 'INCREMENT_LISP=%s\n' "$increment_lisp"
        sha256sum "$increment_lisp/nelisp-eln-native-subr.el" \
            "$increment_lisp/nelisp-eln-registration.el" "$increment_driver"
    } >"$out_dir/increment-source-manifest.txt"
    export NELISP_ELN_INCREMENT_PHASE=host
    if ! "$emacs_bin" --batch -Q --load "$increment_driver" \
        >"$out_dir/increment-host.stdout" 2>"$out_dir/increment-host.stderr"; then
        cat "$out_dir/increment-host.stdout"
        cat "$out_dir/increment-host.stderr" >&2
        exit 1
    fi
    if [ -s "$out_dir/increment-host.stderr" ] || \
       ! grep -Fx "GNU_${gnu_label}_HOST_RESULTS=${gnu_results}_ERROR_ARITY=PASS" \
          "$out_dir/increment-host.stdout" >/dev/null; then
        cat "$out_dir/increment-host.stdout"
        cat "$out_dir/increment-host.stderr" >&2
        echo "GNU $gnu_op host check did not complete cleanly" >&2
        exit 1
    fi
    export NELISP_ELN_INCREMENT_PHASE=nelisp
    if ! "$binary" -L "$increment_lisp" -L "$repo/src" \
        -L "$repo/packages/nl-ffi/src" -L "$repo/lisp" \
        --load "$increment_lisp/nelisp-eln-native-subr.el" \
        --load "$increment_lisp/nelisp-eln-registration.el" \
        --load "$normal_load_wrapper" --load "$increment_driver" \
        >"$out_dir/increment-nelisp.stdout" 2>"$out_dir/increment-nelisp.stderr"; then
        cat "$out_dir/increment-nelisp.stdout"
        cat "$out_dir/increment-nelisp.stderr" >&2
        exit 1
    fi
    if [ -s "$out_dir/increment-nelisp.stderr" ] || \
       ! grep -Fx "$gnu_nelisp_pattern" \
          "$out_dir/increment-nelisp.stdout" >/dev/null; then
        cat "$out_dir/increment-nelisp.stdout"
        cat "$out_dir/increment-nelisp.stderr" >&2
        echo "NeLisp $gnu_op same-artifact check did not complete cleanly" >&2
        exit 1
    fi
    gnu_after=$(sha256sum "$gnu_input" | cut -d ' ' -f 1)
    if [ "$gnu_before" != "$gnu_after" ]; then
        echo "Host or NeLisp changed the GNU arithmetic artifact" >&2
        exit 1
    fi
    printf 'NELISP-ELN-GNU-%s-PASS %s\n' "$gnu_label" "$gnu_before"
    exit 0
fi
if [ "${NELISP_ELN_GNU_IDENTITY_ONLY:-0}" = 1 ]; then
    gnu_input=${NELISP_ELN_GNU_INPUT:-}
    gnu_function=${NELISP_ELN_GNU_FUNCTION:-nelisp-gnu-identity}
    if [ -z "$gnu_input" ] || [ ! -r "$gnu_input" ]; then
        echo "NELISP_ELN_GNU_INPUT must name a readable GNU-produced artifact" >&2
        exit 2
    fi
    export NELISP_ELN_SYSTEM_LOADER_ELN=$gnu_input
    export NELISP_ELN_GNU_FUNCTION=$gnu_function
    gnu_before=$(sha256sum "$gnu_input" | cut -d ' ' -f 1)
    cat >"$out_dir/gnu-identity-host.el" <<'EL'
;;; -*- lexical-binding: t; -*-
(require 'comp)
(unless (and (equal emacs-version "31.1")
             (equal comp-abi-hash "ba35c031"))
  (error "Host ABI does not match pinned GNU Emacs 31.1 profile"))
(load (getenv "NELISP_ELN_SYSTEM_LOADER_ELN") nil t t)
(let ((fn (symbol-function (intern (getenv "NELISP_ELN_GNU_FUNCTION"))))
      (values (list 17 (concat "GNU " "identity")
                    (list 1 (cons 2 3)))))
  (unless (and (subrp fn) (native-comp-function-p fn)
               (equal (subr-arity fn) '(1 . 1)))
    (error "GNU artifact did not register a native fixed-arity identity"))
  (dolist (value values)
    (garbage-collect)
    (let ((result (funcall fn value)))
      (garbage-collect)
      (unless (eq value result)
        (error "GNU identity lost object identity for %S" value)))))
(princ "GNU_ELN_IDENTITY_EQ_GC_ARITY1=3\n")
EL
    if ! "$emacs_bin" --batch -Q -l "$out_dir/gnu-identity-host.el" \
        >"$out_dir/gnu-host.stdout" 2>"$out_dir/gnu-host.stderr"; then
        cat "$out_dir/gnu-host.stdout"
        cat "$out_dir/gnu-host.stderr" >&2
        exit 1
    fi
    if [ -s "$out_dir/gnu-host.stderr" ] || \
       ! grep -Fx 'GNU_ELN_IDENTITY_EQ_GC_ARITY1=3' "$out_dir/gnu-host.stdout" >/dev/null; then
        cat "$out_dir/gnu-host.stdout"
        cat "$out_dir/gnu-host.stderr" >&2
        echo "GNU host identity check did not complete cleanly" >&2
        exit 1
    fi
    cat >"$out_dir/gnu-metadata-probe.el" <<'EL'
(defun nelisp-gnu-metadata-assert ()
  (let* ((name (intern (getenv "NELISP_ELN_GNU_FUNCTION")))
         (owner (cl-find name nelisp-eln-registration--owners
                         :key (lambda (item) (aref item 3)) :test #'eq))
         (token (and owner (aref owner 16)))
         (expected (vector '(function (t) t) nil t 'consp 'listp
                           'symbol-with-pos-p))
         (data (and token (nelisp-eln-registration-metadata-data token)))
         (docs (and token (nelisp-eln-registration-metadata-docs token)))
         (type-word (and token
                         (nelisp-eln-registration-metadata-type-word token))))
    (unless (and token (= (length data) 6) (equal data expected) docs
                 (eq (nelisp-eln-registration-metadata-decode token type-word)
                     (aref data 0))
                 (eq type-word (nelisp-eln-registration-metadata-slot-word
                                token 'data 0)))
      (error "GNU metadata capability assertions failed"))
    (let* ((unit (aref owner 1))
           (handle (aref unit 1))
           (unit-address (aref unit 4))
           (address (nelisp-eln-registration--writable-object handle "d_reloc" 48))
           (i 0))
      (unless (and (= (nelisp-eln-abi-read-word unit-address 48)
                      (nelisp-eln-registration-metadata-data-word token))
                   (= (nelisp-eln-abi-read-word unit-address 40)
                      (nelisp-eln-registration-metadata-docs-word token)))
        (error "GNU metadata unit vector words mismatch"))
      (while (< i 6)
        (unless (= (nelisp-eln-abi-read-word address (* i 8))
                   (nelisp-eln-registration-metadata-slot-word token 'data i))
          (error "GNU metadata relocation mismatch at index %d" i))
        (setq i (1+ i))))
    (princ "NELISP_GNU_METADATA_VECTOR_TYPE_RELOCS_PASS\n")))
EL
    if ! "$binary" -L "$repo/lisp" -L "$shared_lisp" \
        -L "$ffi_root/packages/nl-ffi/src" \
        --load "$shared_lisp/nelisp-eln-emitter.el" \
        --load "$shared_lisp/nelisp-eln-objects.el" \
        --load "$shared_lisp/nelisp-eln-native-subr.el" \
        --load "$shared_lisp/nelisp-eln-registration-objects.el" \
        --load "$shared_lisp/nelisp-eln-registration-vectors.el" \
        --load "$normal_load_wrapper" \
        --load "$out_dir/gnu-metadata-probe.el" \
        --eval '(load (getenv "NELISP_ELN_SYSTEM_LOADER_ELN") nil t t)' \
        --eval '(nelisp-gnu-metadata-assert)' \
        --eval '(let* ((fn (symbol-function (intern (getenv "NELISP_ELN_GNU_FUNCTION")))) (base (symbol-function (quote nelisp-eln-raw-call-word))) (raw-count 0) (values (list 17 (concat "GNU " "identity") (list 1 (cons 2 3))))) (unless (and (subrp fn) (equal (func-arity fn) (cons 1 1))) (error "NeLisp GNU identity is not a unary subr: %S" (func-arity fn))) (unwind-protect (progn (fset (quote nelisp-eln-raw-call-word) (lambda (&rest args) (prog1 (apply base args) (setq raw-count (1+ raw-count))))) (dolist (value values) (garbage-collect) (let ((result (funcall fn value))) (garbage-collect) (unless (eq value result) (error "NeLisp GNU identity lost object identity for %S" value)))) (let ((before raw-count)) (unless (condition-case nil (progn (funcall fn) nil) (wrong-number-of-arguments t)) (error "identity accepted zero arguments")) (unless (condition-case nil (progn (funcall fn 1 2) nil) (wrong-number-of-arguments t)) (error "identity accepted two arguments")) (unless (= before raw-count) (error "wrong arity reached raw-call"))) (unless (>= raw-count 3) (error "expected at least 3 successful raw calls, got %d" raw-count)) (princ (format "NELISP_GNU_ELN_IDENTITY_EQ_GC_ARITY1_RAWCALLS=%d\n" raw-count))) (fset (quote nelisp-eln-raw-call-word) base)))' \
        >"$out_dir/gnu-nelisp.stdout" 2>"$out_dir/gnu-nelisp.stderr"; then
        cat "$out_dir/gnu-nelisp.stdout"
        cat "$out_dir/gnu-nelisp.stderr" >&2
        exit 1
    fi
    if [ -s "$out_dir/gnu-nelisp.stderr" ] || \
       ! grep -Eq '^NELISP_GNU_ELN_IDENTITY_EQ_GC_ARITY1_RAWCALLS=([3-9]|[1-9][0-9]+)$' \
          "$out_dir/gnu-nelisp.stdout"; then
        cat "$out_dir/gnu-nelisp.stdout"
        cat "$out_dir/gnu-nelisp.stderr" >&2
        echo "NeLisp GNU identity check did not complete cleanly" >&2
        exit 1
    fi
    gnu_after=$(sha256sum "$gnu_input" | cut -d ' ' -f 1)
    if [ "$gnu_before" != "$gnu_after" ]; then
        echo "Host or NeLisp changed the GNU input artifact" >&2
        exit 1
    fi
    printf 'NELISP-ELN-GNU-IDENTITY-SMOKE-PASS %s\n' "$gnu_before"
    exit 0
fi
if [ "${NELISP_ELN_BRANCH_ONLY:-0}" = 1 ]; then
    branch_eln=$out_dir/same-artifact-branch.eln
    export NELISP_ELN_SYSTEM_LOADER_ELN=$branch_eln
    if ! "$binary" -L "$shared_lisp" \
        --eval '(progn (require (quote nelisp-eln-emitter)) (let ((ir (nelisp-aot-compiler--parse-stmt (quote (defun nelisp-eln-same-artifact-branch (value) (if value (if value 17 19) 23))) nil nil nil))) (nelisp-eln-emitter-write-ir ir (getenv "NELISP_ELN_SYSTEM_LOADER_ELN"))) (princ "NELISP-ELN-BRANCH-EMIT-PASS\n"))' \
        >"$out_dir/branch-emit.stdout" 2>"$out_dir/branch-emit.stderr"; then
        cat "$out_dir/branch-emit.stdout"
        cat "$out_dir/branch-emit.stderr" >&2
        exit 1
    fi
    if [ -s "$out_dir/branch-emit.stderr" ] || \
       ! grep -Fx 'NELISP-ELN-BRANCH-EMIT-PASS' "$out_dir/branch-emit.stdout" >/dev/null; then
        cat "$out_dir/branch-emit.stdout"
        cat "$out_dir/branch-emit.stderr" >&2
        exit 1
    fi
    branch_hash=$(sha256sum "$branch_eln" | cut -d ' ' -f 1)
    cat >"$out_dir/branch-host.el" <<'EL'
;;; -*- lexical-binding: t; -*-
(require 'comp)
(unless (and (equal emacs-version "31.1")
             (equal comp-abi-hash "ba35c031"))
  (error "Host ABI does not match pinned GNU Emacs 31.1 profile"))
(load (getenv "NELISP_ELN_SYSTEM_LOADER_ELN") nil t t)
(let ((fn (symbol-function 'nelisp-eln-same-artifact-branch)))
  (unless (and (subrp fn) (native-comp-function-p fn)
               (equal (subr-arity fn) '(1 . 1))
               (= (funcall fn nil) 23)
               (= (funcall fn 0) 17)
               (= (funcall fn -1) 17))
    (error "GNU nested branch truth results were incorrect")))
(princ "GNU_NESTED_BRANCH_NIL23_ZERO17_NEGATIVE17=1\n")
EL
    if ! "$emacs_bin" --batch -Q -l "$out_dir/branch-host.el" \
        >"$out_dir/branch-host.stdout" 2>"$out_dir/branch-host.stderr"; then
        cat "$out_dir/branch-host.stdout"
        cat "$out_dir/branch-host.stderr" >&2
        exit 1
    fi
    if [ -s "$out_dir/branch-host.stderr" ] || \
       ! grep -Fx 'GNU_NESTED_BRANCH_NIL23_ZERO17_NEGATIVE17=1' \
          "$out_dir/branch-host.stdout" >/dev/null; then
        cat "$out_dir/branch-host.stdout"
        cat "$out_dir/branch-host.stderr" >&2
        exit 1
    fi
    if ! "$binary" -L "$repo/lisp" -L "$shared_lisp" \
        -L "$ffi_root/packages/nl-ffi/src" \
        --load "$shared_lisp/nelisp-eln-emitter.el" \
        --load "$shared_lisp/nelisp-eln-objects.el" \
        --load "$shared_lisp/nelisp-eln-native-subr.el" \
        --load "$shared_lisp/nelisp-eln-registration-objects.el" \
        --load "$shared_lisp/nelisp-eln-registration-vectors.el" \
        --load "$normal_load_wrapper" \
        --eval '(load (getenv "NELISP_ELN_SYSTEM_LOADER_ELN"))' \
        --eval '(let* ((fn (symbol-function (quote nelisp-eln-same-artifact-branch))) (base (symbol-function (quote nelisp-eln-raw-call-word))) (count 0)) (unwind-protect (progn (fset (quote nelisp-eln-raw-call-word) (lambda (&rest args) (setq count (1+ count)) (apply base args))) (unless (and (subrp fn) (functionp fn) (equal (func-arity fn) (cons 1 1)) (= (funcall fn nil) 23) (= (funcall fn 0) 17) (= (funcall fn -1) 17)) (error "NeLisp nested branch truth results were incorrect")) (unless (= count 3) (error "nested branch raw-call count %d, expected 3" count)) (princ "NELISP_NESTED_BRANCH_NIL23_ZERO17_NEGATIVE17_RAWCALLS3=1\n")) (fset (quote nelisp-eln-raw-call-word) base)))' \
        >"$out_dir/branch-nelisp.stdout" 2>"$out_dir/branch-nelisp.stderr"; then
        cat "$out_dir/branch-nelisp.stdout"
        cat "$out_dir/branch-nelisp.stderr" >&2
        exit 1
    fi
    if [ -s "$out_dir/branch-nelisp.stderr" ] || \
       ! grep -Fx 'NELISP_NESTED_BRANCH_NIL23_ZERO17_NEGATIVE17_RAWCALLS3=1' \
          "$out_dir/branch-nelisp.stdout" >/dev/null; then
        cat "$out_dir/branch-nelisp.stdout"
        cat "$out_dir/branch-nelisp.stderr" >&2
        exit 1
    fi
    branch_after=$(sha256sum "$branch_eln" | cut -d ' ' -f 1)
    if [ "$branch_hash" != "$branch_after" ]; then
        echo "Host or NeLisp changed the branch artifact" >&2
        exit 1
    fi
    printf 'NELISP-ELN-BRANCH-SMOKE-PASS %s\n' "$branch_hash"
    exit 0
fi
cat >"$out_dir/cleanup-failure-driver.el" <<'EL'
;;; -*- lexical-binding: t; -*-
(require 'nelisp-eln-registration)
;; `nelisp-eln-registration' no longer requires `nelisp-native-load'
;; eagerly (S7.7.4 corpus-gate laziness): it is loaded on first genuine
;; use inside the registration/activation path instead.  This driver
;; must force a REAL load itself, before installing its own `fset' mock
;; of `nelisp-native-load--pin-end' below: otherwise the fixture's own
;; first genuine admission attempt (which needs the real
;; `nelisp-native-load--symbol-addr') would trigger that lazy `require'
;; partway through the protected body, re-defining the whole file from
;; source and silently clobbering this mock with the real function.
(require 'nelisp-native-load)
(setq nelisp-eln-registration-debug t)
(let* ((base-call6 (symbol-function 'nelisp-eln-registration--call6))
       (base-pin-end (symbol-function 'nelisp-native-load--pin-end))
       (context-call-count 0) (pin-count 0)
       (first-error nil) (second-error nil))
  (unwind-protect
      (progn
        (fset 'nelisp-eln-registration--call6
              (lambda (address &rest args)
                (setq context-call-count (1+ context-call-count))
                (apply base-call6 address args)))
        (fset 'nelisp-native-load--pin-end
              (lambda (&rest _args)
                (setq pin-count (1+ pin-count))
                (error "injected pin-end failure")))
        (condition-case err
            (load (getenv "NELISP_ELN_SYSTEM_LOADER_ELN"))
          (error (setq first-error err)))
        (unless (and first-error (= context-call-count 2) (= pin-count 1)
                     (= (length nelisp-eln-registration--pending-cleanups) 1)
                     (= (length nelisp-eln-registration--owners) 1)
                     (fboundp 'nelisp-eln-same-artifact-fixture))
          (error "cleanup failure did not retain one pending owner: %S"
                 (list first-error context-call-count pin-count
                       nelisp-eln-registration--pending-cleanups)))
        (condition-case err
            (nelisp-eln-registration-load
             (getenv "NELISP_ELN_SYSTEM_LOADER_ELN"))
          (error (setq second-error err)))
        (unless (and second-error (= context-call-count 2) (= pin-count 1))
          (error "pending attempt was retried: %S" second-error))
        (princ "NELISP_ELN_CLEANUP_FAILURE_FAILCLOSED=1\n"))
    (fset 'nelisp-eln-registration--call6 base-call6)
    (fset 'nelisp-native-load--pin-end base-pin-end)))
EL
if ! command -v "$emacs_bin" >/dev/null 2>&1; then
    echo "EMACS_BIN is not executable: $emacs_bin" >&2
    exit 2
fi

if ! "$binary" -L "$repo/lisp" \
    --eval '(progn (require (quote nelisp-eln-emitter)) (let ((ir (nelisp-aot-compiler--parse-stmt (quote (defun nelisp-eln-same-artifact-fixture () 17)) nil nil nil))) (nelisp-eln-emitter-write-ir ir (getenv "NELISP_ELN_OUT"))) (princ "NELISP-ELN-SAME-ARTIFACT-EMIT-PASS\n"))' \
    >"$out_dir/emit.stdout" 2>"$out_dir/emit.stderr"; then
    cat "$out_dir/emit.stdout"
    cat "$out_dir/emit.stderr" >&2
    exit 1
fi
if [ -s "$out_dir/emit.stderr" ] || \
   ! grep -Fx 'NELISP-ELN-SAME-ARTIFACT-EMIT-PASS' "$out_dir/emit.stdout" >/dev/null; then
    cat "$out_dir/emit.stdout"
    cat "$out_dir/emit.stderr" >&2
    echo "NeLisp did not emit the fixture cleanly" >&2
    exit 1
fi
before=$(sha256sum "$eln" | cut -d ' ' -f 1)
if ! "$binary" -L "$repo/lisp" \
    --eval '(progn (require (quote nelisp-eln-emitter)) (let ((ir (nelisp-aot-compiler--parse-stmt (quote (defun nelisp-eln-same-artifact-identity (value) value)) nil nil nil))) (nelisp-eln-emitter-write-ir ir (getenv "NELISP_ELN_ARG1_OUT"))) (princ "NELISP-ELN-IDENTITY-EMIT-PASS\n"))' \
    >"$out_dir/identity-emit.stdout" 2>"$out_dir/identity-emit.stderr"; then
    cat "$out_dir/identity-emit.stdout"
    cat "$out_dir/identity-emit.stderr" >&2
    exit 1
fi
if [ -s "$out_dir/identity-emit.stderr" ] || \
   ! grep -Fx 'NELISP-ELN-IDENTITY-EMIT-PASS' \
      "$out_dir/identity-emit.stdout" >/dev/null; then
    cat "$out_dir/identity-emit.stdout"
    cat "$out_dir/identity-emit.stderr" >&2
    echo "NeLisp did not emit the identity fixture cleanly" >&2
    exit 1
fi
identity_before=$(sha256sum "$identity_eln" | cut -d ' ' -f 1)

cat >"$out_dir/host-driver.el" <<'EL'
;;; -*- lexical-binding: t; -*-
(require 'comp)
(unless (and (equal emacs-version "31.1")
             (equal comp-abi-hash "ba35c031"))
  (error "Host ABI does not match pinned GNU Emacs 31.1 profile"))
(load (getenv "NELISP_ELN_SYSTEM_LOADER_ELN") nil t t)
(load (getenv "NELISP_ELN_ARG1_ELN") nil t t)
(let ((fn (symbol-function 'nelisp-eln-same-artifact-fixture)))
  (unless (and (subrp fn) (native-comp-function-p fn)
               (equal (subr-arity fn) '(0 . 0))
               (= (funcall fn) 17))
    (error "GNU did not register and execute the emitted native function")))
(let* ((fn (symbol-function 'nelisp-eln-same-artifact-identity))
       (string-value (concat "雪" "狐"))
       (cons-value (list 'nested (cons 2 3)))
       (values (list -17 0 nil string-value cons-value)))
  (unless (and (subrp fn) (native-comp-function-p fn)
               (equal (subr-arity fn) '(1 . 1)))
    (error "GNU did not register the identity subr with arity 1"))
  (garbage-collect)
  (dolist (value values)
    (unless (eq value (funcall fn value))
      (error "GNU identity did not preserve object identity for %S" value))))
(princ "GNU_NATIVE_REGISTRATION_AND_CALL=17\nGNU_NATIVE_IDENTITY_ARITY1=5\n")
EL
if ! "$emacs_bin" --batch -Q -l "$out_dir/host-driver.el" \
    >"$out_dir/host.stdout" 2>"$out_dir/host.stderr"; then
    cat "$out_dir/host.stdout"
    cat "$out_dir/host.stderr" >&2
    exit 1
fi
if [ -s "$out_dir/host.stderr" ] || \
   ! grep -Fx 'GNU_NATIVE_REGISTRATION_AND_CALL=17' "$out_dir/host.stdout" >/dev/null || \
   ! grep -Fx 'GNU_NATIVE_IDENTITY_ARITY1=5' "$out_dir/host.stdout" >/dev/null; then
    cat "$out_dir/host.stdout"
    cat "$out_dir/host.stderr" >&2
    echo "GNU did not complete the same-artifact check cleanly" >&2
    exit 1
fi
after=$(sha256sum "$eln" | cut -d ' ' -f 1)
if [ "$before" != "$after" ]; then
    echo "GNU changed the emitted artifact" >&2
    exit 1
fi

cat >"$out_dir/nelisp-driver.el" <<'EL'
;;; -*- lexical-binding: t; -*-
(require 'nelisp-eln-metadata)
(require 'nl-ffi)
(require 'nl-ffi-memory)
(require 'nelisp-eln-system-loader)
(require 'nelisp-eln-native-subr)
(let* ((path (getenv "NELISP_ELN_SYSTEM_LOADER_ELN"))
       (name (getenv "NELISP_ELN_SAME_ARTIFACT_C_NAME"))
       (handle (nelisp-eln-system-loader-open path))
       (native (nelisp-eln-native-subr-create handle name)))
  (unless (and (subrp native) (functionp native)
               (= (funcall native) 17)
               (= (funcall native) 17)
               (= (funcall native) 17))
    (error "NeLisp failed direct NativeSubr calls into the same artifact"))
  (princ "NELISP_DIRECT_NATIVE_SUBR_RESULTS=17,17,17\n"))
EL
export NELISP_ELN_SAME_ARTIFACT_C_NAME=$function_c_name
if ! "$binary" -L "$repo/lisp" -L "$shared_lisp" \
    -L "$ffi_root/packages/nl-ffi/src" \
    --load "$normal_load_wrapper" \
    --load "$shared_lisp/nelisp-eln-emitter.el" \
    --load "$shared_lisp/nelisp-eln-objects.el" \
    --load "$shared_lisp/nelisp-eln-native-subr.el" \
    --load "$shared_lisp/nelisp-eln-registration-objects.el" \
    --load "$shared_lisp/nelisp-eln-registration-vectors.el" \
    --load "$out_dir/cleanup-failure-driver.el" \
    >"$out_dir/cleanup.stdout" 2>"$out_dir/cleanup.stderr"; then
    cat "$out_dir/cleanup.stdout"
    cat "$out_dir/cleanup.stderr" >&2
    exit 1
fi
if [ -s "$out_dir/cleanup.stderr" ] || \
   ! grep -Fx 'NELISP_ELN_CLEANUP_FAILURE_FAILCLOSED=1' \
      "$out_dir/cleanup.stdout" >/dev/null; then
    cat "$out_dir/cleanup.stdout"
    cat "$out_dir/cleanup.stderr" >&2
    echo "Injected cleanup failure did not fail closed" >&2
    exit 1
fi

if ! "$binary" -L "$repo/lisp" \
    -L "$ffi_root/packages/nl-ffi/src" \
    --load "$out_dir/nelisp-driver.el" \
    >"$out_dir/nelisp.stdout" 2>"$out_dir/nelisp.stderr"; then
    cat "$out_dir/nelisp.stdout"
    cat "$out_dir/nelisp.stderr" >&2
    exit 1
fi
if [ -s "$out_dir/nelisp.stderr" ] || \
   ! grep -Fx 'NELISP_DIRECT_NATIVE_SUBR_RESULTS=17,17,17' "$out_dir/nelisp.stdout" >/dev/null; then
    cat "$out_dir/nelisp.stdout"
    cat "$out_dir/nelisp.stderr" >&2
    echo "NeLisp did not complete same-artifact direct calls cleanly" >&2
    exit 1
fi
after_ne_lisp=$(sha256sum "$eln" | cut -d ' ' -f 1)
if [ "$before" != "$after_ne_lisp" ]; then
    echo "NeLisp changed the emitted artifact" >&2
    exit 1
fi

if ! OUTDIR="$out_dir" "$binary" -L "$repo/lisp" -L "$shared_lisp" \
    -L "$ffi_root/packages/nl-ffi/src" \
    --load "$normal_load_wrapper" \
    --eval '(princ "REGTRACE source-load nelisp-eln-emitter\n")' \
    --load "$shared_lisp/nelisp-eln-emitter.el" \
    --eval '(princ "REGTRACE source-load nelisp-eln-objects\n")' \
    --load "$shared_lisp/nelisp-eln-objects.el" \
    --eval '(princ "REGTRACE source-load nelisp-eln-native-subr\n")' \
    --load "$shared_lisp/nelisp-eln-native-subr.el" \
    --eval '(princ "REGTRACE source-load nelisp-eln-registration-objects\n")' \
    --load "$shared_lisp/nelisp-eln-registration-objects.el" \
    --eval '(princ "REGTRACE source-load nelisp-eln-registration-vectors\n")' \
    --load "$shared_lisp/nelisp-eln-registration-vectors.el" \
    --eval '(setq nelisp-eln-registration-debug t)' \
    --eval '(princ "REGTRACE scalar0-load-start\n")' \
    --eval '(load (getenv "NELISP_ELN_SYSTEM_LOADER_ELN"))' \
    --eval '(princ "REGTRACE scalar0-load-done\n")' \
    --eval '(princ "REGTRACE identity-load-start\n")' \
    --eval '(load (getenv "NELISP_ELN_ARG1_ELN"))' \
    --eval '(princ "REGTRACE identity-load-done\n")' \
    --eval '(setq nelisp-eln-identity-test-values (list -17 0 nil (concat "雪" "狐") (list 17 (cons 2 3))))' \
    --eval '(garbage-collect)' \
    --eval '(progn (princ "REGTRACE scalar0-first-apply\n") (let* ((fn (symbol-function (quote nelisp-eln-same-artifact-fixture))) (value (funcall fn))) (unless (and (subrp fn) (functionp fn) (= value 17)) (error "post-load native function returned %S" value)) (princ "NELISP_ELN_POST_RETURN_CALL=17\n")))' \
    --eval '(let ((fn (symbol-function (quote nelisp-eln-same-artifact-identity)))) (unless (and (subrp fn) (functionp fn) (equal (func-arity fn) (cons 1 1))) (error "post-load identity subr has wrong arity: %S" (func-arity fn))) (dolist (value nelisp-eln-identity-test-values) (unless (eq value (funcall fn value)) (error "identity lost NeLisp object identity: %S" value))) (princ "NELISP_ELN_POST_RETURN_IDENTITY_ARITY1=5\n"))' \
    --eval '(let* ((fn (symbol-function (quote nelisp-eln-same-artifact-identity))) (base (symbol-function (quote nelisp-eln-raw-call-word))) (raw-count 0)) (unwind-protect (progn (fset (quote nelisp-eln-raw-call-word) (lambda (&rest args) (setq raw-count (1+ raw-count)) (apply base args))) (dolist (args (list nil (list 1 2))) (unless (condition-case nil (progn (apply fn args) nil) (wrong-number-of-arguments t)) (error "identity subr accepted wrong arity: %S" args))) (unless (= raw-count 0) (error "wrong arity reached raw-call bridge %d times" raw-count)) (princ "NELISP_ELN_IDENTITY_ARITY_ERRORS=2_RAWCALLS=0\n")) (fset (quote nelisp-eln-raw-call-word) base)))' \
    --eval '(let* ((fn (symbol-function (quote nelisp-eln-same-artifact-identity))) (base (symbol-function (quote nelisp-eln-raw-call-word))) (raw-count 0) (units (length nelisp-eln-objects--live-units)) (arenas (length nelisp-eln-objects--arenas)) (cleanups (length nelisp-eln-objects--pending-cleanups)) (records (length nelisp-eln-objects--identity-records))) (unwind-protect (progn (fset (quote nelisp-eln-raw-call-word) (lambda (&rest args) (setq raw-count (1+ raw-count)) (apply base args))) (unless (condition-case err (progn (funcall fn (list (quote nested) (cons 2 3))) nil) (nelisp-eln-objects-unsupported (and (equal (car (cdr err)) (quote unsupported-symbol-state)) t))) (error "interned symbol identity input was not rejected by the documented codec boundary")) (unless (= raw-count 0) (error "unsupported symbol reached raw-call bridge %d times" raw-count)) (unless (and (= units (length nelisp-eln-objects--live-units)) (= arenas (length nelisp-eln-objects--arenas)) (= cleanups (length nelisp-eln-objects--pending-cleanups)) (= records (length nelisp-eln-objects--identity-records))) (error "unsupported symbol failure leaked identity state")) (princ "NELISP_ELN_INTERNED_SYMBOL_UNSUPPORTED=1_RAWCALLS=0_CLEANUP=1\n")) (fset (quote nelisp-eln-raw-call-word) base)))' \
    --eval '(let* ((fn (symbol-function (quote nelisp-eln-same-artifact-identity))) (base (symbol-function (quote nelisp-eln-raw-call-word))) (encode-base (symbol-function (quote nelisp-eln-objects-encode))) (raw-count 0) (gc-count 0)) (unwind-protect (progn (fset (quote nelisp-eln-raw-call-word) (lambda (&rest args) (setq raw-count (1+ raw-count)) (apply base args))) (fset (quote nelisp-eln-objects-encode) (lambda (unit value) (let ((word (funcall encode-base unit value))) (when (and (< gc-count 2) (or (stringp value) (consp value))) (setq gc-count (1+ gc-count)) (garbage-collect)) word))) (dolist (value nelisp-eln-identity-test-values) (unless (eq value (funcall fn value)) (error "identity changed under raw-call instrumentation: %S" value))) (unless (= raw-count 5) (error "successful identity calls reached raw-call bridge %d times, expected 5" raw-count)) (unless (= gc-count 2) (error "expected two encode-time collections, got %d" gc-count)) (princ "NELISP_ELN_IDENTITY_SUCCESSFUL_RAWCALLS=5_GC=2\n")) (fset (quote nelisp-eln-raw-call-word) base) (fset (quote nelisp-eln-objects-encode) encode-base)))' \
    >"$out_dir/registration.stdout" 2>"$out_dir/registration.stderr"; then
    cat "$out_dir/registration.stdout"
    cat "$out_dir/registration.stderr" >&2
    exit 1
fi
if [ -s "$out_dir/registration.stderr" ] || \
   ! grep -Fx 'NELISP_ELN_POST_RETURN_CALL=17' \
      "$out_dir/registration.stdout" >/dev/null || \
   ! grep -Fx 'NELISP_ELN_POST_RETURN_IDENTITY_ARITY1=5' \
      "$out_dir/registration.stdout" >/dev/null || \
   ! grep -Fx 'NELISP_ELN_IDENTITY_ARITY_ERRORS=2_RAWCALLS=0' \
      "$out_dir/registration.stdout" >/dev/null || \
   ! grep -Fx 'NELISP_ELN_INTERNED_SYMBOL_UNSUPPORTED=1_RAWCALLS=0_CLEANUP=1' \
      "$out_dir/registration.stdout" >/dev/null || \
   ! grep -Fx 'NELISP_ELN_IDENTITY_SUCCESSFUL_RAWCALLS=5_GC=2' \
      "$out_dir/registration.stdout" >/dev/null; then
    cat "$out_dir/registration.stdout"
    cat "$out_dir/registration.stderr" >&2
    echo "NeLisp did not complete same-artifact registration cleanly" >&2
    exit 1
fi
after_registration=$(sha256sum "$eln" | cut -d ' ' -f 1)
if [ "$before" != "$after_registration" ]; then
    echo "NeLisp registration changed the emitted artifact" >&2
    exit 1
fi
identity_after_registration=$(sha256sum "$identity_eln" | cut -d ' ' -f 1)
if [ "$identity_before" != "$identity_after_registration" ]; then
    echo "NeLisp registration changed the identity artifact" >&2
    exit 1
fi
printf 'NELISP-ELN-SAME-ARTIFACT-PASS %s %s %s\n' "$before" "$function_c_name" "$identity_function_c_name"
