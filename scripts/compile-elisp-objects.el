;;; compile-elisp-objects.el --- Doc 99 §99.B build orchestrator  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 99 §99.B build-time orchestrator.  Invoked by
;; `build-tool/build.rs' as
;;
;;   emacs --batch -Q -L lisp -l scripts/compile-elisp-objects.el \
;;         -f compile-elisp-objects-emit-all
;;
;; The function walks a hard-coded manifest of (elisp-source-feature .
;; output-object) pairs, runs `nelisp-phase47-compile-to-object' on the
;; canonical source from each feature, and writes the resulting ET_REL
;; .o files under `target/elisp-objects/'.
;;
;; The manifest is intentionally a defconst (not TOML) — §99.B is a
;; one-entry spike and parsing TOML inside `emacs --batch' would just
;; add a build dep on `toml.el'.  When the manifest grows past ~5
;; entries we can migrate.

;;; Code:

(require 'cl-lib)

;; Hard-coded `lisp/' resolution: this script is run from the repo
;; root by `cargo build', so relative paths resolve from there.
(let* ((this (or load-file-name buffer-file-name))
       (script-dir (and this (file-name-directory this)))
       (repo-root (and script-dir
                       (file-name-as-directory
                        (expand-file-name ".." script-dir))))
       (lisp-dir (and repo-root (expand-file-name "lisp" repo-root))))
  (when (and lisp-dir (file-directory-p lisp-dir))
    (add-to-list 'load-path lisp-dir)))

(require 'nelisp-phase47-compiler)

(defconst compile-elisp-objects-manifest
  '((nelisp-cc-spike-noop
     :source-var nelisp-cc-spike-noop--source
     :output "nelisp_spike_noop.o")
    (nelisp-cc-fact-i64
     :source-var nelisp-cc-fact-i64--source
     :output "nelisp_fact_i64.o")
    ;; Doc 100 §100.C — first real bi_* swap: `(truncate INT)' Int arm.
    (nelisp-cc-truncate-int
     :source-var nelisp-cc-truncate-int--source
     :output "nelisp_truncate_int.o")
    ;; Doc 101 §101.B — `(length CONS)' proper-list walk.
    (nelisp-cc-length-cons
     :source-var nelisp-cc-length-cons--source
     :output "nelisp_length_cons.o")
    ;; Doc 101 §101.C — `(eq SYMBOL SYMBOL)' via Symbol/Str read ops.
    (nelisp-cc-eq-symbol
     :source-var nelisp-cc-eq-symbol--source
     :output "nelisp_eq_symbol.o")
    ;; Doc 101 §101.D — `(cons A B)' via Cons construction ops.
    (nelisp-cc-cons-construct
     :source-var nelisp-cc-cons-construct--source
     :output "nelisp_cons_construct.o")
    ;; Doc 111 §111.B — `(recordp X)' via direct Sexp tag test.
    (nelisp-cc-recordp
     :source-var nelisp-cc-recordp--source
     :output "nelisp_recordp.o"
     :requires-arch x86_64)
    ;; Doc 111 §111.C — `(aref VECTOR IDX)' Vector arm swap.
    (nelisp-cc-aref-vector
     :source-var nelisp-cc-aref-vector--source
     :output "nelisp_aref_vector.o"
     :requires-arch x86_64)
    ;; Doc 100 §100.D — `jit/arith.rs' 12-trampoline swap.  Each entry
    ;; emits one `.o' file exporting one `nelisp_jit_NAME' symbol that
    ;; the `unified_fn_ptr' table in `build-tool/src/jit/bridge.rs'
    ;; resolves at link time.  Linux-x86_64 only — non-linux / non-
    ;; x86_64 build paths keep the legacy Rust trampolines for now.
    (nelisp-cc-jit-arith
     :source-var nelisp-cc-jit-arith-add2--source
     :output "nelisp_jit_add2.o")
    (nelisp-cc-jit-arith
     :source-var nelisp-cc-jit-arith-sub2--source
     :output "nelisp_jit_sub2.o")
    (nelisp-cc-jit-arith
     :source-var nelisp-cc-jit-arith-mul2--source
     :output "nelisp_jit_mul2.o")
    (nelisp-cc-jit-arith
     :source-var nelisp-cc-jit-arith-eq2--source
     :output "nelisp_jit_eq2.o")
    (nelisp-cc-jit-arith
     :source-var nelisp-cc-jit-arith-lt2--source
     :output "nelisp_jit_lt2.o")
    (nelisp-cc-jit-arith
     :source-var nelisp-cc-jit-arith-gt2--source
     :output "nelisp_jit_gt2.o")
    (nelisp-cc-jit-arith
     :source-var nelisp-cc-jit-arith-le2--source
     :output "nelisp_jit_le2.o")
    (nelisp-cc-jit-arith
     :source-var nelisp-cc-jit-arith-ge2--source
     :output "nelisp_jit_ge2.o")
    (nelisp-cc-jit-arith
     :source-var nelisp-cc-jit-arith-logior2--source
     :output "nelisp_jit_logior2.o")
    (nelisp-cc-jit-arith
     :source-var nelisp-cc-jit-arith-logand2--source
     :output "nelisp_jit_logand2.o")
    (nelisp-cc-jit-arith
     :source-var nelisp-cc-jit-arith-logxor2--source
     :output "nelisp_jit_logxor2.o")
    (nelisp-cc-jit-arith
     :source-var nelisp-cc-jit-arith-ash--source
     :output "nelisp_jit_ash.o")
    ;; Doc 110 §110.E.2.a — `jit/float.rs' 4 arithmetic trampoline
    ;; swaps (add / sub / mul / div).  Each entry emits one `.o'
    ;; exporting one `nl_jit_float_*' symbol that the linker
    ;; resolves into the static archive against the Rust extern
    ;; in `build-tool/src/jit/bridge.rs::float_link'.  The 5
    ;; comparison trampolines (eq_eps / lt / gt / le / ge) need
    ;; §110.C compiler integration and ship in §110.E.2.b.
    ;;
    ;; `:requires-arch x86_64' — Stage 2.a only ships the x86_64
    ;; f64 emit path; aarch64 f64 emit ships in §110.D and removes
    ;; the gate.  Until then, building on aarch64 skips these
    ;; entries and the Rust trampolines stay live (gated by an
    ;; inverse `#[cfg]' in `build-tool/src/jit/float.rs').
    (nelisp-cc-jit-float
     :source-var nelisp-cc-jit-float-add--source
     :output "nl_jit_float_add.o"
     :requires-arch (x86_64 aarch64))
    (nelisp-cc-jit-float
     :source-var nelisp-cc-jit-float-sub--source
     :output "nl_jit_float_sub.o"
     :requires-arch (x86_64 aarch64))
    (nelisp-cc-jit-float
     :source-var nelisp-cc-jit-float-mul--source
     :output "nl_jit_float_mul.o"
     :requires-arch (x86_64 aarch64))
    (nelisp-cc-jit-float
     :source-var nelisp-cc-jit-float-div--source
     :output "nl_jit_float_div.o"
     :requires-arch (x86_64 aarch64))
    ;; Doc 110 §110.C.2.a — 4 ordered comparison trampolines.
    ;; eq_eps stays Rust until §110.C.2.b (= rodata + NaN mask).
    (nelisp-cc-jit-float
     :source-var nelisp-cc-jit-float-lt--source
     :output "nl_jit_float_lt.o"
     :requires-arch (x86_64 aarch64))
    (nelisp-cc-jit-float
     :source-var nelisp-cc-jit-float-gt--source
     :output "nl_jit_float_gt.o"
     :requires-arch (x86_64 aarch64))
    (nelisp-cc-jit-float
     :source-var nelisp-cc-jit-float-le--source
     :output "nl_jit_float_le.o"
     :requires-arch (x86_64 aarch64))
    (nelisp-cc-jit-float
     :source-var nelisp-cc-jit-float-ge--source
     :output "nl_jit_float_ge.o"
     :requires-arch (x86_64 aarch64))
    ;; Doc 110 §110.C.2.b — EQ-EPS (= `(a-b).abs() < 1e-15').
    ;; Uses the SUBSD + ANDPD + UCOMISD + SETB/SETNP/AND mask
    ;; sequence emitted by `--emit-f64-eq-eps'.
    (nelisp-cc-jit-float
     :source-var nelisp-cc-jit-float-eq-eps--source
     :output "nl_jit_float_eq_eps.o"
     :requires-arch (x86_64 aarch64))
    ;; Doc 110 §110.F — `jit/math.rs' 3 unary f64 trampoline swaps.
    ;; `float' = identity, `exp' / `log' = libm extern call via the
    ;; new `(f64-call SYM ARG)' grammar form introduced in §110.F.
    (nelisp-cc-jit-math
     :source-var nelisp-cc-jit-math-float--source
     :output "nl_jit_float_float.o"
     :requires-arch (x86_64 aarch64))
    (nelisp-cc-jit-math
     :source-var nelisp-cc-jit-math-exp--source
     :output "nl_jit_float_exp.o"
     :requires-arch (x86_64 aarch64))
    (nelisp-cc-jit-math
     :source-var nelisp-cc-jit-math-log--source
     :output "nl_jit_float_log.o"
     :requires-arch (x86_64 aarch64)))
  "Build-time manifest of elisp features → ET_REL output files.
Each entry is `(FEATURE :source-var SYM :output BASENAME)' where
FEATURE is the feature to `require', SYM is the defconst holding
the canonical source form, and BASENAME is the .o filename written
to OUT-DIR.  When the spike grows past one entry, switch to a TOML
manifest under `build-tool/elisp-objects.toml'.")

(defun compile-elisp-objects--out-dir ()
  "Return the absolute path of `target/elisp-objects/' under the repo root.
Honors the `NELISP_ELISP_OBJECTS_DIR' environment variable when set
(= `build.rs' passes `$OUT_DIR' / elisp-objects so cargo doesn't see
stale artifacts across feature combinations)."
  (or (getenv "NELISP_ELISP_OBJECTS_DIR")
      (let* ((this (or load-file-name buffer-file-name))
             (script-dir (and this (file-name-directory this)))
             (repo-root (and script-dir
                             (file-name-as-directory
                             (expand-file-name ".." script-dir)))))
        (expand-file-name "target/elisp-objects" repo-root))))

(defun compile-elisp-objects--target-arch ()
  "Return the Phase 47 target arch symbol for this batch run.
Defaults to `x86_64' when `NELISP_PHASE47_TARGET_ARCH' is unset.
Signals a clear error on unsupported values."
  (pcase (or (getenv "NELISP_PHASE47_TARGET_ARCH") "x86_64")
    ("x86_64" 'x86_64)
    ("aarch64" 'aarch64)
    (other
     (error
      "compile-elisp-objects: unsupported NELISP_PHASE47_TARGET_ARCH %S (expected x86_64 or aarch64)"
      other))))

(defun compile-elisp-objects--target-format ()
  "Return the Phase 47 output format symbol for this batch run.
Looks at `NELISP_PHASE47_TARGET_OS' (forwarded by `build.rs')
and picks `'mach-o' for macOS, `'elf' otherwise (= linux + the
default).  Doc 100 §100.D Stage 3 added Mach-O support for
macos-aarch64; other OS / format combinations stay on ELF."
  (pcase (or (getenv "NELISP_PHASE47_TARGET_OS") "linux")
    ("macos" 'mach-o)
    (_ 'elf)))

(defun compile-elisp-objects-emit-all ()
  "Compile every manifest entry to its output `.o' file.
Returns the list of absolute paths written.  Used by
`build-tool/build.rs' as the `-f' callback (= no args, exits the
batch process via the outer `emacs --batch' driver).

Each manifest entry may specify a `:requires-arch SYM' keyword
(Doc 110 §110.E.2.a) — when present and the current target arch
does not match SYM, the entry is silently skipped.  This lets
Stage 2.a ship the x86_64 f64 swap entries while aarch64 builds
fall through to the Rust trampolines that remain in
`build-tool/src/jit/float.rs' under an inverse cfg gate."
  (let* ((out-dir (compile-elisp-objects--out-dir))
         (arch (compile-elisp-objects--target-arch))
         (format (compile-elisp-objects--target-format))
         (written nil))
    (unless (file-directory-p out-dir)
      (make-directory out-dir t))
    (dolist (entry compile-elisp-objects-manifest)
      (let* ((feature (car entry))
             (props   (cdr entry))
             (src-var (plist-get props :source-var))
             (output  (plist-get props :output))
             (requires-arch (plist-get props :requires-arch))
             (out-path (expand-file-name output out-dir)))
        (cond
         ((and requires-arch
               (cond ((symbolp requires-arch) (not (eq requires-arch arch)))
                     ((listp requires-arch) (not (memq arch requires-arch)))
                     (t t)))
          (message "[compile-elisp-objects] skipping %s -> %s (= requires %S, building %S)"
                   feature output requires-arch arch))
         (t
          (require feature)
          (unless (boundp src-var)
            (error "compile-elisp-objects: %S has no :source-var %S"
                   feature src-var))
          (let ((sexp (symbol-value src-var)))
            (message "[compile-elisp-objects] %s -> %s"
                     feature out-path)
            (nelisp-phase47-compile-to-object sexp out-path
                                              :arch arch :format format)
            (push out-path written))))))
    (nreverse written)))

(provide 'compile-elisp-objects)

;;; compile-elisp-objects.el ends here
