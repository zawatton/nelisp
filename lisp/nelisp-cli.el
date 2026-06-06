;;; nelisp-cli.el --- Doc 128 / Doc 49 Wave 7 — CLI dispatch (elisp side)  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 128 — pushes the CLI parsing + sub-command dispatch out of the
;; Rust binary `build-tool/src/bin/nelisp.rs' down into elisp so the
;; production binary is a ~50 LOC bootstrap stub (= Env::new_global
;; + lookup `nelisp-cli-main' + apply + map ExitCode).
;;
;; Doc 49 Wave 7 — extends CLI with Emacs-style batch-mode flags so the
;; nelisp binary can drive `scripts/compile-elisp-objects.el' without
;; requiring a host Emacs installation.  New flags (processed left-to-
;; right in order, before -l loads):
;;
;;   --batch              enable batch mode (sets `noninteractive' t)
;;   -Q | --quick         ignore init files (no-op; compatibility)
;;   -L DIR | --directory DIR   prepend DIR to `load-path'
;;   -l FILE | --load FILE      load FILE silently (no final-value print)
;;                              (batch mode: silent; old mode: prints last)
;;   -f FUNC | --funcall FUNC   call FUNC with no arguments
;;   --eval EXPR          evaluate EXPR; used to call setup forms (setenv …)
;;   --setenv VAR=VAL     shorthand for (setenv VAR VAL) — injected before -l
;;
;; The batch flags are detected when argv[1] == "--batch".  All other
;; argv forms fall through to the original single-command dispatcher.
;;
;; Exit code contract (consumed by the Rust stub's
;; `ExitCode::from((code as u8).min(255))'):
;;   0 — success
;;   1 — eval / IO error
;;   2 — usage error (= bad argv)
;;
;; The version string is sourced from the global `nelisp--cli-version'
;; which the Rust stub seeds with `env!("CARGO_PKG_VERSION")' via
;; `Env::set_value' before invoking `nelisp-cli-main'.  If unset (=
;; test path, REPL), `nelisp-cli-main' falls back to "unknown".
;;
;; Wave A25.3 (AOT meta-driver bootstrap).  When the standalone
;; NeLisp binary is invoked with `--setenv NELISP_USE_META_DRIVER=1' (or
;; equivalent OS env when host Emacs is the runtime), the batch dispatch
;; short-circuits the legacy `compile-elisp-objects-emit-range' elisp
;; iteration chain and invokes the AOT-friendly static-manifest
;; walker from `scripts/compile-elisp-objects-meta.el' through the new
;; `nelisp--cli-meta-driver-main' entry point.  Output `.o' files share
;; the production filenames so a `cmp' against the host-Emacs reference
;; is the verification gate.  Rust LOC delta = 0 (= all dispatch logic
;; lives in this elisp file plus a one-line `unless featurep' guard in
;; the meta-driver script).

;;; Code:

(declare-function compile-elisp-artifact "nelisp-artifact" (args))
(declare-function exec-elisp-artifact "nelisp-artifact" (args))
(declare-function eval-elisp-artifact "nelisp-artifact" (args))
(declare-function inspect-elisp-artifact "nelisp-artifact" (args))

(defvar nelisp--cli-version "unknown"
  "Production binary version string.

Seeded by the Rust bootstrap stub in `build-tool/src/bin/nelisp.rs'
(via `Env::set_value') from the `CARGO_PKG_VERSION' compile-time
constant before the elisp `nelisp-cli-main' dispatch runs.")

(defconst nelisp--cli-usage
  "usage: nelisp --version
       nelisp eval EXPR                 # evaluate EXPR and print the result
       nelisp -l FILE                   # load FILE and print the last result
       nelisp exec FILE                 # load FILE silently (no final-value print)
       nelisp compile-elisp-artifact ...  # write a private .nelc + manifest
       nelisp exec-elisp-artifact FILE.nelc FORM...
       nelisp eval-elisp-artifact FILE.nelc FORM...
       nelisp inspect-elisp-artifact FILE.nelc
       nelisp -                         # read from stdin and print the last result
       nelisp --batch [-Q] [-L DIR...] [--setenv VAR=VAL...] [--eval EXPR...] [-l FILE...] [-f FUNC]
                                        # Doc 49 build-host batch mode"
  "USAGE banner — Doc 128 + Doc 49 Wave 7 batch-mode flags.")

(defun nelisp--cli-print-error (msg)
  "Write MSG to stderr with the `nelisp: ' prefix + newline."
  (nelisp--write-stderr-line (concat "nelisp: " msg)))

(defun nelisp--cli-slurp-stdin ()
  "Block-read stdin until EOF, return the accumulated string.

Loops `read-stdin-bytes' with a 64 KiB chunk until it returns nil
(= EOF).  Returns the concatenated UTF-8 string (= what `eval_str_all'
expects)."
  (let ((acc "") (chunk nil) (done nil))
    (while (not done)
      (setq chunk (read-stdin-bytes 65536))
      (cond
       ((null chunk) (setq done t))
       ((and (stringp chunk) (= (length chunk) 0)) (setq done t))
       (t (setq acc (concat acc chunk)))))
    acc))

(defun nelisp--cli-eval-string-print (input)
  "Eval INPUT as a single form, princ-print the result, return exit code.

Mirrors the pre-Doc-128 Rust `run_eval' (= `eval' + `println!').
Returns 0 on success, 1 on eval error."
  (condition-case err
      (let* ((forms (nelisp--read-all-from-string-impl input))
             (n (length forms)))
        (cond
         ((= n 0)
          (nelisp--cli-print-error
           "eval error: empty input — at least one form required")
          1)
         ((> n 1)
          (nelisp--cli-print-error
           (format "eval error: expected exactly one form, got %d" n))
          1)
         (t
          (let ((result (eval (car forms))))
            (nelisp--write-stdout-bytes (prin1-to-string result))
            (nelisp--write-stdout-bytes "\n")
            0))))
    (error
     (nelisp--cli-print-error
      (format "eval error: %s" (error-message-string err)))
     1)))

(defun nelisp--cli-eval-string-all-print (input)
  "Eval every top-level form in INPUT in sequence, print last value.

Mirrors the pre-Doc-128 Rust `run_eval_all' used by `-l FILE' and `-'.
Returns 0 on success, 1 on eval error.  Empty input prints nil."
  (condition-case err
      (let ((forms (nelisp--read-all-from-string-impl input))
            (last nil))
        (let ((cur forms))
          (while cur
            (setq last (eval (car cur)))
            (setq cur (cdr cur))))
        (nelisp--write-stdout-bytes (prin1-to-string last))
        (nelisp--write-stdout-bytes "\n")
        0)
    (error
     (nelisp--cli-print-error
      (format "eval error: %s" (error-message-string err)))
     1)))

(defun nelisp--cli-exec-string-silent (input)
  "Eval every top-level form in INPUT but suppress the final-value print.

Mirrors the pre-Doc-128 Rust `exec_eval_all' used by `exec FILE' for
stdio servers (= elisp-lsp, future REPL-over-stdio) where any byte
after the last protocol reply corrupts the wire.  Errors still flow
to stderr / exit 1."
  (condition-case err
      (let ((cur (nelisp--read-all-from-string-impl input)))
        (while cur
          (eval (car cur))
          (setq cur (cdr cur)))
        0)
    (error
     (nelisp--cli-print-error
      (format "eval error: %s" (error-message-string err)))
     1)))

(defun nelisp--cli-read-file (path)
  "Read PATH as UTF-8 string, return it.  Returns nil + prints error if missing."
  (let ((src (condition-case _err
                 (nelisp--syscall-read-file path)
               (error nil))))
    (cond
     ((null src)
      (nelisp--cli-print-error (format "cannot read %s" path))
      nil)
     (t src))))

(defun nelisp--cli-run-artifact-command (fn args)
  "Load `nelisp-artifact' then call FN with ARGS."
  (condition-case err
      (progn
        (require 'nelisp-artifact)
        (funcall fn args))
    (error
     (nelisp--cli-print-error
      (format "artifact command error: %s" (error-message-string err)))
     1)))

;; ---------------------------------------------------------------------------
;; Doc 49 Wave 7 — batch-mode dispatcher
;; ---------------------------------------------------------------------------

(defun nelisp--cli-batch-load-file (path)
  "Load PATH silently (batch mode: no final-value print).
Returns 0 on success, 1 on load error."
  (condition-case err
      (progn (load path) 0)
    (error
     (nelisp--cli-print-error
      (format "load error in %s: %s" path (error-message-string err)))
     1)))

(defun nelisp--cli-batch-eval-expr (expr)
  "Evaluate the string EXPR (a single or multi-form expression).
Used for --eval EXPR batch args.  Returns 0 on success, 1 on error."
  (condition-case err
      (let ((forms (nelisp--read-all-from-string-impl expr)))
        (let ((cur forms))
          (while cur
            (eval (car cur))
            (setq cur (cdr cur))))
        0)
    (error
     (nelisp--cli-print-error
      (format "eval error: %s" (error-message-string err)))
     1)))

(defun nelisp--cli-batch-funcall (name)
  "Call function named NAME (a string) with no arguments.
Returns 0 on success, 1 on error."
  (condition-case err
      (let ((sym (intern name)))
        (unless (fboundp sym)
          (signal 'error (list (format "Undefined function: %s" name))))
        (funcall sym)
        0)
    (error
     (nelisp--cli-print-error
      (format "funcall error (%s): %s" name (error-message-string err)))
     1)))

(defun nelisp--cli-batch-ensure-host ()
  "Ensure `nelisp-build-host' functions are available.

Called lazily after the first -L dir is added to `load-path'.
Loads `nelisp-build-host' from `load-path' if not already provided.
If the load fails, installs a minimal inline stub that covers the
pre-load phase (add-to-list + load-path + bare getenv/setenv).

This split (inline stub first, proper load after -L) means
`compile-elisp-objects.el' can use the full API."
  (unless (featurep 'nelisp-build-host)
    (condition-case _
        (load "nelisp-build-host" nil t)
      (error nil))
    ;; Minimal inline stub for the pre-load phase when load failed.
    (unless (boundp 'load-path)
      (setq load-path nil))
    (unless (fboundp 'add-to-list)
      (fset 'add-to-list
            (lambda (list-var element &optional append compare-fn)
              (let* ((lst    (symbol-value list-var))
                     (cmp-fn (or compare-fn 'equal))
                     (present (let ((cur lst))
                                (catch 'hit
                                  (while cur
                                    (when (funcall cmp-fn (car cur) element)
                                      (throw 'hit t))
                                    (setq cur (cdr cur)))
                                  nil))))
                (unless present
                  (if append
                      (set list-var (append lst (list element)))
                    (set list-var (cons element lst))))
                (symbol-value list-var)))))
    (unless (boundp 'process-environment)
      (setq process-environment nil))
    (unless (fboundp 'getenv)
      (fset 'getenv
            (lambda (variable)
              (when (boundp 'process-environment)
                (let ((prefix (concat variable "="))
                      (plen   (+ (length variable) 1))
                      (found  nil)
                      (cur    process-environment))
                  (while (and cur (not found))
                    (let ((entry (car cur)))
                      (when (and (stringp entry)
                                 (>= (length entry) plen)
                                 (string-equal (substring entry 0 plen) prefix))
                        (setq found (substring entry plen))))
                    (setq cur (cdr cur)))
                  found)))))
    (unless (fboundp 'setenv)
      (fset 'setenv
            (lambda (variable &optional value _substitute)
              (unless (boundp 'process-environment)
                (setq process-environment nil))
              (let ((prefix (concat variable "="))
                    (plen   (+ (length variable) 1))
                    (new-env nil))
                (let ((cur process-environment))
                  (while cur
                    (let ((entry (car cur)))
                      (unless (and (stringp entry)
                                   (>= (length entry) plen)
                                   (string-equal (substring entry 0 plen) prefix))
                        (setq new-env (cons entry new-env))))
                    (setq cur (cdr cur))))
                (setq new-env (nreverse new-env))
                (when value
                  (setq new-env (cons (concat variable "=" value) new-env)))
                (setq process-environment new-env)
                value)))
      )
    (unless (fboundp 'make-directory)
      (fset 'make-directory
            (lambda (dir &optional parents)
              (nl-make-directory dir (if parents t nil))
              nil)))))

;; ---------------------------------------------------------------------------
;; Wave A25.3 — AOT meta-driver bootstrap (env-var dispatch).
;; ---------------------------------------------------------------------------
;;
;; When the standalone NeLisp binary is invoked with `NELISP_USE_META_DRIVER=1'
;; in its environment, the batch dispatcher short-circuits the legacy elisp
;; iteration chain (= `compile-elisp-objects-emit-range' walking the manifest
;; with `dolist' + `plist-get' + `(require feature)') and instead drives the
;; AOT-friendly static-manifest walker from `scripts/compile-elisp-
;; objects-meta.el'.
;;
;; The host-Emacs and standalone NeLisp paths share the same `.o' emit kernel
;; (`nelisp-aot-compile-to-object'), so a successful standalone-mode run
;; produces byte-identical output to the host-Emacs reference.  Wave A25.2's
;; `nelisp_meta_should_rebuild' kernel is reachable from the meta-driver via
;; the existing `compile-elisp-objects-meta--should-rebuild' host-shim today;
;; once Wave A25.4+ surfaces an `extern-call'-driven host wrapper around the
;; compiled `.o', that shim swaps to a direct extern call without changing
;; the dispatch shape here.
;;
;; Rust LOC delta: 0 (the Rust bootstrap stub in `build-tool/src/bin/nelisp.rs'
;; is unchanged; the new env-var branch lives entirely in elisp).

(defun nelisp--cli-meta-driver-load-deps ()
  "Pre-load the AOT meta-driver dependency chain.

Standalone NeLisp's `require' resolves `(require 'nelisp-aot-compiler)'
through the elisp `load' shim defined in `lisp/nelisp-stdlib-misc.el'.
That shim sets `default-directory' to the loaded file's parent dir, so
nested `(require ...)' calls executed during the load body resolve their
file probes relative to that dir — fine on host Emacs (which has its own
absolute load-path walk) but fragile on the standalone elisp shim because
the inner `locate-library' walks `default-directory' first and only then
the `load-path' list.  We sidestep that by hoisting the dep chain to
top-level requires before the meta-driver's own load runs.

Order matches `nelisp-aot-compiler.el's own require chain (lines 92-96).
Returns t on full success, signals on the first missing feature.

Also installs a `locate-file' polyfill when the standalone runtime is
missing one (= used by `compile-elisp-objects--source-file' to map
manifest feature symbols → `.el' paths via `load-path' walk).  The
polyfill is added through `defalias' only if `locate-file' is unbound
— host Emacs callers already have a real one and stay untouched."
  (require 'cl-lib)
  (require 'nelisp-asm-arm64)
  (require 'nelisp-asm-x86_64)
  (require 'nelisp-elf-write)
  (require 'nelisp-sexp-layout)
  (require 'nelisp-aot-compiler)
  (require 'nelisp-cc-meta-driver)
  (unless (fboundp 'locate-file)
    (defalias 'locate-file
      (lambda (filename path &optional _suffixes _predicate)
        ;; Minimal polyfill: walks PATH for FILENAME, returns first
        ;; absolute match or nil.  Ignores SUFFIXES + PREDICATE (the
        ;; meta-driver only calls `(locate-file BASENAME load-path)'
        ;; with the suffix already baked into BASENAME).
        (let ((cur path)
              (hit nil))
          (while (and cur (null hit))
            (let* ((root (car cur))
                   (candidate (and (stringp root) (> (length root) 0)
                                   (concat (if (eq (aref root (1- (length root))) ?/)
                                               root
                                             (concat root "/"))
                                           filename))))
              (when (and candidate (file-exists-p candidate))
                (setq hit candidate)))
            (setq cur (cdr cur)))
          hit))))
  t)

(defun nelisp--cli-meta-driver-locate-script (basename)
  "Resolve BASENAME (= a `<file>.el' path component) on `load-path'.
Returns an absolute path, or nil if not found.  The standalone NeLisp
`locate-library' walks `default-directory' first, then `load-path' — so
as long as the caller passed `-L scripts/' / `-L /abs/scripts/' before
this runs the lookup succeeds.  Returns nil rather than signaling so the
caller can format a useful error with all probed candidates.

The path is canonicalised to an absolute form via the
`nelisp--syscall-canonicalize' primitive when available, falling back
to `expand-file-name'.  This matters because the standalone elisp
`load' shim re-routes its argument through `locate-library', which
prepends each `load-path' entry (including `default-directory') to
the input — so passing back a relative path like
\"scripts/compile-elisp-objects.el\" causes a doubled-prefix probe
\"lisp/scripts/compile-elisp-objects.el\" that never matches the
on-disk file."
  (let ((hit (locate-library basename)))
    (cond
     ((null hit) nil)
     ;; Already absolute — pass through.
     ((and (stringp hit) (> (length hit) 0) (eq (aref hit 0) ?/))
      hit)
     ;; Prefer the canonicalize syscall (= absolute, symlink-resolved).
     ((fboundp 'nelisp--syscall-canonicalize)
      (or (nelisp--syscall-canonicalize hit) hit))
     ;; Final fallback — leaves a relative path on hosts without the
     ;; canonicalize primitive; `load' may fail but the caller will
     ;; surface a clear error.
     (t (expand-file-name hit)))))

(defun nelisp--cli-meta-driver-main ()
  "Wave A25.3 — AOT meta-driver bootstrap entry point.

Drives the standalone-NeLisp branch that bypasses the legacy elisp
manifest walker in `compile-elisp-objects-emit-all' / `-emit-range' and
instead invokes the AOT-friendly static-manifest walker from
`scripts/compile-elisp-objects-meta.el'.

Steps:
  1. Pre-load the aot-compiler dep chain via
     `nelisp--cli-meta-driver-load-deps' (= works around standalone
     NeLisp's nested-require / default-directory interaction).
  2. Locate `compile-elisp-objects.el' on `load-path' (= the caller must
     have passed `-L scripts/' before driving here) and `load' it.  This
     pre-populates the manifest defconst + the `--out-dir' / `--target-
     arch' / `--target-format' helpers, and provides `compile-elisp-
     objects' so the meta-driver's own load body short-circuits.
  3. Locate and load `compile-elisp-objects-meta.el', which builds the
     static manifest vector + the per-entry walker around the A25.2
     `nelisp-cc-meta-driver' kernel source.
  4. Invoke `compile-elisp-objects-meta-emit-subset' which compiles the
     5-entry PoC subset (= spike-noop, fact-i64, truncate-int,
     length-cons, cons-construct) into the same `target/elisp-objects/'
     directory as the legacy driver.  Output paths share the production
     names so a byte-identity probe against the host-Emacs reference is
     just `cmp $REF $OUT'.

Returns an integer exit code (0 = success, 1 = error)."
  (condition-case err
      (progn
        (nelisp--cli-meta-driver-load-deps)
        (let* ((production-script
                (nelisp--cli-meta-driver-locate-script "compile-elisp-objects.el"))
               (meta-script
                (nelisp--cli-meta-driver-locate-script
                 "compile-elisp-objects-meta.el")))
          (unless production-script
            (signal 'error
                    (list "compile-elisp-objects.el not on load-path \
(pass -L scripts/ before driving the meta path)")))
          (unless meta-script
            (signal 'error
                    (list "compile-elisp-objects-meta.el not on load-path \
(pass -L scripts/ before driving the meta path)")))
          (load production-script nil t)
          (load meta-script nil t)
          ;; Wave A27 — when `NELISP_META_DRIVER_FULL=1' is set in the
          ;; environment, drive the full 212-entry production manifest
          ;; via `compile-elisp-objects-meta-emit-all' (chunked walker
          ;; dispatch).  Default `NELISP_USE_META_DRIVER=1' alone keeps
          ;; the A26 5-entry PoC subset (= `-emit-subset') for backward
          ;; compatibility with the spike validation runs.
          (let* ((full-p (let ((v (getenv "NELISP_META_DRIVER_FULL")))
                           (and v (stringp v) (> (length v) 0)
                                (not (equal v "0")))))
                 (emit-fn-name (if full-p
                                   "compile-elisp-objects-meta-emit-all"
                                 "compile-elisp-objects-meta-emit-subset"))
                 (emit-fn (intern emit-fn-name)))
            (unless (fboundp emit-fn)
              (signal 'error
                      (list (format "%s not bound after meta-script load"
                                    emit-fn-name))))
            (let ((paths (funcall emit-fn)))
              (nelisp--write-stdout-bytes
               (format "[nelisp-meta-driver%s] wrote %d objects\n"
                       (if full-p "/full" "")
                       (length paths)))
              0))))
    (error
     (nelisp--cli-print-error
      (format "meta-driver error: %s" (error-message-string err)))
     1)))

(defun nelisp--cli-batch-dispatch (args)
  "Process the batch-mode ARGS list (argv without binary name + --batch).

ARGS is a list of strings.  We walk it left-to-right:

  -Q | --quick           skip (no init file to worry about)
  -L DIR | --directory DIR   add DIR to load-path, then try to load
                             nelisp-build-host for full host API
  -l FILE | --load FILE      load FILE silently
  -f FUNC | --funcall FUNC   call FUNC
  --eval EXPR                evaluate EXPR
  --setenv VAR=VAL           shorthand: (setenv VAR VAL)

Wave A25.3 — when `NELISP_USE_META_DRIVER=1' is set in the environment,
the dispatcher walks ARGS as usual up through `-L' / `--setenv' setup,
then short-circuits the remaining `-l' / `-f' chain by invoking
`nelisp--cli-meta-driver-main' which drives the AOT-friendly
static-manifest walker directly.  This bypasses the legacy elisp
iteration chain (= `dolist' + `plist-get' + `(require feature)') while
keeping the per-entry `.o' emit byte-identical to the host-Emacs
reference path.

Returns an integer exit code (0 = success, 1 = error, 2 = bad args)."
  ;; Bare minimum bootstrap so -L and --setenv work before any file is loaded.
  (nelisp--cli-batch-ensure-host)
  ;; Wave A25.3 — `NELISP_USE_META_DRIVER=1' short-circuits the elisp
  ;; interpreter dispatch chain.  We still walk ARGS to honor `-L', `-Q',
  ;; `--setenv', and `--eval' (= setup-only flags that prepare the
  ;; load-path + env state for the meta-driver), but skip `-l' / `-f'
  ;; because those drive the legacy `compile-elisp-objects-emit-range'
  ;; iteration which the meta-driver replaces.
  ;;
  ;; The detection scans ARGS for a `--setenv NELISP_USE_META_DRIVER=...'
  ;; pair before the main walk so the `-l' / `-f' skip decision is in
  ;; force from the first argv entry — `getenv' alone would be too late
  ;; because the elisp shim's `process-environment' is empty until a
  ;; `--setenv' pair runs through the dispatcher.  An OS-side env-var
  ;; check via Rust would also work but adds Rust LOC (= violates the
  ;; A25.3 hard rule of LOC delta = 0).
  (let* ((meta-driver-p
          (or (let ((v (getenv "NELISP_USE_META_DRIVER")))
                (and v (stringp v) (> (length v) 0) (not (equal v "0"))))
              ;; Pre-scan ARGS for `--setenv NELISP_USE_META_DRIVER=...'
              ;; so the flag is honored even though the actual setenv
              ;; pair hasn't been processed yet.  The literal prefix
              ;; `NELISP_USE_META_DRIVER=' is 23 chars long; a `0' or
              ;; empty value is treated as a disable flag (matches the
              ;; getenv branch above).
              (let ((scan args)
                    (hit nil))
                (while (and scan (not hit))
                  (let ((flag (car scan)))
                    (when (and (equal flag "--setenv")
                               (cdr scan)
                               (stringp (cadr scan))
                               (let ((pair (cadr scan)))
                                 (and (>= (length pair) 23)
                                      (equal (substring pair 0 23)
                                             "NELISP_USE_META_DRIVER=")))
                               (let* ((pair (cadr scan))
                                      (val (substring pair 23)))
                                 (and (> (length val) 0)
                                      (not (equal val "0")))))
                      (setq hit t)))
                  (setq scan (cdr scan)))
                hit)))
         (exit-code 0)
         (cur args))
    (while (and cur (= exit-code 0))
      (let ((flag (car cur)))
        (setq cur (cdr cur))
        (cond
         ;; -Q / --quick — ignore init file; no-op here
         ((or (equal flag "-Q") (equal flag "--quick"))
          nil)
         ;; -L DIR / --directory DIR
         ((or (equal flag "-L") (equal flag "--directory"))
          (let ((dir (car cur)))
            (setq cur (cdr cur))
            (if dir
                (progn
                  (add-to-list 'load-path dir)
                  ;; Retry build-host load now that load-path is non-empty.
                  (nelisp--cli-batch-ensure-host))
              (nelisp--cli-print-error "-L requires a directory argument")
              (setq exit-code 2))))
         ;; -l FILE / --load FILE
         ((or (equal flag "-l") (equal flag "--load"))
          (let ((file (car cur)))
            (setq cur (cdr cur))
            (cond
             ((null file)
              (nelisp--cli-print-error "-l requires a file argument")
              (setq exit-code 2))
             (meta-driver-p
              ;; Wave A25.3 — skip the load so the legacy dispatch chain
              ;; doesn't fight the meta-driver's pre-resolved deps.
              nil)
             (t
              (setq exit-code (nelisp--cli-batch-load-file file))))))
         ;; -f FUNC / --funcall FUNC
         ((or (equal flag "-f") (equal flag "--funcall"))
          (let ((func (car cur)))
            (setq cur (cdr cur))
            (cond
             ((null func)
              (nelisp--cli-print-error "-f requires a function name argument")
              (setq exit-code 2))
             (meta-driver-p
              ;; Wave A25.3 — skip the funcall; the meta-driver runs at
              ;; loop exit and supersedes whatever entry point the caller
              ;; would have driven (typically `compile-elisp-objects-emit-
              ;; range').
              nil)
             (t
              (setq exit-code (nelisp--cli-batch-funcall func))))))
         ;; --eval EXPR
         ((equal flag "--eval")
          (let ((expr (car cur)))
            (setq cur (cdr cur))
            (if expr
                (setq exit-code (nelisp--cli-batch-eval-expr expr))
              (nelisp--cli-print-error "--eval requires an expression argument")
              (setq exit-code 2))))
         ;; --setenv VAR=VAL
         ((equal flag "--setenv")
          (let ((pair (car cur)))
            (setq cur (cdr cur))
            (if (and pair (stringp pair))
                (let* ((eq-pos (let ((i 0) (found nil))
                                 (while (and (< i (length pair)) (not found))
                                   (when (= (aref pair i) ?=)
                                     (setq found i))
                                   (setq i (1+ i)))
                                 found))
                       (var (and eq-pos (substring pair 0 eq-pos)))
                       (val (and eq-pos (substring pair (1+ eq-pos)))))
                  (if var
                      (setenv var val)
                    (nelisp--cli-print-error
                     (format "--setenv argument must be VAR=VAL, got: %s" pair))
                    (setq exit-code 2)))
              (nelisp--cli-print-error "--setenv requires a VAR=VAL argument")
              (setq exit-code 2))))
         ;; Unknown flag
         (t
          (nelisp--cli-print-error
           (format "unrecognized batch flag: %s" flag))
          (setq exit-code 2)))))
    ;; Wave A25.3 — after argv setup (= -L / --setenv etc.) completes
    ;; cleanly, if `NELISP_USE_META_DRIVER' is set drive the AOT
    ;; meta-driver directly.  This replaces whatever `-l' / `-f' chain
    ;; the caller passed (we already skipped them above) — the meta-
    ;; driver's iteration kernel is the unit of work for this batch run.
    (when (and meta-driver-p (= exit-code 0))
      (setq exit-code (nelisp--cli-meta-driver-main)))
    exit-code))

(defun nelisp-cli-main (argv)
  "Entry point invoked by `build-tool/src/bin/nelisp.rs'.

ARGV is a list of strings (= the C `argv'); ARGV[0] is the binary
name (typically `nelisp').  Returns an integer exit code consumed by
the Rust stub's `ExitCode::from'.

See file header for the CLI surface + exit-code contract."
  (let* ((args (and (listp argv) (cdr argv)))
         (n (length args)))
    (cond
     ;; --batch mode (Doc 49 Wave 7) — Emacs-style multi-flag processing
     ((and (>= n 1) (equal (nth 0 args) "--batch"))
      (nelisp--cli-batch-dispatch (cdr args)))
     ;; --version / -V
     ((and (= n 1)
           (or (equal (nth 0 args) "--version")
               (equal (nth 0 args) "-V")))
      (nelisp--write-stdout-bytes (format "nelisp %s\n" nelisp--cli-version))
      0)
     ;; eval EXPR
     ((and (= n 2) (equal (nth 0 args) "eval"))
      (nelisp--cli-eval-string-print (nth 1 args)))
     ;; -l FILE / --load FILE
     ((and (= n 2)
           (or (equal (nth 0 args) "-l")
               (equal (nth 0 args) "--load")))
      (let ((src (nelisp--cli-read-file (nth 1 args))))
        (if src (nelisp--cli-eval-string-all-print src) 1)))
     ;; exec FILE
     ((and (= n 2) (equal (nth 0 args) "exec"))
      (let ((src (nelisp--cli-read-file (nth 1 args))))
        (if src (nelisp--cli-exec-string-silent src) 1)))
     ;; compile-elisp-artifact ...
     ((and (>= n 1) (equal (nth 0 args) "compile-elisp-artifact"))
      (nelisp--cli-run-artifact-command #'compile-elisp-artifact args))
     ;; exec-elisp-artifact FILE.nelc FORM...
     ((and (>= n 2) (equal (nth 0 args) "exec-elisp-artifact"))
      (nelisp--cli-run-artifact-command #'exec-elisp-artifact args))
     ;; eval-elisp-artifact FILE.nelc FORM...
     ((and (>= n 2) (equal (nth 0 args) "eval-elisp-artifact"))
      (nelisp--cli-run-artifact-command #'eval-elisp-artifact args))
     ;; inspect-elisp-artifact FILE.nelc
     ((and (>= n 2) (equal (nth 0 args) "inspect-elisp-artifact"))
      (nelisp--cli-run-artifact-command #'inspect-elisp-artifact args))
     ;; -  (stdin)
     ((and (= n 1) (equal (nth 0 args) "-"))
      (nelisp--cli-eval-string-all-print (nelisp--cli-slurp-stdin)))
     ;; usage error
     (t
      (nelisp--write-stderr-line nelisp--cli-usage)
      2))))

(provide 'nelisp-cli)

;;; nelisp-cli.el ends here
