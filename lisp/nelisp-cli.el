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

;;; Code:

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

Returns an integer exit code (0 = success, 1 = error, 2 = bad args)."
  ;; Bare minimum bootstrap so -L and --setenv work before any file is loaded.
  (nelisp--cli-batch-ensure-host)
  (let ((exit-code 0)
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
            (if file
                (setq exit-code (nelisp--cli-batch-load-file file))
              (nelisp--cli-print-error "-l requires a file argument")
              (setq exit-code 2))))
         ;; -f FUNC / --funcall FUNC
         ((or (equal flag "-f") (equal flag "--funcall"))
          (let ((func (car cur)))
            (setq cur (cdr cur))
            (if func
                (setq exit-code (nelisp--cli-batch-funcall func))
              (nelisp--cli-print-error "-f requires a function name argument")
              (setq exit-code 2))))
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
     ;; -  (stdin)
     ((and (= n 1) (equal (nth 0 args) "-"))
      (nelisp--cli-eval-string-all-print (nelisp--cli-slurp-stdin)))
     ;; usage error
     (t
      (nelisp--write-stderr-line nelisp--cli-usage)
      2))))

(provide 'nelisp-cli)

;;; nelisp-cli.el ends here
