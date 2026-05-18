;;; nelisp-cli.el --- Doc 128 — End-user `nelisp' CLI dispatch (elisp side)  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 128 — pushes the CLI parsing + sub-command dispatch out of the
;; Rust binary `build-tool/src/bin/nelisp.rs' down into elisp so the
;; production binary is a ~50 LOC bootstrap stub (= Env::new_global
;; + lookup `nelisp-cli-main' + apply + map ExitCode).
;;
;; Surface (unchanged from the pre-Doc-128 Rust impl):
;;
;;   nelisp --version | -V         print version + exit
;;   nelisp eval EXPR              evaluate EXPR + print result
;;   nelisp -l FILE | --load FILE  load + eval FILE, print last value
;;   nelisp exec FILE              load + eval FILE silently
;;   nelisp -                      read stdin + eval, print last value
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

Note: image manipulation (compile-image / eval-image) lives in the
dev-tooling `nelisp-baker' binary behind the `image-baker' feature
(= `make bake-images' / `cargo run --bin nelisp-baker --features
image-baker')."
  "USAGE banner — adapted verbatim from the pre-Doc-128 Rust const.")

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

(defun nelisp-cli-main (argv)
  "Entry point invoked by `build-tool/src/bin/nelisp.rs'.

ARGV is a list of strings (= the C `argv'); ARGV[0] is the binary
name (typically `nelisp').  Returns an integer exit code consumed by
the Rust stub's `ExitCode::from'.

See file header for the CLI surface + exit-code contract."
  (let* ((args (and (listp argv) (cdr argv)))
         (n (length args)))
    (cond
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
