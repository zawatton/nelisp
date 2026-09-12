;;; nelisp-repl-script-lines.el --- script file -> standalone REPL input lines -*- lexical-binding: t; -*-

;;; Commentary:

;; The standalone REPL is line-oriented: `nl_repl_read_line' stops at the
;; first newline and `nl_repl_make_source' wraps exactly that text in the
;; value-printing form.  A single form written across two lines is therefore
;; cut in half and each half is evaluated on its own.  Measured 2026-09-12
;; against target/nelisp built from 2fb0fd8f3:
;;
;;     printf '(princ (format "SUM=%%S\n"\n  (+ 1 2)))\n(exit)\n' \
;;       | tools/ai/nelisp-ai.sh repl --no-prompt
;;     => invalid-read-syntax: "(let ((v (progn\n(princ (format \"SUM=%S\\n\"\n)))..."
;;
;; So every scripted session -- an agent piping forms in, a saved reproduction
;; recipe, a smoke test -- has to hand-flatten each form onto one physical
;; line, and hand-flattening quoted source is exactly where quoting mistakes
;; come from.
;;
;; This generator removes that constraint without changing the REPL.  Host
;; Emacs -- the real Lisp reader, never a regex and never a paren counter --
;; splits the script into top-level forms, and each form's EXACT source text
;; is emitted as ONE line in which the form's own newlines travel as string
;; escapes.  The REPL still reads one line, evaluates one form, prints one
;; value, and contains an error to the form that raised it.
;;
;; What this does NOT do: it does not evaluate the script here, it does not
;; reformat or re-print the forms (the bytes between the reader's start and
;; end positions are copied verbatim), and it does not merge forms.  A file
;; whose last form is incomplete is reported with its line number instead of
;; being silently truncated.

;;; Code:

(defconst nelisp-repl-script-lines-helper
  "(defun nelisp-repl--eval-source (source) (eval (car (read-from-string source)) t))"
  "The one definition the generated lines call.

Emitted once per generated chunk; re-emitting it is an identical
redefinition, so a session that loads several scripts stays correct.
`eval' is called with LEXICAL non-nil so a script sees the same binding
rule a `-*- lexical-binding: t -*-' source file would get.")

(defconst nelisp-repl-script-lines-line-cap (- 4194304 512)
  "Largest generated line, in bytes.

The REPL's own input buffer is `nelisp-standalone--reader-read-cap'
\(scripts/nelisp-standalone-build.el) and `nl_repl_read_line_loop' stops
512 bytes short of it.  A longer line would be silently truncated at that
boundary and then fail to read, so this generator refuses it instead.
`nelisp-repl-script-lines-cap-matches-reader-p' asserts the two agree.")

(defun nelisp-repl-script-lines-cap-matches-reader-p ()
  "Return non-nil when the line cap still matches the REPL's read cap.

Returns nil when `nelisp-standalone--reader-read-cap' is not loaded, so a
caller can tell \"not checked\" from \"checked and equal\"."
  (and (boundp 'nelisp-standalone--reader-read-cap)
       (= nelisp-repl-script-lines-line-cap
          (- (symbol-value 'nelisp-standalone--reader-read-cap) 512))))

(defun nelisp-repl-script-lines--form-texts (file)
  "Return the exact source text of each top-level form in FILE, in order.

Leading whitespace and comments are skipped with the reader's own syntax,
so a returned string starts at the form's first character."
  (with-temp-buffer
    (let ((coding-system-for-read 'utf-8-unix))
      (insert-file-contents file))
    (goto-char (point-min))
    (let ((texts nil)
          (done nil))
      (with-syntax-table emacs-lisp-mode-syntax-table
        (while (not done)
          (forward-comment (buffer-size))
          (if (eobp)
              (setq done t)
            (let ((start (point)))
              (condition-case nil
                  (read (current-buffer))
                (end-of-file
                 (error "%s:%d: file ends inside a form"
                        file (line-number-at-pos start))))
              (push (buffer-substring-no-properties start (point)) texts)))))
      (nreverse texts))))

(defun nelisp-repl-script-lines--line (text file index)
  "Return the single REPL input line that evaluates TEXT.

FILE and INDEX name the form only in the error raised when the line would
exceed `nelisp-repl-script-lines-line-cap' or would not be one line."
  (let* ((print-escape-newlines t)
         (print-escape-control-characters t)
         (print-length nil)
         (print-level nil)
         (line (concat "(nelisp-repl--eval-source "
                       (prin1-to-string text)
                       ")")))
    (when (string-match-p "\n" line)
      (error "%s: form %d still contains a newline after escaping"
             file index))
    (when (> (string-bytes line) nelisp-repl-script-lines-line-cap)
      (error "%s: form %d is %d bytes, over the REPL line cap of %d"
             file index (string-bytes line)
             nelisp-repl-script-lines-line-cap))
    line))

(defun nelisp-repl-script-lines (file)
  "Return the REPL input lines that evaluate FILE, helper definition first."
  (let ((index 0))
    (cons nelisp-repl-script-lines-helper
          (mapcar (lambda (text)
                    (setq index (1+ index))
                    (nelisp-repl-script-lines--line text file index))
                  (nelisp-repl-script-lines--form-texts file)))))

(defun nelisp-repl-script-lines-batch ()
  "Append the lines for NELISP_REPL_SCRIPT_IN to NELISP_REPL_SCRIPT_OUT.

Both paths come from the environment so no path travels through shell
quoting or through Lisp source text."
  (let ((in (getenv "NELISP_REPL_SCRIPT_IN"))
        (out (getenv "NELISP_REPL_SCRIPT_OUT")))
    (when (or (null in) (string= in ""))
      (error "nelisp-repl-script-lines: NELISP_REPL_SCRIPT_IN is unset"))
    (when (or (null out) (string= out ""))
      (error "nelisp-repl-script-lines: NELISP_REPL_SCRIPT_OUT is unset"))
    (unless (file-readable-p in)
      (error "nelisp-repl-script-lines: cannot read %s" in))
    (let ((lines (nelisp-repl-script-lines in))
          (coding-system-for-write 'utf-8-unix))
      (with-temp-buffer
        (dolist (line lines)
          (insert line "\n"))
        (write-region (point-min) (point-max) out t 'silent)))))

(provide 'nelisp-repl-script-lines)

;;; nelisp-repl-script-lines.el ends here
