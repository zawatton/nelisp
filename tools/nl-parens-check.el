;;; nl-parens-check.el --- Batch gate for unbalanced Emacs Lisp parens -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

(require 'nl-parens)
(require 'seq)
(require 'subr-x)

(defun nl-parens-check--files-in (directory)
  "Return every Emacs Lisp file below DIRECTORY, or nil when it is absent."
  (if (file-directory-p directory)
      (directory-files-recursively directory "\\.el\\'")
    nil))

(defconst nl-parens-check--enclosing-file "tools/parens-enclosing-forms.txt"
  "Files that wrap column-0 definitions in one enclosing column-0 form.")

(defun nl-parens-check--enclosing-paths ()
  "Return the relative paths listed in `nl-parens-check--enclosing-file'."
  (when (file-exists-p nl-parens-check--enclosing-file)
    (with-temp-buffer
      (insert-file-contents nl-parens-check--enclosing-file)
      (let ((paths nil))
        (dolist (line (split-string (buffer-string) "\n" t))
          (setq line (string-trim line))
          (unless (or (string-empty-p line) (string-prefix-p "#" line))
            (push line paths)))
        (nreverse paths)))))

(defun nl-parens-check--whole-file-problem (path)
  "Return a message when PATH does not read as balanced Emacs Lisp, else nil.
Every top-level sexp is skipped with the Emacs Lisp syntax table; the
loop stops at end of buffer, so a stray close, an unterminated form or an
unbalanced paren anywhere is reported with its line."
  (with-temp-buffer
    (insert-file-contents path)
    (with-syntax-table emacs-lisp-mode-syntax-table
      (goto-char (point-min))
      (condition-case err
          (progn
            (while (progn (forward-comment (buffer-size)) (not (eobp)))
              (forward-sexp 1))
            nil)
        (scan-error
         (format "%s:%d: error: does not read as balanced Emacs Lisp (%s)"
                 path (line-number-at-pos (or (nth 2 err) (point)))
                 (cadr err)))))))

(let ((paths nil))
  (dolist (directory '("lisp" "src" "scripts"))
    (setq paths (append paths (nl-parens-check--files-in directory))))
  (dolist (package (directory-files "packages" t "\\`[^.]"))
    (setq paths (append paths (nl-parens-check--files-in
                               (expand-file-name "src" package))
                        (nl-parens-check--files-in
                         (expand-file-name "test" package)))))
  (let* ((enclosing (nl-parens-check--enclosing-paths))
         (listed-p (lambda (path) (member (file-relative-name path) enclosing)))
         (findings (nl-parens-check-files (seq-remove listed-p paths)))
         (whole (delq nil (mapcar #'nl-parens-check--whole-file-problem
                                  (seq-filter listed-p paths)))))
    (dolist (stale (seq-remove (lambda (rel) (seq-find (lambda (p) (equal (file-relative-name p) rel)) paths))
                               enclosing))
      (push (format "%s:1: error: listed in %s but not scanned (missing file)"
                    stale nl-parens-check--enclosing-file)
            whole))
    (princ (nl-parens-report findings))
    (dolist (message whole) (princ (concat message "\n")))
    (setq findings (append findings whole))
    ;; Machine-readable tail.  A clean run of this gate prints nothing
    ;; at all, which is indistinguishable from a run that scanned no
    ;; files — if `paths' were ever empty (wrong working directory, a
    ;; renamed package layout) the gate would pass in silence.  The
    ;; contract is in tools/ai/README.md.
    (princ (format "GATE-COUNT checked=%d findings=%d\n"
                   (length paths) (length findings)))
    (when findings (kill-emacs 1))))

;;; nl-parens-check.el ends here
