;;; reader-surface-audit.el --- detect fboundp-liar builtins in the reader  -*- lexical-binding: t; -*-

;; Doc 11 follow-up (2026-06-10): `nelisp--syscall-readdir' was listed in
;; `nelisp-standalone--reader-builtins' (so `fboundp' returned t) but had no
;; dispatch arm, so calling it aborted the caller via the combiner's
;; deferred-apply stash.  This audit mechanically detects that whole bug
;; class: every name whose fboundp-truth comes from the builtin name list
;; MUST have a matching `(:lit "NAME")' / `(:u8 "NAME")' dispatch arm.
;;
;; Usage:  emacs --batch -Q -L lisp -L src -L scripts \
;;           -l reader-surface-audit -f nelisp-reader-surface-audit
;; Exit 0 when clean; exit 1 and list the liars otherwise.
;;
;; Limitation: semantic gaps (e.g. the empty-envp execve bug) are invisible
;; to a name-level audit; those still need behavioural smokes.

;;; Code:

(require 'nelisp-standalone-build)

(defun nelisp-reader-surface-audit--dispatch-names ()
  "Return every dispatch-arm name ((:lit \"X\") / (:u8 \"X\")) in the build script."
  (let ((file (locate-library "nelisp-standalone-build.el"))
        (names nil))
    (unless file
      (setq file (expand-file-name "nelisp-standalone-build.el"
                                   (file-name-directory load-file-name))))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (while (re-search-forward "((:\\(?:lit\\|u8\\) \"\\([^\"]+\\)\")" nil t)
        (push (match-string 1) names)))
    names))

(defconst nelisp-reader-surface-audit--combiner-handled
  '("eval" "funcall" "apply" "fset" "symbol-function"
    "nelisp--push-frame" "nelisp--pop-frame" "nelisp--bind-local"
    "nelisp--push-captured" "nelisp--set-use-elisp" "nelisp--env-globals-op")
  "Names the apply combiner executes directly (cls 1) — no dispatch arm needed.")

(defconst nelisp-reader-surface-audit--combiner-deferred
  '("signal" "nelisp--syscall-stat" "nelisp--syscall-readdir"
    "nelisp--syscall-read-file" "nelisp--syscall-canonicalize"
    "nelisp--apply-lambda-payload" "nelisp--apply-builtin-payload")
  "Names the apply combiner stashes as deferred (cls 2) — calling one through
the apply path aborts the caller.  Standalone-facing code must never rely on
these; prefer the dispatch-armed equivalents (e.g.
`nelisp--syscall-readdir-names').")

(defun nelisp-reader-surface-audit ()
  "Report builtin names that claim fboundp but have no dispatch arm."
  (let* ((claimed nelisp-standalone--reader-builtins)
         (dispatch (nelisp-reader-surface-audit--dispatch-names))
         (liars nil)
         (hidden nil))
    (dolist (name claimed)
      (unless (or (member name dispatch)
                  (member name nelisp-reader-surface-audit--combiner-handled))
        (push name liars)))
    (dolist (name dispatch)
      (unless (member name claimed)
        (push name hidden)))
    (princ (format "reader-surface-audit: %d claimed, %d dispatch arms\n"
                   (length claimed) (length dispatch)))
    (when hidden
      (princ (format "  info: %d dispatch arms not in the fboundp list (callable, fboundp nil):\n"
                     (length hidden)))
      (dolist (name (sort hidden #'string<))
        (princ (format "    %s\n" name))))
    (princ (format "  note: %d combiner-deferred names abort when applied — never rely on them:\n"
                   (length nelisp-reader-surface-audit--combiner-deferred)))
    (dolist (name nelisp-reader-surface-audit--combiner-deferred)
      (princ (format "    %s\n" name)))
    (if (null liars)
        (princ "  PASS: every claimed builtin has a dispatch arm\n")
      (princ (format "  FAIL: %d fboundp-liar builtin(s) — fboundp t, call aborts:\n"
                     (length liars)))
      (dolist (name (sort liars #'string<))
        (princ (format "    %s\n" name)))
      (kill-emacs 1))))

(provide 'reader-surface-audit)

;;; reader-surface-audit.el ends here
