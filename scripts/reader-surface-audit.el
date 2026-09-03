;;; reader-surface-audit.el --- detect fboundp-liar builtins in the reader  -*- lexical-binding: t; -*-

;; Doc 11 follow-up (2026-06-10): `nelisp--syscall-readdir' was listed in
;; `nelisp-standalone--reader-builtins' (so `fboundp' returned t) but had no
;; dispatch arm, so calling it aborted the caller via the combiner's
;; deferred-apply stash.  This audit mechanically detects that whole bug
;; class: every name whose fboundp-truth comes from the builtin name list
;; MUST occur in the dispatch table the reader builder actually returns.
;;
;; Usage:  emacs --batch -Q -L lisp -L src -L scripts \
;;           -l reader-surface-audit -f nelisp-reader-surface-audit
;; Exit 0 when clean; exit 1 and list the liars otherwise.
;;
;; Limitation: semantic gaps (e.g. the empty-envp execve bug) are invisible
;; to a name-level audit; those still need behavioural smokes.

;;; Code:

(require 'nelisp-standalone-build)

(defconst nelisp-reader-surface-audit--targets
  '(linux-x86_64 linux-aarch64 macos-aarch64 windows-x86_64)
  "Release targets whose reader dispatch tables this audit checks.")

(defun nelisp-reader-surface-audit--target-dispatch-names (target)
  "Return the names in the reader dispatch table built for TARGET."
  (let ((nelisp-standalone--target target))
    (mapcar
     (lambda (arm)
       (let ((key (car arm)))
         (unless (and (consp key)
                      (memq (car key) '(:lit :u8))
                      (stringp (cadr key)))
           (error "Malformed reader dispatch arm for %s: %S" target arm))
         (cadr key)))
     (nelisp-standalone--applyfn-reader-table))))

(defun nelisp-reader-surface-audit--dispatch-names ()
  "Return names with a dispatch arm on every release target.

Ask the final reader-table builder instead of scanning its source spelling.
That makes literal, `cons'-built, and future computed arms indistinguishable,
so changing how an arm is constructed cannot make this audit rot.  Build all
release-target variants rather than trusting the host/default target; a name
missing from even one target is absent from the returned intersection."
  (let ((tables
         (mapcar #'nelisp-reader-surface-audit--target-dispatch-names
                 nelisp-reader-surface-audit--targets)))
    (cl-reduce
     (lambda (common names)
       (cl-remove-if-not (lambda (name) (member name names)) common))
     (cdr tables)
     :initial-value (car tables))))

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
    ;; Machine-readable tail, before the verdict so it survives both
    ;; exit paths.  `claimed' is the population this audit is about: if
    ;; `nelisp-standalone--reader-builtins' were ever empty the audit
    ;; would print PASS having compared nothing.  See tools/ai/README.md.
    (princ (format "GATE-COUNT checked=%d findings=%d\n"
                   (length claimed) (length liars)))
    (if (null liars)
        (princ "  PASS: every claimed builtin has a dispatch arm\n")
      (princ (format "  FAIL: %d fboundp-liar builtin(s) — fboundp t, call aborts:\n"
                     (length liars)))
      (dolist (name (sort liars #'string<))
        (princ (format "    %s\n" name)))
      (kill-emacs 1))))

(provide 'reader-surface-audit)

;;; reader-surface-audit.el ends here
