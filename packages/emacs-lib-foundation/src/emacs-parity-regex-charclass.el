;;; emacs-parity-regex-charclass.el --- literal backslash in [...] -*- lexical-binding: t; -*-

;; The standalone NeLisp regexp engine (`nelisp-regex.el', baked into the
;; runtime image / prebuilt bootstrap) parsed a backslash inside a `[...]'
;; character class as a PCRE-style escape: `nelisp-rx--class-char' consumed the
;; `\' and returned the FOLLOWING character as a literal.  GNU Emacs instead
;; treats `\' as an ORDINARY literal character inside a bracket expression, so
;;
;;   [^\]   means "not backslash"        (its `]' closes the class)
;;   [\]    matches a backslash
;;   [\w]   matches a literal `\' or `w'
;;
;; The escape behaviour mis-parsed `[^\]' as an unterminated class (the `]'
;; that should close it was swallowed as an escaped literal), so `string-match'
;; signalled `nelisp-rx-syntax-error' ("unterminated character class" /
;; "missing \) for group") on EVERY Emacs regexp with a backslash inside a
;; char class.  The visible casualty in the real-init audit is CC Mode:
;; `noncontinued-line-end' = "\\(\\=\\|\\(\\=\\|[^\\]\\)[\n\r]\\)" is evaluated
;; while building `c-cpp-matchers' (= `c-matchers-1'), producing 15 rx-syntax
;; errors and the downstream `(c-lang-const c-matchers-N java)' bare-aborts,
;; and blocking `(provide 'cc-mode)'.  The bug is pervasive (any package with a
;; backslash in a bracket expression), not CC-specific.
;;
;; `nelisp-regex.el' is INLINED into the prebuilt bootstrap, so editing the
;; source alone does not reach an already-built binary.  Redefine the reader
;; here, loaded from the `nemacs-next-session' parity dolist before user init;
;; `src/nelisp-regex.el' carries the same fix for rebuilt runtime images.
;; `nelisp-rx--parse-class' calls `nelisp-rx--class-char' by name (not inlined),
;; so overriding the reader corrects class parsing without touching the parser.
;;
;; Guarded to the standalone substrate (the reader only exists there); a host
;; Emacs, which uses its own C regexp engine, is left untouched.

(when (and (fboundp 'nelisp-rx--peek)
           (fboundp 'nelisp-rx--advance))
  (defun nelisp-rx--class-char ()
    "Read one literal character inside `[...]' with GNU Emacs semantics.
Backslash is an ordinary literal character inside a character class; it does
not introduce an escape.  Return the character as an integer."
    (let ((c (nelisp-rx--peek)))
      (cond
       ((null c)
        (signal 'nelisp-rx-syntax-error '("unterminated class")))
       (t (nelisp-rx--advance) c)))))

;; GNU Emacs' `subregexp-context-p' deliberately treats malformed prefixes
;; as a context check result: errors for an unfinished class/group are caught,
;; while other syntax errors are re-signalled.  The standalone regexp reader
;; uses its own condition name, so the vendor `subr.el' implementation must
;; be replaced after it loads.
(defun emacs-parity-regex-charclass--prefix-p (prefix string)
  "Return non-nil when STRING starts with PREFIX without using regexps."
  (and (stringp string)
       (>= (length string) (length prefix))
       (equal prefix (substring string 0 (length prefix)))))

(when (fboundp 'nelisp-rx--parse)
  (defun subregexp-context-p (regexp pos &optional start)
    "Return non-nil when POS is in a normal subregexp context in REGEXP."
    (condition-case err
        (progn
          (string-match (substring regexp (or start 0) pos) "")
          t)
      (nelisp-rx-syntax-error
       (let ((message (cadr err)))
         (not (or (emacs-parity-regex-charclass--prefix-p
                   "unterminated character class" message)
                  (emacs-parity-regex-charclass--prefix-p
                   "unterminated class" message)
                  (emacs-parity-regex-charclass--prefix-p
                   "unterminated \\{...\\}" message)
                  (emacs-parity-regex-charclass--prefix-p
                   "trailing backslash" message)))))
      (invalid-regexp
       (not (member (cadr err) '("Unmatched [ or [^"
                                  "Unmatched \\{"
                                  "Trailing backslash")))))))

(provide 'emacs-parity-regex-charclass)
;;; emacs-parity-regex-charclass.el ends here
