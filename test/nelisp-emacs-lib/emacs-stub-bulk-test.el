;;; emacs-stub-bulk-test.el --- ERT for the T51 nil-stub-race fixes  -*- lexical-binding: t; -*-

;;; Commentary:

;; T51 (2026-09-05).  `src/emacs-stub-bulk.el' used to stub `subr-arity',
;; `frame-char-width', and `frame-char-height' into its blanket
;; nil-returning bulk list (`(fset SYM (lambda (&rest _) nil))'), the
;; same fallback used for most of that list's rarely-exercised names.
;; Both were pure value-passing functions real callers use in
;; arithmetic, so a silent nil there does not fail gracefully -- it
;; blows up the *next* form:
;;
;;   - real, unmodified upstream `org-compat.el' calls
;;     `(subr-arity (symbol-function (quote get-buffer-create)))'
;;     unconditionally at `require' time to detect the Emacs<28 vs 28+
;;     calling convention; `(= 2 (cdr (subr-arity ...)))' then signals
;;     `wrong-type-argument number-or-marker-p' on a nil `subr-arity'.
;;   - `skk-vars.el' (loaded transitively by `ddskk') has
;;     `(defcustom skk-tooltip-x-offset (/ (1+ (frame-char-height)) 2)
;;     ...)' at its own load time; `1+' signals the same error on a nil
;;     `frame-char-height'.
;;
;; Both broke `require' for every one of `ob', every `ob-*' language,
;; `org-agenda', `org-id', `ox-icalendar', `ox-latex', `anki-editor',
;; `ddskk', and `vulpea' (the full 17-feature
;; `wrong-type-argument number-or-marker-p' cluster from the
;; package-load-matrix S1 report) -- two independent root causes
;; landing on the exact same error signature.
;;
;; This uses the same "unbind the host's real definition, reload the
;; source so its `unless (fboundp ...)' guard takes the polyfill
;; branch, restore afterward" technique as
;; `emacs-stub-residuals-test.el' uses for `emacs-stub.el', so the
;; real polyfill body is exercised and pinned under host Emacs too, not
;; only on the standalone reader (where host Emacs's real `subr-arity'/
;; `frame-char-width'/`frame-char-height' are never present to shadow
;; it).

;;; Code:

(require 'ert)
(require 'emacs-stub-bulk)

(defun emacs-stub-bulk-test--source-file (library)
  "Return source .el path for LIBRARY."
  (let ((file (locate-library library)))
    (when (and file (string-match-p "\\.elc\\'" file))
      (setq file (substring file 0 (1- (length file)))))
    file))

(ert-deftest emacs-stub-bulk-test/subr-arity-matches-host-for-get-buffer-create ()
  "The polyfill must match real (host) Emacs's own `subr-arity' value.
`get-buffer-create' is a true subr under host Emacs, so
`(subr-arity (symbol-function 'get-buffer-create))' there is the
authoritative GNU value this fix must reproduce."
  (skip-unless (fboundp 'subr-arity))
  (let* ((source (emacs-stub-bulk-test--source-file "emacs-stub-bulk"))
         (host-value (subr-arity (symbol-function 'get-buffer-create)))
         (saved (symbol-function 'subr-arity)))
    (unwind-protect
        (progn
          (fmakunbound 'subr-arity)
          (load source nil 'no-message)
          (should (fboundp 'subr-arity))
          (should (equal host-value
                         (subr-arity (symbol-function 'get-buffer-create)))))
      (fset 'subr-arity saved))))

(ert-deftest emacs-stub-bulk-test/subr-arity-polyfill-shapes ()
  "Pin the polyfill's own required/optional/rest counting logic.
Runs regardless of host-Emacs presence, matching the
`emacs-stub-residuals-test.el' \"polyfill-body shape parity\"
convention: this exercises what the standalone reader will see when
the polyfill actually fires, independent of whether `subr-arity' also
happens to already be host-bound in this process."
  (let* ((source (emacs-stub-bulk-test--source-file "emacs-stub-bulk"))
         (saved (and (fboundp 'subr-arity) (symbol-function 'subr-arity))))
    (unwind-protect
        (progn
          (fmakunbound 'subr-arity)
          (load source nil 'no-message)
          (should (equal '(1 . 2) (subr-arity (lambda (a &optional b) (list a b)))))
          (should (equal '(0 . 0) (subr-arity (lambda () nil))))
          (should (equal '(2 . many) (subr-arity (lambda (a b &rest c) (list a b c)))))
          (should (equal '(0 . many) (subr-arity (lambda (&rest c) c)))))
      (if saved
          (fset 'subr-arity saved)
        (fmakunbound 'subr-arity)))))

(ert-deftest emacs-stub-bulk-test/frame-char-dimensions-match-emacs-frame-when-loaded ()
  "When `emacs-frame' is loaded, the polyfill must delegate to it.
`emacs-frame-frame-char-width'/`-height' are the real, frame-aware
implementation (Doc 34 Sec 2.11 LOCKED invariant: 8x16 default cell);
the polyfill must reproduce their value, not some independent
constant."
  (require 'emacs-frame)
  (let* ((source (emacs-stub-bulk-test--source-file "emacs-stub-bulk"))
         (expected-width (emacs-frame-frame-char-width))
         (expected-height (emacs-frame-frame-char-height))
         (saved-width (and (fboundp 'frame-char-width) (symbol-function 'frame-char-width)))
         (saved-height (and (fboundp 'frame-char-height) (symbol-function 'frame-char-height))))
    (unwind-protect
        (progn
          (fmakunbound 'frame-char-width)
          (fmakunbound 'frame-char-height)
          (load source nil 'no-message)
          (should (equal expected-width (frame-char-width)))
          (should (equal expected-height (frame-char-height))))
      (if saved-width (fset 'frame-char-width saved-width) (fmakunbound 'frame-char-width))
      (if saved-height (fset 'frame-char-height saved-height) (fmakunbound 'frame-char-height)))))

(ert-deftest emacs-stub-bulk-test/frame-char-dimensions-fall-back-without-emacs-frame ()
  "Without `emacs-frame' loaded at all, the polyfill still returns a
real number (the same 8x16 LOCKED-invariant default), never nil."
  (let* ((source (emacs-stub-bulk-test--source-file "emacs-stub-bulk"))
         (saved-width (and (fboundp 'frame-char-width) (symbol-function 'frame-char-width)))
         (saved-height (and (fboundp 'frame-char-height) (symbol-function 'frame-char-height)))
         (was-emacs-frame-bound (fboundp 'emacs-frame-frame-char-width))
         (saved-emacs-frame-width (and was-emacs-frame-bound
                                        (symbol-function 'emacs-frame-frame-char-width))))
    (unwind-protect
        (progn
          (fmakunbound 'frame-char-width)
          (fmakunbound 'frame-char-height)
          ;; Simulate "emacs-frame never loaded" regardless of what an
          ;; earlier test in this same batch process pulled in.
          (fmakunbound 'emacs-frame-frame-char-width)
          (load source nil 'no-message)
          (should (equal 8 (frame-char-width)))
          (should (equal 16 (frame-char-height))))
      (if saved-width (fset 'frame-char-width saved-width) (fmakunbound 'frame-char-width))
      (if saved-height (fset 'frame-char-height saved-height) (fmakunbound 'frame-char-height))
      (when was-emacs-frame-bound
        (fset 'emacs-frame-frame-char-width saved-emacs-frame-width)))))

(ert-deftest emacs-stub-bulk-test/subr-arity-not-in-nil-stub-list ()
  "`subr-arity'/`frame-char-width'/`frame-char-height' must not remain
in the blanket nil-returning bulk list.  Regression guard for the
exact defect this test file exists for: being present there means ANY
caller (with a real host or standalone definition installed later or
not) can never see a fix land, because the dolist there only checks
`unless (fboundp ...)' -- if any of these names is still enumerated, a
future edit could reintroduce a nil re-stub ordering hazard."
  (let ((source (emacs-stub-bulk-test--source-file "emacs-stub-bulk")))
    (with-temp-buffer
      (insert-file-contents source)
      (goto-char (point-min))
      (should (re-search-forward "--stub-defuns--" nil t))
      (let ((list-start (point))
            (list-end (progn (re-search-forward "--stub-defmacros--" nil t)
                              (point))))
        (goto-char list-start)
        (should-not (re-search-forward "\\_<subr-arity\\_>" list-end t))
        (goto-char list-start)
        (should-not (re-search-forward "\\_<frame-char-width\\_>" list-end t))
        (goto-char list-start)
        (should-not (re-search-forward "\\_<frame-char-height\\_>" list-end t))))))

(provide 'emacs-stub-bulk-test)
;;; emacs-stub-bulk-test.el ends here
