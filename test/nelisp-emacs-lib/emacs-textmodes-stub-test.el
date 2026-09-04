;;; emacs-textmodes-stub-test.el --- Phase 4 'C' polyfill tests  -*- lexical-binding: t; -*-

;;; Commentary:

;; Phase 4 'C' (2026-05-06): direct unit tests for
;; `emacs-textmodes--word-wrap' and `emacs-textmodes-fill-region' /
;; `-count-matches'.  The s.el integration tests in
;; `emacs-melpa-real-s-el-test.el' exercise the same code path
;; indirectly via host `fill-region' / `count-matches' (= our
;; polyfills are skipped under host because of the `unless (fboundp
;; ...)' guard).  This module pins the polyfills themselves so they
;; cannot regress silently under the nelisp driver.

;;; Code:

(require 'ert)
(require 'emacs-textmodes-stub)

;;;; word-wrap (pure-string helper) ------------------------------------

(ert-deftest emacs-textmodes-test/word-wrap-basic ()
  (should (string= "hello\nworld\nfoo\nbar"
                   (emacs-textmodes--word-wrap "hello world foo bar" 5))))

(ert-deftest emacs-textmodes-test/word-wrap-greedy-pack ()
  (should (string= "this is\na test"
                   (emacs-textmodes--word-wrap "this is a test" 7))))

(ert-deftest emacs-textmodes-test/word-wrap-single-token-larger-than-width ()
  "A token longer than WIDTH gets its own line (= no mid-token break)."
  (should (string= "longwordhere\na b"
                   (emacs-textmodes--word-wrap "longwordhere a b" 5))))

(ert-deftest emacs-textmodes-test/word-wrap-collapses-runs ()
  "Multiple whitespace + embedded newlines fold to a single token gap."
  (should (string= "multi space"
                   (emacs-textmodes--word-wrap "  multi   space  " 80)))
  (should (string= "a b c"
                   (emacs-textmodes--word-wrap "a\n\n  b\tc" 80))))

(ert-deftest emacs-textmodes-test/word-wrap-empty ()
  (should (string= "" (emacs-textmodes--word-wrap "" 80)))
  (should (string= "" (emacs-textmodes--word-wrap "   \n\t" 80))))

;;;; fill-region (buffer-side, prefixed entry point) -------------------

(ert-deftest emacs-textmodes-test/paragraph-defaults-are-available ()
  (should (boundp 'paragraph-start))
  (should (boundp 'paragraph-separate))
  (should (string= "\f\\|[ \t]*$" paragraph-start))
  (should (string= "[ \t\f]*$" paragraph-separate)))

(ert-deftest emacs-textmodes-test/org-activation-standard-vars-defaults ()
  "Standard text/editing variables used by Org activation are available."
  (dolist (cell '((tab-width . 8)
                  (fill-column . 70)
                  (indent-tabs-mode . t)
                  (left-margin . 0)
                  (fill-prefix . nil)
                  (truncate-lines . nil)
                  (word-wrap . nil)
                  (case-fold-search . t)
                  (selective-display . nil)
                  (cursor-type . t)
                  (align-mode-rules-list . nil)
                  (calc-embedded-open-mode . nil)
                  (buffer-face-mode-face . nil)
                  (pcomplete-command-completion-function . nil)
                  (pcomplete-command-name-function . nil)
                  (pcomplete-default-completion-function . nil)
                  (pcomplete-parse-arguments-function . nil)
                  (pcomplete-termination-string . nil)
                  (auto-fill-function . nil)
                  (normal-auto-fill-function . do-auto-fill)
                  (filter-buffer-substring-functions . nil)
                  (filter-buffer-substring-function . buffer-substring--filter)
                  (fill-nobreak-predicate . nil)
                  (paragraph-start . "\f\\|[ \t]*$")
                  (paragraph-separate . "[ \t\f]*$")
                  (sentence-end-double-space . t)
                  (sentence-end-without-period . nil)
                  (sentence-end-without-space . "。．？！")
                  (sentence-end . nil)
                  (sentence-end-base . "[.?!…‽][]\"'”’)}»›]*")
                  (page-delimiter . "^\014")
                  (paragraph-ignore-fill-prefix . nil)
                  (colon-double-space . nil)
                  (adaptive-fill-mode . t)
                  (adaptive-fill-regexp . "[-–!|#%;>*·•‣⁃◦ \t]*")
                  (adaptive-fill-first-line-regexp . "\\`[ \t]*\\'")
                  (adaptive-fill-function . ignore)
                  (fill-paragraph-function . nil)
                  (fill-paragraph-handle-comment . t)
                  (fill-forward-paragraph-function . forward-paragraph)
                  (auto-fill-inhibit-regexp . nil)
                  (comment-use-syntax . undecided)
                  (comment-fill-column . nil)
                  (comment-column . 32)
                  (comment-start . nil)
                  (comment-start-skip . nil)
                  (comment-end-skip . nil)
                  (comment-end . "")
                  (comment-indent-function . comment-indent-default)
                  (comment-insert-comment-function . nil)
                  (comment-region-function . comment-region-default)
                  (uncomment-region-function . uncomment-region-default)
                  (comment-continue . nil)
                  (comment-add . 0)
                  (comment-style . indent)
                  (comment-padding . " ")
                  (comment-inline-offset . 1)
                  (comment-multi-line . nil)
                  (comment-empty-lines . nil)
                  (comment-line-break-function . comment-indent-new-line)))
      (should (boundp (car cell)))
      (should (equal (default-value (car cell)) (cdr cell)))))

(ert-deftest emacs-textmodes-test/filter-buffer-substring-defaults ()
  "The bootstrap textmodes substrate exposes simple.el substring filtering."
  (should (boundp 'filter-buffer-substring-function))
  (should (fboundp 'filter-buffer-substring))
  (should (fboundp 'buffer-substring--filter))
  (should (fboundp 'delete-and-extract-region))
  (with-temp-buffer
    (insert "abcdef")
    (should (equal "bcd" (filter-buffer-substring 2 5)))
    (should (equal "abcdef" (buffer-string)))
    (should (equal "bc" (filter-buffer-substring 2 4 t)))
    (should (equal "adef" (buffer-string)))))

(ert-deftest emacs-textmodes-test/paragraph-defaults-register-locality-in-source ()
  (let* ((file (locate-library "emacs-textmodes-stub"))
         (file (if (and file (string-match-p "\\.elc\\'" file))
                   (substring file 0 -1)
                 file)))
    (should (and file (file-exists-p file)))
    (with-temp-buffer
      (insert-file-contents file)
      (dolist (needle '("(defvar paragraph-start"
                        "(defvar paragraph-separate"
                        "(make-variable-buffer-local sym)"
                        "tab-width fill-column indent-tabs-mode"
                        "paragraph-start"
                        "paragraph-separate"))
        (goto-char (point-min))
        (should (search-forward needle nil t))))))

(ert-deftest emacs-textmodes-test/fill-region-shrinks-to-column ()
  (with-temp-buffer
    (insert "hello world foo bar baz")
    (let ((fill-column 7))
      (emacs-textmodes-fill-region (point-min) (point-max)))
    (should (string= "hello\nworld\nfoo bar\nbaz" (buffer-string)))))

(ert-deftest emacs-textmodes-test/fill-region-leaves-short-alone ()
  (with-temp-buffer
    (insert "short")
    (let ((fill-column 80))
      (emacs-textmodes-fill-region (point-min) (point-max)))
    (should (string= "short" (buffer-string)))))

;;;; count-matches (buffer-side, prefixed entry point) -----------------

(ert-deftest emacs-textmodes-test/count-matches-basic ()
  (with-temp-buffer
    (insert "banana")
    (should (= 3 (emacs-textmodes-count-matches "a")))))

(ert-deftest emacs-textmodes-test/count-matches-non-overlap ()
  "Matches advance past the previous match end (= non-overlapping)."
  (with-temp-buffer
    (insert "abracadabra")
    (should (= 2 (emacs-textmodes-count-matches "ab")))))

(ert-deftest emacs-textmodes-test/count-matches-no-match ()
  (with-temp-buffer
    (insert "banana")
    (should (= 0 (emacs-textmodes-count-matches "z")))))

(ert-deftest emacs-textmodes-test/count-matches-bounded ()
  "RSTART / REND limit the scan range."
  (with-temp-buffer
    (insert "aaaa")
    (should (= 2 (emacs-textmodes-count-matches "a" 1 3)))))

(provide 'emacs-textmodes-stub-test)

;;; emacs-textmodes-stub-test.el ends here
