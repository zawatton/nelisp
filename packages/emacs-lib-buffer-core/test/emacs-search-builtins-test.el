;;; emacs-search-builtins-test.el --- ERT tests for emacs-search-builtins  -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for the Layer 2 Emacs search/match builtin bridge.  Under
;; batch host Emacs the host C builtins remain active, so the
;; substrate-direct `nelisp-ec-*' API is used for most assertions and a
;; couple of synthetic helpers exercise the polyfill bodies directly.

;;; Code:

(require 'ert)
(require 'emacs-search-builtins)
(require 'cl-lib)

(defmacro emacs-search-builtins-test--with-fresh-world (&rest body)
  "Run BODY with a clean NeLisp buffer registry/current-buffer state."
  (declare (indent 0) (debug (body)))
  `(let ((nelisp-ec--buffers nil)
         (nelisp-ec--current-buffer nil)
         (nelisp-ec--match-data nil))
     ,@body))

(defmacro emacs-search-builtins-test--with-temp-buffer-polyfill (&rest body)
  "Mirror a temporary-buffer style substrate setup for host-safe checks."
  (declare (indent 0) (debug (body)))
  (let ((buf (make-symbol "buf")))
    (list 'let (list (list buf (list 'nelisp-ec-generate-new-buffer
                                     " *temp*")))
          (list 'unwind-protect
                (cons 'nelisp-ec-with-current-buffer (cons buf body))
                (list 'nelisp-ec-kill-buffer buf)))))

;;;; A. Load cleanly

(ert-deftest emacs-search-builtins-test/require-loads-cleanly ()
  (should (featurep 'emacs-search-builtins))
  (dolist (sym '(string-match string-match-p replace-match
                 replace-regexp-in-string re-search-forward
                 re-search-backward search-forward search-backward
                 looking-at looking-at-p match-data match-beginning
                 match-end match-string match-string-no-properties))
    (should (fboundp sym))))

;;;; B. Literal search moves point and returns the new position

(ert-deftest emacs-search-builtins-test/literal-search-forward-returns-new-point ()
  (emacs-search-builtins-test--with-fresh-world
    (let ((buf (nelisp-ec-generate-new-buffer "search")))
      (nelisp-ec-with-current-buffer buf
        (nelisp-ec-insert "alpha beta gamma")
        (nelisp-ec-goto-char (nelisp-ec-point-min))
        (should (= 11 (nelisp-ec-search-forward "beta")))
        (should (= 11 (nelisp-ec-point)))))))

;;;; C. Regex search populates match data accessors

(ert-deftest emacs-search-builtins-test/regex-search-sets-match-data-accessors ()
  (emacs-search-builtins-test--with-fresh-world
    (let ((buf (nelisp-ec-generate-new-buffer "regex")))
      (nelisp-ec-with-current-buffer buf
        (nelisp-ec-insert "alpha beta gamma")
        (nelisp-ec-goto-char (nelisp-ec-point-min))
        (should (= 11 (nelisp-ec-re-search-forward "beta")))
        (should (integerp (nelisp-ec-match-beginning 0)))
        (should (integerp (nelisp-ec-match-end 0)))
        (should (= 7 (nelisp-ec-match-beginning 0)))
        (should (= 11 (nelisp-ec-match-end 0)))))))

;;;; D. looking-at truthiness

(ert-deftest emacs-search-builtins-test/looking-at-returns-truthy-at-match-and-nil-elsewhere ()
  (emacs-search-builtins-test--with-fresh-world
    (let ((buf (nelisp-ec-generate-new-buffer "looking")))
      (nelisp-ec-with-current-buffer buf
        (nelisp-ec-insert "beta gamma")
        (nelisp-ec-goto-char (nelisp-ec-point-min))
        (should (nelisp-ec-looking-at "beta"))
        (should (= 1 (nelisp-ec-match-beginning 0)))
        (should (= 5 (nelisp-ec-match-end 0)))
        (nelisp-ec-goto-char 6)
        (should-not (nelisp-ec-looking-at "beta"))))))

;;;; E. match-data shape and buffer-path match-string polyfill body

(ert-deftest emacs-search-builtins-test/match-data-returns-list-and-buffer-path-match-string-works ()
  (emacs-search-builtins-test--with-fresh-world
    (let ((buf (nelisp-ec-generate-new-buffer "match-data")))
      (nelisp-ec-with-current-buffer buf
        (nelisp-ec-insert "alpha beta gamma")
        (nelisp-ec-goto-char (nelisp-ec-point-min))
        (should (= 11 (nelisp-ec-re-search-forward "beta")))
        (should (equal '(7 11) (nelisp-ec-match-data)))
        (cl-letf (((symbol-function 'buffer-substring)
                   #'nelisp-ec-buffer-substring)
                  ((symbol-function 'emacs-search-builtins-test--match-string)
                   (lambda (num &optional string)
                     (let ((b (nelisp-ec-match-beginning num))
                           (e (nelisp-ec-match-end num)))
                       (when (and (integerp b) (integerp e))
                         (cond
                          ((stringp string)
                           (substring string b e))
                          (t
                           (buffer-substring b e))))))))
          (should (equal "beta"
                         (emacs-search-builtins-test--match-string 0))))))))

;;;; F. Host match-string STRING path smoke test

(ert-deftest emacs-search-builtins-test/match-string-with-string-argument-uses-host-path ()
  (let ((source "alpha beta gamma"))
    (should (= 6 (string-match "b\\(et\\)a" source)))
    (should (equal "et" (match-string 1 source)))
    (should (equal "beta" (match-string 0 source)))))

;;;; G. Host builtin re-search-forward remains bound and numeric

(ert-deftest emacs-search-builtins-test/host-re-search-forward-is-bound-and-returns-a-number ()
  (with-temp-buffer
    (insert "x")
    (goto-char (point-min))
    (should (fboundp 're-search-forward))
    (should (numberp (re-search-forward "x")))
    (should (= 2 (point)))))

;;;; H. Backward search retreats and NOERROR suppresses misses

(ert-deftest emacs-search-builtins-test/search-backward-retreats-and-honors-noerror ()
  (emacs-search-builtins-test--with-fresh-world
    (let ((buf (nelisp-ec-generate-new-buffer "backward")))
      (nelisp-ec-with-current-buffer buf
        (nelisp-ec-insert "abc def abc")
        (nelisp-ec-goto-char (nelisp-ec-point-max))
        (should (= 9 (nelisp-ec-search-backward "abc")))
        (should (= 9 (nelisp-ec-point)))
        (should-not (nelisp-ec-search-backward "zzz" nil t))
        (should (= 9 (nelisp-ec-point)))))))

;;;; I. Requiring the module again leaves the function cells unchanged

(ert-deftest emacs-search-builtins-test/require-is-idempotent ()
  (let ((before-re-search-forward (symbol-function 're-search-forward))
        (before-string-match (symbol-function 'string-match))
        (before-replace-regexp (symbol-function 'replace-regexp-in-string))
        (before-match-data (symbol-function 'match-data))
        (before-match-string (symbol-function 'match-string)))
    (require 'emacs-search-builtins)
    (should (eq before-re-search-forward (symbol-function 're-search-forward)))
    (should (eq before-string-match (symbol-function 'string-match)))
    (should (eq before-replace-regexp
                (symbol-function 'replace-regexp-in-string)))
    (should (eq before-match-data (symbol-function 'match-data)))
    (should (eq before-match-string (symbol-function 'match-string)))))

(ert-deftest emacs-search-builtins-test/bridge-overwrites-standalone-stubs-in-source ()
  (should (fboundp 'emacs-search-builtins--install-function-p))
  (should-not (emacs-search-builtins--install-function-p 're-search-forward))
  (let* ((file (locate-library "emacs-search-builtins"))
         (file (if (and file (string-match-p "\\.elc\\'" file))
                   (concat (substring file 0 (- (length file) 1)))
                 file)))
    (should (and file (file-exists-p file)))
    (with-temp-buffer
      (insert-file-contents file)
      (dolist (sym '(string-match string-match-p replace-match
                     replace-regexp-in-string re-search-forward
                     re-search-backward search-forward search-backward
                     looking-at looking-at-p match-data match-beginning
                     match-end match-string match-string-no-properties))
        (goto-char (point-min))
        (should (search-forward
                 (format "(when (emacs-search-builtins--install-function-p '%s)"
                         sym)
                 nil t))))))

;;;; J. Synthetic polyfill count loop honors COUNT

(ert-deftest emacs-search-builtins-test/polyfill-count-loop-honors-count ()
  (let ((call-count 0))
    (cl-letf (((symbol-function 'nelisp-ec-re-search-forward)
               (lambda (&rest _args)
                 (setq call-count (1+ call-count))
                 call-count))
              ((symbol-function 'emacs-search-builtins-test--re-search)
               (lambda (regexp &optional bound noerror count)
                 (let ((c (or count 1))
                       (last nil))
                   (while (and (> c 0)
                               (setq last (nelisp-ec-re-search-forward regexp
                                                                       bound
                                                                       noerror)))
                     (setq c (1- c)))
                   last))))
      (should (= 4 (emacs-search-builtins-test--re-search "x" nil nil 4)))
      (should (= 4 call-count)))))

(provide 'emacs-search-builtins-test)

;;; emacs-search-builtins-test.el ends here
