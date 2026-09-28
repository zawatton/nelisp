;;; nelisp-vendor-source-test.el --- Tests for pinned GNU source forms -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;;; Commentary:

;; Keep the selector's static alias handling and evaluator installation covered.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'nelisp-eval)
(require 'nelisp-vendor-source)

(ert-deftest nelisp-vendor-source-selects-only-the-requested-defalias ()
  (let* ((forms (nelisp-read-all
                 (nelisp-vendor-source-forms
                  "vendor/staged-emacs-lisp/subr.el"
                  '(string= string< string>))))
         (names (mapcar (lambda (form) (cadr (cadr form))) forms)))
    (should (equal names '(string= string< string>)))
    (should (equal (string-trim
                    (nelisp-vendor-source-form
                     "vendor/staged-emacs-lisp/subr.el" 'string>))
                   "(defalias 'string> #'string-greaterp)"))))

(ert-deftest nelisp-vendor-source-selects-cl-defstruct-forms-by-name ()
  (let ((base (nelisp-vendor-source-form
               "vendor/staged-emacs-lisp/cl-preloaded.el" 'cl--class))
        (derived (nelisp-vendor-source-form
                  "vendor/staged-emacs-lisp/cl-preloaded.el"
                  'cl-derived-type-class)))
    (should (string-match-p "^(cl-defstruct[ \n]+(cl--class\\_>" base))
    (should (string-match-p
             "^(cl-defstruct[ \n]+(cl-derived-type-class\\_>" derived))
    (should (< (nelisp-vendor-source-form-position
                "vendor/staged-emacs-lisp/cl-preloaded.el" 'cl--class)
               (nelisp-vendor-source-form-position
                "vendor/staged-emacs-lisp/cl-preloaded.el"
                'cl-derived-type-class)))))

(ert-deftest nelisp-vendor-source-selects-gnu-defface-macro ()
  (let ((form (nelisp-vendor-source-form
               "vendor/staged-emacs-lisp/custom.el" 'defface)))
    (should (string-match-p "^(defmacro defface\\_>" form))
    (should (string-match-p "custom-declare-face" form))))

(ert-deftest nelisp-vendor-source-selects-menu-bar-preload-assignment ()
  (should (equal (string-trim
                  (nelisp-vendor-source-form
                   "vendor/emacs-lisp/menu-bar.el" 'menu-bar-final-items))
                 "(setq menu-bar-final-items '(help-menu))")))

(ert-deftest nelisp-vendor-source-selects-custom-handle-all-keywords ()
  (let ((form (nelisp-vendor-source-form
               "vendor/staged-emacs-lisp/custom.el"
               'custom-handle-all-keywords)))
    (should (string-match-p "^(defun custom-handle-all-keywords\\_>" form))
    (should (string-match-p "custom-handle-keyword" form))))

(ert-deftest nelisp-vendor-source-selects-custom-version-metadata-helpers ()
  (dolist (function '(custom-add-version custom-add-package-version))
    (let ((form (nelisp-vendor-source-form
                 "vendor/staged-emacs-lisp/custom.el" function)))
      (should (string-match-p (format "^(defun %s\\_>" function) form)))))

(ert-deftest nelisp-vendor-source-selects-keymap-read-only-bind-helpers ()
  (let ((forms (nelisp-read-all
                (nelisp-vendor-source-forms
                 "vendor/staged-emacs-lisp/keymap.el"
                 '(keymap--read-only-filter keymap-read-only-bind)))))
    (should (equal (mapcar (lambda (form) (cadr form)) forms)
                   '(keymap--read-only-filter keymap-read-only-bind)))))

(ert-deftest nelisp-vendor-source-selects-button-type-substrate ()
  (let ((forms (nelisp-read-all
                (nelisp-vendor-source-forms
                 "vendor/emacs-lisp/button.el"
                 '(button-category-symbol define-button-type)))))
    (should (equal (mapcar (lambda (form) (cadr form)) forms)
                   '(button-category-symbol define-button-type)))))

(ert-deftest nelisp-vendor-source-selects-ansi-osc-provider-form ()
  (let ((form (nelisp-vendor-source-form
               "vendor/emacs-lisp/ansi-osc.el" 'ansi-osc-apply-on-region)))
    (should (string-match-p "^(defun ansi-osc-apply-on-region\\_>" form))))

(ert-deftest nelisp-vendor-source-selects-regexp-opt-provider-form ()
  (let ((form (nelisp-vendor-source-form
               "vendor/emacs-lisp/emacs-lisp/regexp-opt.el" 'regexp-opt)))
    (should (string-match-p "^(defun regexp-opt\\_>" form))))

(ert-deftest nelisp-vendor-source-selects-custom-declare-face ()
  (let ((form (nelisp-vendor-source-form
               "vendor/staged-emacs-lisp/cus-face.el"
               'custom-declare-face)))
    (should (string-match-p "^(defun custom-declare-face\\_>" form))
    (should (string-match-p "face-spec-set" form))
    (should (string-match-p "custom-handle-all-keywords" form))))

(ert-deftest nelisp-vendor-source-selects-face-spec-set ()
  (let ((form (nelisp-vendor-source-form
               "vendor/staged-emacs-lisp/faces.el" 'face-spec-set)))
    (should (string-match-p "^(defun face-spec-set\\_>" form))
    (should (string-match-p "face-spec-recalc" form))))

(ert-deftest nelisp-vendor-source-selects-make-empty-face ()
  (let ((form (nelisp-vendor-source-form
               "vendor/staged-emacs-lisp/faces.el" 'make-empty-face)))
    (should (string-match-p "^(defun make-empty-face\\_>" form))
    (should (string-match-p "(make-face face)" form))))

(ert-deftest nelisp-vendor-source-selects-facep ()
  (let ((form (nelisp-vendor-source-form
               "vendor/staged-emacs-lisp/faces.el" 'facep)))
    (should (string-match-p "^(defun facep\\_>" form))
    (should (string-match-p "internal-lisp-face-p" form))))

(ert-deftest nelisp-vendor-source-selects-set-face-documentation ()
  (let ((form (nelisp-vendor-source-form
               "vendor/staged-emacs-lisp/faces.el" 'set-face-documentation)))
    (should (string-match-p "^(defun set-face-documentation\\_>" form))
    (should (string-match-p "face-documentation" form))))

(ert-deftest nelisp-vendor-source-installs-string-comparison-forms ()
  (nelisp--reset)
  (should (eq (nelisp-eval '(string-equal-ignore-case "AbC" "aBc")) t))
  (should-not (nelisp-eval '(string-equal-ignore-case "AbC" "abcx")))
  (should (eq (nelisp-eval '(string-greaterp 'zeta 'alpha)) t))
  (should (eq (nelisp-eval '(string= "same" "same")) t))
  (should (eq (nelisp-eval '(string< "a" "b")) t))
  (should (eq (nelisp-eval '(string> "b" "a")) t)))

(ert-deftest nelisp-vendor-source-installs-alist-get ()
  (nelisp--reset)
  (should (equal '(missing 7)
                 (nelisp-eval
                  '(let ((key (copy-sequence "k"))
                         (alist (list (cons "k" 7))))
                     (list (alist-get key alist 'missing)
                           (alist-get key alist 'missing nil #'equal))))))
  (nelisp--reset))

(ert-deftest nelisp-vendor-source-installs-match-string-no-properties ()
  (nelisp--reset)
  (should (equal (nelisp-eval
                  '(progn (string-match "\\(ab\\)" "zab")
                          (match-string-no-properties 1 "zab")))
                 "ab"))
  (nelisp--reset))

(ert-deftest nelisp-vendor-source-installs-simple-string-empty-p ()
  (nelisp--reset)
  (should (eq (nelisp-eval '(string-empty-p "")) t))
  (should-not (nelisp-eval '(string-empty-p "x")))
  (should-not (nelisp-eval '(string-empty-p nil)))
  (should-error (nelisp-eval '(string-empty-p 12354))
                :type 'wrong-type-argument))

(ert-deftest nelisp-vendor-source-installs-file-attribute-accessors ()
  (nelisp--reset)
  (let ((attributes '(nil 1 2 3 4 5 6 7 "" nil 10 11)))
    (dolist (name '(file-attribute-size
                    file-attribute-modification-time
                    file-attribute-file-identifier))
      (should (equal (nelisp-eval (list name (list 'quote attributes)))
                     (funcall name attributes)))))
  (let ((source
         (nelisp-vendor-source-forms
          "vendor/staged-emacs-lisp/files.el"
          '(file-attribute-size file-attribute-modification-time
            file-attribute-file-identifier))))
    (dolist (name '(file-attribute-size
                    file-attribute-modification-time
                    file-attribute-file-identifier))
      (should (string-match-p (format "(defsubst %s " name) source))))
  (nelisp--reset))

(ert-deftest nelisp-vendor-source-selects-mule-conf-password-custom-forms ()
  (let ((source
         (nelisp-vendor-source-forms
          "vendor/emacs-lisp/international/mule-conf.el"
          '(password-word-equivalents password-colon-equivalents))))
    (should (string-match-p "^(defcustom password-word-equivalents\\_>" source))
    (should (string-match-p "^(defcustom password-colon-equivalents\\_>" source))
    (should (string-match-p ":version \\\"27.1\\\"" source))
    (should (string-match-p ":group 'processes" source))))

(ert-deftest nelisp-vendor-source-rejects-mule-conf-pin-mismatch ()
  (let ((nelisp-vendor-source--cache (make-hash-table :test #'equal)))
    (cl-letf (((symbol-function 'secure-hash) (lambda (&rest _) "wrong-pin")))
      (should-error
       (nelisp-vendor-source-form
        "vendor/emacs-lisp/international/mule-conf.el"
        'password-word-equivalents)
       :type 'error))))

(provide 'nelisp-vendor-source-test)

;;; nelisp-vendor-source-test.el ends here
