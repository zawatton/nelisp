;;; calc-ext-test.el --- ERT for the Calc extension facade  -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)

(defconst calc-ext-test--src-dir
  (expand-file-name
   "../src"
   (file-name-directory (or load-file-name buffer-file-name))))

(defconst calc-ext-test--tier3-features
  '(emacs-tier3-facades calc gnus info treesit nxml nxml-mode vc)
  "Tier 3 features that a host `calc' load may install.")

(defconst calc-ext-test--tier3-entrypoints
  '(widget-create widget-insert widget-apply widget-value widget-get
    widget-put widget-convert widgetp widget-setup
    calc full-calc quick-calc calc-do-quick-calc calc-eval calc-dispatch
    gnus gnus-no-server gnus-group-read-group gnus-summary-read-group
    gnus-summary-show-thread
    info Info-goto-node Info-find-node Info-directory Info-mode
    info-lookup-symbol
    treesit-available-p treesit-ready-p treesit-language-available-p
    treesit-parser-list treesit-parser-create treesit-node-at
    treesit-buffer-root-node treesit-query-compile treesit-query-capture
    treesit-node-type treesit-node-start treesit-node-end
    nxml-mode nxml-validate nxml-complete nxml-scan-prolog
    nxml-balanced-close-start-tag-block
    url-retrieve url-retrieve-synchronously url-copy-file
    url-insert-file-contents url-generic-parse-url url-host url-port
    url-filename url-type
    vc-next-action vc-dir vc-print-log vc-diff vc-status vc-register
    vc-responsible-backend vc-backend)
  "Tier 3 entrypoints whose cells must survive this test's load.")

(defun calc-ext-test--with-clean-tier3 (thunk)
  "Call THUNK while restoring host Tier 3 facade state afterward.

Requiring host `calc' loads the broad facade bundle.  The Calc extension test
must not leave those feature and function cells installed for vendor-first
tests that run later in the same Emacs process.

This is a function rather than a macro so the test remains byte-compilable
before all ERT and CL macro helpers have been loaded."
  (let ((original-features features)
        (function-cells
         (mapcar (lambda (symbol)
                   (cons symbol
                         (and (fboundp symbol) (symbol-function symbol))))
                 calc-ext-test--tier3-entrypoints)))
    (unwind-protect
        (progn
          (dolist (feature calc-ext-test--tier3-features)
            (setq features (remove feature features)))
          (dolist (symbol calc-ext-test--tier3-entrypoints)
            (when (fboundp symbol)
              (fmakunbound symbol)))
          (funcall thunk))
      (setq features original-features)
      (dolist (cell function-cells)
        (if (cdr cell)
            (fset (car cell) (cdr cell))
          (when (fboundp (car cell))
            (fmakunbound (car cell))))))))

(ert-deftest calc-ext-test/facade-loads-with-minimal-calc ()
  "Requiring `calc-ext' must keep the minimal Calc contract loadable.

The vendor extension assumes variables from the full GNU Calc package.  This
test follows the normal source load path and proves that the companion facade
is selected instead, while its supported scalar bindings remain usable."
  (calc-ext-test--with-clean-tier3
   (lambda ()
     (let ((load-path (cons calc-ext-test--src-dir load-path)))
       (should
        (string=
         (file-name-sans-extension
          (file-truename (expand-file-name "calc-ext.el" calc-ext-test--src-dir)))
         (file-name-sans-extension (file-truename (locate-library "calc-ext")))))
       (require 'calc)
       (require 'calc-ext)
       (should (featurep 'calc))
       (should (featurep 'calc-ext))
       (should (boundp 'calc-mode-map))
       (should (keymapp calc-mode-map))
       (should (fboundp 'calc-init-extensions))
       (should (fboundp 'calc-shift-prefix))
       (should (fboundp 'calc-init-prefixes))
       (should-not (calc-init-extensions))
       (should (eq 'calc-plus (lookup-key calc-mode-map "+")))
       (should (eq 'calc-divide (lookup-key calc-mode-map "/")))
       (should (eq 'calc-shift-prefix (lookup-key calc-mode-map "mS")))
       (should-error (calc-shift-prefix) :type 'emacs-tier3-facade-unsupported)))))

(ert-deftest calc-ext-test/scalar-integer-compatibility-slice ()
  "Keep the scalar helpers used by UUID generators functional."
  (calc-ext-test--with-clean-tier3
   (lambda ()
     (let ((load-path (cons calc-ext-test--src-dir load-path)))
       (require 'calc-ext)
       (should (= #x1b21dd213814000 (math-read-radix "1b21dd213814000" 16)))
       (should-not (math-read-radix "12z" 16))
       (should (= 42 (math-add 40 2)))
       (should (= 84 (math-mul 42 2)))
       (should (= 1024 (math-power-of-2 10)))
       (should (= #xff (math-clip #x1ff 8)))
       (should (equal '(12 . 3) (math-idivmod 123 10)))
       (should (= 42 (math-fixnum 42)))
       (let ((random-value (calcFunc-random 100)))
         (should (integerp random-value))
         (should (>= random-value 0))
         (should (< random-value 100)))))))

(provide 'calc-ext-test)

;;; calc-ext-test.el ends here
