;;; emacs-tier3-facades-test.el --- tests for Tier 3 subsystem facades  -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)

(defconst emacs-tier3-facades-test--source
  (expand-file-name
   "../src/emacs-tier3-facades.el"
   (file-name-directory (or load-file-name buffer-file-name))))

(defconst emacs-tier3-facades-test--src-dir
  (file-name-directory emacs-tier3-facades-test--source))

(defconst emacs-tier3-facades-test--features
  '(emacs-tier3-facades widget calc gnus info treesit nxml nxml-mode url vc)
  "Features provided by the Tier 3 facade file.")

(defconst emacs-tier3-facades-test--loaders
  '((widget . "widget")
    (calc . "calc")
    (gnus . "gnus")
    (info . "info")
    (treesit . "treesit")
    (nxml . "nxml")
    (nxml-mode . "nxml-mode")
    (url . "url")
    (vc . "vc"))
  "Tier 3 facade feature loaders.")

(defconst emacs-tier3-facades-test--entrypoints
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
  "Principal Tier 3 facade entrypoints.")

(defmacro emacs-tier3-facades-test--with-clean-state (&rest body)
  "Run BODY after temporarily clearing Tier 3 facade feature/function cells."
  (declare (indent 0) (debug t))
  `(let ((original-features features)
         (function-cells
          (mapcar (lambda (symbol)
                    (cons symbol
                          (and (fboundp symbol) (symbol-function symbol))))
                  emacs-tier3-facades-test--entrypoints)))
     (unwind-protect
         (progn
           (dolist (feature emacs-tier3-facades-test--features)
             (setq features (remove feature features)))
           (dolist (symbol emacs-tier3-facades-test--entrypoints)
             (when (fboundp symbol)
               (fmakunbound symbol)))
           ,@body)
       (setq features original-features)
       (dolist (cell function-cells)
         (if (cdr cell)
             (fset (car cell) (cdr cell))
           (when (fboundp (car cell))
             (fmakunbound (car cell))))))))

(defmacro emacs-tier3-facades-test--with-clean-facade (&rest body)
  "Load the Tier 3 facade source, then run BODY in a temporary clean state."
  (declare (indent 0) (debug t))
  `(emacs-tier3-facades-test--with-clean-state
     (load emacs-tier3-facades-test--source nil t)
     ,@body))

(ert-deftest emacs-tier3-facades-test/feature-loaders-resolve-to-src ()
  (let ((load-path (cons emacs-tier3-facades-test--src-dir load-path)))
    (dolist (loader emacs-tier3-facades-test--loaders)
      (let ((expected (expand-file-name (concat (cdr loader) ".el")
                                        emacs-tier3-facades-test--src-dir))
            (actual (locate-library (cdr loader))))
        (should actual)
        (should (string= (file-name-sans-extension
                          (file-truename expected))
                         (file-name-sans-extension
                          (file-truename actual))))))))

(ert-deftest emacs-tier3-facades-test/feature-loaders-require-facade ()
  (dolist (loader emacs-tier3-facades-test--loaders)
    (emacs-tier3-facades-test--with-clean-state
      (let ((load-path (cons emacs-tier3-facades-test--src-dir load-path)))
        (require (car loader))
        (should (featurep (car loader)))
        (should (featurep 'emacs-tier3-facades))
        (should (fboundp 'widget-create))
        (should (fboundp 'calc))
        (should (fboundp 'gnus))
        (should (fboundp 'info))
        (should (fboundp 'treesit-available-p))
        (should (fboundp 'nxml-mode))
        (should (fboundp 'url-retrieve))
        (should (fboundp 'vc-next-action))))))

(ert-deftest emacs-tier3-facades-test/provides-tier3-features ()
  (emacs-tier3-facades-test--with-clean-facade
    (dolist (feature emacs-tier3-facades-test--features)
      (should (featurep feature)))))

(ert-deftest emacs-tier3-facades-test/principal-entrypoints-are-fboundp ()
  (emacs-tier3-facades-test--with-clean-facade
    (dolist (symbol emacs-tier3-facades-test--entrypoints)
      (should (fboundp symbol)))))

(ert-deftest emacs-tier3-facades-test/noop-predicates-return-nil ()
  (emacs-tier3-facades-test--with-clean-facade
    (should-not (widgetp '(widget)))
    (should-not (widget-setup))
    (should-not (Info-mode))
    (should-not (treesit-available-p))
    (should-not (treesit-ready-p 'python))
    (should-not (treesit-language-available-p 'python))
    (should-not (treesit-parser-list))
    (should-not (treesit-node-at 1))
    (should-not (treesit-buffer-root-node))
    (should-not (treesit-node-type nil))
    (should-not (treesit-node-start nil))
    (should-not (treesit-node-end nil))
    (should-not (nxml-mode))
    (should-not (url-generic-parse-url "https://example.invalid"))
    (should-not (url-host nil))
    (should-not (url-port nil))
    (should-not (url-filename nil))
    (should-not (url-type nil))
    (let ((root (make-temp-file "emacs-tier3-novc-" t)))
      (unwind-protect
          (progn
            (should-not (vc-responsible-backend root))
            (should-not (vc-backend root)))
        (when (file-directory-p root)
          (delete-directory root t))))))

(ert-deftest emacs-tier3-facades-test/vc-backend-detects-vc-markers ()
  (emacs-tier3-facades-test--with-clean-facade
    (dolist (case '((".git" . Git)
                    (".hg" . Hg)
                    (".svn" . SVN)))
      (let* ((root (make-temp-file "emacs-tier3-vc-" t))
             (nested (expand-file-name "src/lib" root))
             (file (expand-file-name "feature.el" nested)))
        (unwind-protect
            (progn
              (make-directory nested t)
              (make-directory (expand-file-name (car case) root))
              (with-temp-file file
                (insert ";; feature\n"))
              (should (eq (cdr case) (vc-responsible-backend nested)))
              (should (eq (cdr case) (vc-backend file))))
          (when (file-directory-p root)
            (delete-directory root t)))))))

(ert-deftest emacs-tier3-facades-test/unsupported-entrypoints-signal-clearly ()
  (emacs-tier3-facades-test--with-clean-facade
    (dolist (call '((widget-create 'push-button)
                    (calc)
                    (calc-do-quick-calc "1+1")
                    (gnus)
                    (info)
                    (treesit-parser-create 'python)
                    (treesit-query-compile 'python "(_) @node")
                    (nxml-validate)
                    (url-retrieve "https://example.invalid" #'ignore)
                    (vc-next-action)))
      (should-error (eval call t)
                    :type 'emacs-tier3-facade-unsupported))))

(provide 'emacs-tier3-facades-test)

;;; emacs-tier3-facades-test.el ends here
