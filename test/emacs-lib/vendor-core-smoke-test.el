;;; vendor-core-smoke-test.el --- tests for vendor core smoke helpers  -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)

(load (expand-file-name
       "../scripts/vendor-core-smoke.el"
       (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

(defun vendor-core-smoke-test--featurep-for (enabled)
  "Return a `featurep' stub that reports ENABLED as present."
  (lambda (feature &optional _subfeature)
    (memq feature enabled)))

(ert-deftest vendor-core-smoke-test/feature-loaded ()
  (should (featurep 'vendor-core-smoke))
  (dolist (sym '(vendor-core-smoke-batch
                 vendor-core-smoke--selected-modules
                 vendor-core-smoke--parse-module-symbols
                 vendor-core-smoke--check-files
                 vendor-core-smoke--check-simple
                 vendor-core-smoke--check-dired
                 vendor-core-smoke--check-help-mode
                 vendor-core-smoke--check-help-fns
                 vendor-core-smoke--check-subr-x
                 vendor-core-smoke--check-seq
                 vendor-core-smoke--check-map
                 vendor-core-smoke--check-lisp
                 vendor-core-smoke--check-case-table
                 vendor-core-smoke--check-cdl
                 vendor-core-smoke--check-range
                 vendor-core-smoke--check-regi
                 vendor-core-smoke--check-lisp-mode
                 vendor-core-smoke--check-ielm
                 vendor-core-smoke--check-isearch
                 vendor-core-smoke--check-minibuffer
                 vendor-core-smoke--check-project
                 vendor-core-smoke--check-hex-util
                 vendor-core-smoke--check-map-ynp
                 vendor-core-smoke--check-charprop
                 vendor-core-smoke--check-charscript
                 vendor-core-smoke--check-emoji-labels
                 vendor-core-smoke--check-iso-transl
                 vendor-core-smoke--check-cp51932
                 vendor-core-smoke--check-eucjp-ms
                 vendor-core-smoke--check-fontset
                 vendor-core-smoke--check-idna-mapping
                 vendor-core-smoke--check-ja-dic-utl))
    (should (fboundp sym))))

(ert-deftest vendor-core-smoke-test/default-selects-all-candidates ()
  (let ((vendor-core-smoke-modules nil)
        (vendor-core-smoke-default-limit 0)
        (vendor-core-smoke-candidates
         '((files . check-files)
           (simple . check-simple))))
    (cl-letf (((symbol-function 'getenv) (lambda (&rest _) nil)))
      (should (equal (vendor-core-smoke--selected-modules)
                     vendor-core-smoke-candidates)))))

(ert-deftest vendor-core-smoke-test/limit-zero-selects-all-candidates ()
  (let ((vendor-core-smoke-modules nil)
        (vendor-core-smoke-default-limit 1)
        (vendor-core-smoke-candidates
         '((files . check-files)
           (simple . check-simple)
           (dired . check-dired))))
    (cl-letf (((symbol-function 'getenv)
               (lambda (name)
                 (and (string= name "VENDOR_CORE_LIMIT") "0"))))
      (should (equal (vendor-core-smoke--selected-modules)
                     vendor-core-smoke-candidates)))))

(ert-deftest vendor-core-smoke-test/modules-env-selects-named-candidates ()
  (let ((vendor-core-smoke-modules nil)
        (vendor-core-smoke-module-spec nil)
        (vendor-core-smoke-default-limit 1)
        (vendor-core-smoke-candidates
         '((files . check-files)
           (simple . check-simple)
           (dired . check-dired))))
    (cl-letf (((symbol-function 'getenv)
               (lambda (name)
                 (cond
                  ((string= name "VENDOR_CORE_MODULES") "simple,dired")
                  ((string= name "VENDOR_CORE_LIMIT") "1")
                  (t nil)))))
      (should (equal (vendor-core-smoke--selected-modules)
                     '((simple . check-simple)
                       (dired . check-dired)))))))

(ert-deftest vendor-core-smoke-test/module-spec-overrides-env-and-limit ()
  (let ((vendor-core-smoke-modules nil)
        (vendor-core-smoke-module-spec "simple")
        (vendor-core-smoke-default-limit 1)
        (vendor-core-smoke-candidates
         '((files . check-files)
           (simple . check-simple))))
    (cl-letf (((symbol-function 'getenv)
               (lambda (name)
                 (cond
                  ((string= name "VENDOR_CORE_MODULES") "files")
                  ((string= name "VENDOR_CORE_LIMIT") "0")
                  (t nil)))))
      (should (equal (vendor-core-smoke--selected-modules)
                     '((simple . check-simple)))))))

(ert-deftest vendor-core-smoke-test/modules-env-accepts-whitespace ()
  (should (equal (vendor-core-smoke--parse-module-symbols
                  " files simple\tdired\nproject ")
                 '(files simple dired project))))

(ert-deftest vendor-core-smoke-test/modules-env-rejects-unknown ()
  (let ((vendor-core-smoke-modules nil)
        (vendor-core-smoke-module-spec nil)
        (vendor-core-smoke-candidates '((files . check-files))))
    (cl-letf (((symbol-function 'getenv)
               (lambda (name)
                 (and (string= name "VENDOR_CORE_MODULES") "missing"))))
      (should-error (vendor-core-smoke--selected-modules)))))

(ert-deftest vendor-core-smoke-test/explicit-modules-override-limit ()
  (let ((vendor-core-smoke-modules '((project . check-project)))
        (vendor-core-smoke-default-limit 0))
    (should (equal (vendor-core-smoke--selected-modules)
                   '((project . check-project))))))

(ert-deftest vendor-core-smoke-test/run-one-reports-pass-and-fail ()
  (cl-letf (((symbol-function 'vendor-core-smoke-test--pass)
             (lambda () 'ok))
            ((symbol-function 'vendor-core-smoke-test--fail)
             (lambda () (error "nope"))))
    (should (equal (vendor-core-smoke--run-one
                    '(files . vendor-core-smoke-test--pass))
                   '(files pass "")))
    (let ((result (vendor-core-smoke--run-one
                   '(simple . vendor-core-smoke-test--fail))))
      (should (equal (car result) 'simple))
      (should (eq (cadr result) 'fail))
      (should (string-match-p "nope" (caddr result))))))

(ert-deftest vendor-core-smoke-test/simple-check-stays-lightweight ()
  "The simple lane must not load the full `emacs-init' bootstrap."
  (let (required)
    (cl-letf (((symbol-function 'require)
               (lambda (feature &optional _filename _noerror)
                 (push feature required)
                 feature)))
      (should (eq 'ok (vendor-core-smoke--check-simple)))
      (should (memq 'simple required))
      (should-not (memq 'emacs-init required)))))

(ert-deftest vendor-core-smoke-test/files-check-stays-lightweight ()
  "The files lane must not load the full `emacs-init' bootstrap."
  (let (required)
    (cl-letf (((symbol-function 'require)
               (lambda (feature &optional _filename _noerror)
                 (push feature required)
                 feature)))
      (should (eq 'ok (vendor-core-smoke--check-files)))
      (should (memq 'files required))
      (should-not (memq 'emacs-init required)))))

(ert-deftest vendor-core-smoke-test/dired-check-stays-lightweight ()
  "The dired lane must not load the full `emacs-init' bootstrap."
  (let ((original-features features)
        required)
    (unwind-protect
        (progn
          (provide 'dired)
          (cl-letf (((symbol-function 'require)
                     (lambda (feature &optional _filename _noerror)
                       (push feature required)
                       feature))
                    ((symbol-function 'dired) (lambda (&rest _) 'ok))
                    ((symbol-function 'dired-mode) (lambda (&rest _) 'ok))
                    ((symbol-function 'dired-find-file) (lambda (&rest _) 'ok))
                    ((symbol-function 'dired-next-line) (lambda (&rest _) 'ok))
                    ((symbol-function 'dired-previous-line) (lambda (&rest _) 'ok))
                    ((symbol-function 'dired-up-directory) (lambda (&rest _) 'ok)))
            (should (eq 'ok (vendor-core-smoke--check-dired)))
            (should (memq 'dired required))
            (should-not (memq 'emacs-init required))))
      (setq features original-features))))

(ert-deftest vendor-core-smoke-test/help-mode-check-stays-lightweight ()
  "The help-mode lane must not load the full `emacs-init' bootstrap."
  (let (required)
    (cl-letf (((symbol-function 'require)
               (lambda (feature &optional _filename _noerror)
                 (push feature required)
                 feature))
              ((symbol-function 'featurep)
               (vendor-core-smoke-test--featurep-for '(help-mode)))
              ((symbol-function 'help-mode) (lambda (&rest _) 'ok))
              ((symbol-function 'help-go-back) (lambda (&rest _) 'ok))
              ((symbol-function 'help-go-forward) (lambda (&rest _) 'ok)))
      (should (eq 'ok (vendor-core-smoke--check-help-mode)))
      (should (memq 'help-mode required))
      (should-not (memq 'emacs-init required)))))

(ert-deftest vendor-core-smoke-test/help-fns-check-stays-lightweight ()
  "The help-fns lane must not load the full `emacs-init' bootstrap."
  (let (required)
    (cl-letf (((symbol-function 'require)
               (lambda (feature &optional _filename _noerror)
                 (push feature required)
                 feature))
              ((symbol-function 'featurep)
               (vendor-core-smoke-test--featurep-for '(help-fns)))
              ((symbol-function 'describe-function) (lambda (&rest _) 'ok))
              ((symbol-function 'describe-variable) (lambda (&rest _) 'ok))
              ((symbol-function 'describe-symbol) (lambda (&rest _) 'ok)))
      (should (eq 'ok (vendor-core-smoke--check-help-fns)))
      (should (memq 'help-fns required))
      (should-not (memq 'emacs-init required)))))

(ert-deftest vendor-core-smoke-test/subr-x-check-stays-lightweight ()
  "The subr-x lane must not load the full `emacs-init' bootstrap."
  (let ((original-features features)
        required)
    (unwind-protect
        (progn
          (provide 'subr-x)
          (cl-letf (((symbol-function 'require)
                     (lambda (feature &optional _filename _noerror)
                       (push feature required)
                       feature))
                    ((symbol-function 'thread-first) (lambda (&rest _) 'ok))
                    ((symbol-function 'thread-last) (lambda (&rest _) 'ok))
                    ((symbol-function 'hash-table-empty-p) (lambda (&rest _) 'ok))
                    ((symbol-function 'hash-table-keys) (lambda (&rest _) 'ok))
                    ((symbol-function 'hash-table-values) (lambda (&rest _) 'ok))
                    ((symbol-function 'string-remove-prefix) (lambda (&rest _) 'ok))
                    ((symbol-function 'string-remove-suffix) (lambda (&rest _) 'ok))
                    ((symbol-function 'string-replace) (lambda (&rest _) 'ok))
                    ((symbol-function 'string-limit) (lambda (&rest _) 'ok))
                    ((symbol-function 'string-pad) (lambda (&rest _) 'ok))
                    ((symbol-function 'proper-list-p) (lambda (&rest _) 'ok))
                    ((symbol-function 'mapcan) (lambda (&rest _) 'ok)))
            (should (eq 'ok (vendor-core-smoke--check-subr-x)))
            (should (memq 'subr-x required))
            (should-not (memq 'emacs-init required))))
      (setq features original-features))))

(ert-deftest vendor-core-smoke-test/seq-check-stays-lightweight ()
  "The seq lane must not load the full `emacs-init' bootstrap."
  (let ((original-features features)
        required)
    (unwind-protect
        (progn
          (provide 'seq)
          (cl-letf (((symbol-function 'require)
                     (lambda (feature &optional _filename _noerror)
                       (push feature required)
                       feature))
                    ((symbol-function 'seqp) (lambda (&rest _) 'ok))
                    ((symbol-function 'seq-length) (lambda (&rest _) 'ok))
                    ((symbol-function 'seq-elt) (lambda (&rest _) 'ok))
                    ((symbol-function 'seq-map) (lambda (&rest _) 'ok))
                    ((symbol-function 'seq-filter) (lambda (&rest _) 'ok))
                    ((symbol-function 'seq-remove) (lambda (&rest _) 'ok))
                    ((symbol-function 'seq-find) (lambda (&rest _) 'ok))
                    ((symbol-function 'seq-some) (lambda (&rest _) 'ok))
                    ((symbol-function 'seq-every-p) (lambda (&rest _) 'ok))
                    ((symbol-function 'seq-reduce) (lambda (&rest _) 'ok))
                    ((symbol-function 'seq-uniq) (lambda (&rest _) 'ok))
                    ((symbol-function 'seq-concatenate) (lambda (&rest _) 'ok)))
            (should (eq 'ok (vendor-core-smoke--check-seq)))
            (should (memq 'seq required))
            (should-not (memq 'emacs-init required))))
      (setq features original-features))))

(ert-deftest vendor-core-smoke-test/map-check-stays-lightweight ()
  "The map lane must not load the full `emacs-init' bootstrap."
  (let ((original-features features)
        required)
    (unwind-protect
        (progn
          (provide 'map)
          (cl-letf (((symbol-function 'require)
                     (lambda (feature &optional _filename _noerror)
                       (push feature required)
                       feature))
                    ((symbol-function 'mapp) (lambda (&rest _) 'ok))
                    ((symbol-function 'map-elt) (lambda (&rest _) 'ok))
                    ((symbol-function 'map-keys) (lambda (&rest _) 'ok))
                    ((symbol-function 'map-values) (lambda (&rest _) 'ok))
                    ((symbol-function 'map-pairs) (lambda (&rest _) 'ok))
                    ((symbol-function 'map-apply) (lambda (&rest _) 'ok))
                    ((symbol-function 'map-do) (lambda (&rest _) 'ok))
                    ((symbol-function 'map-empty-p) (lambda (&rest _) 'ok))
                    ((symbol-function 'map-contains-key) (lambda (&rest _) 'ok))
                    ((symbol-function 'map-merge) (lambda (&rest _) 'ok))
                    ((symbol-function 'map-merge-with) (lambda (&rest _) 'ok))
                    ((symbol-function 'map-into) (lambda (&rest _) 'ok))
                    ((symbol-function 'map-put!) (lambda (&rest _) 'ok))
                    ((symbol-function 'map-insert) (lambda (&rest _) 'ok)))
            (should (eq 'ok (vendor-core-smoke--check-map)))
            (should (memq 'map required))
            (should-not (memq 'emacs-init required))))
      (setq features original-features))))

(ert-deftest vendor-core-smoke-test/lisp-check-stays-lightweight ()
  "The lisp lane must not load the full `emacs-init' bootstrap."
  (let ((original-features features)
        required)
    (unwind-protect
        (progn
          (provide 'lisp)
          (cl-letf (((symbol-function 'require)
                     (lambda (feature &optional _filename _noerror)
                       (push feature required)
                       feature))
                    ((symbol-function 'forward-sexp) (lambda (&rest _) 'ok))
                    ((symbol-function 'backward-sexp) (lambda (&rest _) 'ok))
                    ((symbol-function 'mark-sexp) (lambda (&rest _) 'ok))
                    ((symbol-function 'forward-list) (lambda (&rest _) 'ok))
                    ((symbol-function 'backward-list) (lambda (&rest _) 'ok))
                    ((symbol-function 'down-list) (lambda (&rest _) 'ok))
                    ((symbol-function 'up-list) (lambda (&rest _) 'ok))
                    ((symbol-function 'backward-up-list) (lambda (&rest _) 'ok))
                    ((symbol-function 'kill-sexp) (lambda (&rest _) 'ok))
                    ((symbol-function 'backward-kill-sexp) (lambda (&rest _) 'ok))
                    ((symbol-function 'beginning-of-defun) (lambda (&rest _) 'ok))
                    ((symbol-function 'end-of-defun) (lambda (&rest _) 'ok))
                    ((symbol-function 'mark-defun) (lambda (&rest _) 'ok))
                    ((symbol-function 'insert-pair) (lambda (&rest _) 'ok))
                    ((symbol-function 'insert-parentheses) (lambda (&rest _) 'ok))
                    ((symbol-function 'delete-pair) (lambda (&rest _) 'ok))
                    ((symbol-function 'check-parens) (lambda (&rest _) 'ok)))
            (should (eq 'ok (vendor-core-smoke--check-lisp)))
            (should (memq 'lisp required))
            (should-not (memq 'emacs-init required))))
      (setq features original-features))))

(ert-deftest vendor-core-smoke-test/case-table-check-stays-lightweight ()
  "The case-table lane must not load the full `emacs-init' bootstrap."
  (let ((original-features features)
        required)
    (unwind-protect
        (progn
          (provide 'case-table)
          (cl-letf (((symbol-function 'require)
                     (lambda (feature &optional _filename _noerror)
                       (push feature required)
                       feature))
                    ((symbol-function 'describe-buffer-case-table) (lambda (&rest _) 'ok))
                    ((symbol-function 'case-table-get-table) (lambda (&rest _) 'ok))
                    ((symbol-function 'get-upcase-table) (lambda (&rest _) 'ok))
                    ((symbol-function 'copy-case-table) (lambda (&rest _) 'ok))
                    ((symbol-function 'set-case-syntax-delims) (lambda (&rest _) 'ok))
                    ((symbol-function 'set-case-syntax-pair) (lambda (&rest _) 'ok))
                    ((symbol-function 'set-upcase-syntax) (lambda (&rest _) 'ok))
                    ((symbol-function 'set-downcase-syntax) (lambda (&rest _) 'ok))
                    ((symbol-function 'set-case-syntax) (lambda (&rest _) 'ok))
                    ((symbol-function 'make-char-table) (lambda (&rest _) 'ok))
                    ((symbol-function 'char-table-p) (lambda (&rest _) 'ok))
                    ((symbol-function 'char-table-range) (lambda (&rest _) 'ok))
                    ((symbol-function 'set-char-table-range) (lambda (&rest _) 'ok))
                    ((symbol-function 'current-case-table) (lambda (&rest _) 'ok))
                    ((symbol-function 'standard-case-table) (lambda (&rest _) 'ok)))
            (should (eq 'ok (vendor-core-smoke--check-case-table)))
            (should (memq 'case-table required))
            (should-not (memq 'emacs-init required))))
      (setq features original-features))))

(ert-deftest vendor-core-smoke-test/cdl-check-stays-lightweight ()
  "The cdl lane must not load the full `emacs-init' bootstrap."
  (let ((original-features features)
        required)
    (unwind-protect
        (progn
          (provide 'cdl)
          (cl-letf (((symbol-function 'require)
                     (lambda (feature &optional _filename _noerror)
                       (push feature required)
                       feature))
                    ((symbol-function 'cdl-get-file) (lambda (&rest _) 'ok))
                    ((symbol-function 'cdl-put-region) (lambda (&rest _) 'ok))
                    ((symbol-function 'call-process) (lambda (&rest _) 'ok))
                    ((symbol-function 'call-process-region) (lambda (&rest _) 'ok)))
            (should (eq 'ok (vendor-core-smoke--check-cdl)))
            (should (memq 'cdl required))
            (should-not (memq 'emacs-init required))))
      (setq features original-features))))

(ert-deftest vendor-core-smoke-test/range-check-stays-lightweight ()
  "The range lane must not load the full `emacs-init' bootstrap."
  (let ((original-features features)
        required)
    (unwind-protect
        (progn
          (provide 'range)
          (cl-letf (((symbol-function 'require)
                     (lambda (feature &optional _filename _noerror)
                       (push feature required)
                       feature))
                    ((symbol-function 'range-normalize) (lambda (&rest _) 'ok))
                    ((symbol-function 'range-denormalize) (lambda (&rest _) 'ok))
                    ((symbol-function 'range-difference) (lambda (&rest _) 'ok))
                    ((symbol-function 'range-intersection) (lambda (&rest _) 'ok))
                    ((symbol-function 'range-compress-list) (lambda (&rest _) 'ok))
                    ((symbol-function 'range-uncompress) (lambda (&rest _) 'ok))
                    ((symbol-function 'range-add-list) (lambda (&rest _) 'ok))
                    ((symbol-function 'range-remove) (lambda (&rest _) 'ok))
                    ((symbol-function 'range-member-p) (lambda (&rest _) 'ok))
                    ((symbol-function 'range-list-intersection) (lambda (&rest _) 'ok))
                    ((symbol-function 'range-list-difference) (lambda (&rest _) 'ok))
                    ((symbol-function 'range-length) (lambda (&rest _) 'ok))
                    ((symbol-function 'range-concat) (lambda (&rest _) 'ok))
                    ((symbol-function 'range-map) (lambda (&rest _) 'ok)))
            (should (eq 'ok (vendor-core-smoke--check-range)))
            (should (memq 'range required))
            (should-not (memq 'emacs-init required))))
      (setq features original-features))))

(ert-deftest vendor-core-smoke-test/regi-check-stays-lightweight ()
  "The regi lane must not load the full `emacs-init' bootstrap."
  (let ((original-features features)
        required)
    (unwind-protect
        (progn
          (provide 'regi)
          (cl-letf (((symbol-function 'require)
                     (lambda (feature &optional _filename _noerror)
                       (push feature required)
                       feature))
                    ((symbol-function 'regi-pos) (lambda (&rest _) 'ok))
                    ((symbol-function 'regi-mapcar) (lambda (&rest _) 'ok))
                    ((symbol-function 'regi-interpret) (lambda (&rest _) 'ok)))
            (should (eq 'ok (vendor-core-smoke--check-regi)))
            (should (memq 'regi required))
            (should-not (memq 'emacs-init required))))
      (setq features original-features))))

(ert-deftest vendor-core-smoke-test/lisp-mode-check-stays-lightweight ()
  "The lisp-mode lane must not load the full `emacs-init' bootstrap."
  (let (required)
    (cl-letf (((symbol-function 'require)
               (lambda (feature &optional _filename _noerror)
                 (push feature required)
                 feature))
              ((symbol-function 'emacs-lisp-mode) (lambda (&rest _) 'ok))
              ((symbol-function 'lisp-mode) (lambda (&rest _) 'ok))
              ((symbol-function 'eval-defun) (lambda (&rest _) 'ok))
              ((symbol-function 'indent-sexp) (lambda (&rest _) 'ok)))
      (should (eq 'ok (vendor-core-smoke--check-lisp-mode)))
      (should (memq 'lisp-mode required))
      (should-not (memq 'emacs-init required)))))

(ert-deftest vendor-core-smoke-test/ielm-check-stays-lightweight ()
  "The ielm lane must not load the full `emacs-init' bootstrap."
  (let ((features (cons 'ielm features))
        required)
    (cl-letf (((symbol-function 'require)
               (lambda (feature &optional _filename _noerror)
                 (push feature required)
                 feature))
              ((symbol-function 'featurep)
               (vendor-core-smoke-test--featurep-for '(ielm)))
              ((symbol-function 'ielm) (lambda (&rest _) 'ok))
              ((symbol-function 'ielm-send-input) (lambda (&rest _) 'ok)))
      (should (eq 'ok (vendor-core-smoke--check-ielm)))
      (should (memq 'ielm required))
      (should-not (memq 'emacs-init required)))))

(ert-deftest vendor-core-smoke-test/isearch-check-stays-lightweight ()
  "The isearch lane must not load the full `emacs-init' bootstrap."
  (let (required)
    (cl-letf (((symbol-function 'require)
               (lambda (feature &optional _filename _noerror)
                 (push feature required)
                 feature))
              ((symbol-function 'isearch-forward) (lambda (&rest _) 'ok))
              ((symbol-function 'isearch-backward) (lambda (&rest _) 'ok))
              ((symbol-function 'isearch-forward-regexp)
               (lambda (&rest _) 'ok)))
      (should (eq 'ok (vendor-core-smoke--check-isearch)))
      (should (memq 'isearch required))
      (should-not (memq 'emacs-init required)))))

(ert-deftest vendor-core-smoke-test/minibuffer-check-stays-lightweight ()
  "The minibuffer lane must not load the full `emacs-init' bootstrap."
  (let (required)
    (cl-letf (((symbol-function 'require)
               (lambda (feature &optional _filename _noerror)
                 (push feature required)
                 feature))
              ((symbol-function 'completing-read) (lambda (&rest _) 'ok))
              ((symbol-function 'minibuffer-complete) (lambda (&rest _) 'ok))
              ((symbol-function 'minibuffer-complete-and-exit)
               (lambda (&rest _) 'ok)))
      (should (eq 'ok (vendor-core-smoke--check-minibuffer)))
      (should (memq 'minibuffer required))
      (should-not (memq 'emacs-init required)))))

(ert-deftest vendor-core-smoke-test/project-check-stays-lightweight ()
  "The project lane must not load the full `emacs-init' bootstrap."
  (let ((features (cons 'project features))
        required)
    (cl-letf (((symbol-function 'require)
               (lambda (feature &optional _filename _noerror)
                 (push feature required)
                 feature))
              ((symbol-function 'featurep)
               (vendor-core-smoke-test--featurep-for '(project)))
              ((symbol-function 'project-current) (lambda (&rest _) 'ok))
              ((symbol-function 'project-find-file) (lambda (&rest _) 'ok))
              ((symbol-function 'project-switch-project)
               (lambda (&rest _) 'ok)))
      (should (eq 'ok (vendor-core-smoke--check-project)))
      (should (memq 'project required))
      (should-not (memq 'emacs-init required)))))

(defun vendor-core-smoke-test--snapshot-values (symbols)
  "Return current value state for SYMBOLS."
  (mapcar (lambda (sym)
            (list sym (boundp sym) (and (boundp sym) (symbol-value sym))))
          symbols))

(defun vendor-core-smoke-test--restore-values (snapshot)
  "Restore variable values from SNAPSHOT."
  (dolist (entry snapshot)
    (let ((sym (nth 0 entry))
          (was-bound (nth 1 entry))
          (value (nth 2 entry)))
      (if was-bound
          (set sym value)
        (makunbound sym)))))

(defun vendor-core-smoke-test--snapshot-properties (symbols property)
  "Return current PROPERTY state for SYMBOLS."
  (mapcar (lambda (sym)
            (list sym (get sym property)))
          symbols))

(defun vendor-core-smoke-test--restore-properties (snapshot property)
  "Restore PROPERTY values from SNAPSHOT."
  (dolist (entry snapshot)
    (put (car entry) property (cadr entry))))

(ert-deftest vendor-core-smoke-test/class-a-i18n-checks-stay-lightweight ()
  "Class-A/i18n lanes must not load the full `emacs-init' bootstrap."
  (let* ((features-to-provide
          '(hex-util map-ynp charprop charscript emoji-labels iso-transl
                     cp51932 eucjp-ms fontset idna-mapping ja-dic-utl))
         (vars-to-set
          '(read-answer-short read-answer-map--memoize charprop--registry
                              charscript--scripts char-script-table
                              emoji--labels emoji--names emoji--derived
                              iso-transl-char-map iso-transl-language-alist
                              iso-transl-ctl-x-8-map key-translation-map
                              font-encoding-alist script-representative-chars
                              fontset-alias-alist standard-fontset-spec
                              idna-mapping-table skkdic-okurigana-table
                              skkdic-okuri-ari skkdic-okuri-nasi
                              skkdic-prefix skkdic-postfix))
         (translation-symbols
          '(cp51932-decode cp51932-encode eucjp-ms-decode eucjp-ms-encode))
         (original-features features)
         (value-snapshot (vendor-core-smoke-test--snapshot-values vars-to-set))
         (property-snapshot
          (vendor-core-smoke-test--snapshot-properties
           translation-symbols 'translation-table))
         required)
    (unwind-protect
        (progn
          (dolist (feature features-to-provide)
            (provide feature))
          (set 'read-answer-short 'auto)
          (set 'read-answer-map--memoize nil)
          (set 'charprop--registry '((name nil "name" nil)))
          (set 'charscript--scripts '(latin emoji))
          (set 'char-script-table (make-char-table 'char-script-table nil))
          (set 'emoji--labels '(("Smileys")))
          (set 'emoji--names (make-hash-table :test 'equal))
          (set 'emoji--derived (make-hash-table :test 'equal))
          (set 'iso-transl-char-map '(("A" . [65])))
          (set 'iso-transl-language-alist '(("Test" ("A" . [65]))))
          (set 'iso-transl-ctl-x-8-map (make-sparse-keymap))
          (set 'key-translation-map (make-sparse-keymap))
          (set 'font-encoding-alist '(("ascii" . ascii)))
          (set 'script-representative-chars '((latin ?A)))
          (set 'fontset-alias-alist nil)
          (set 'standard-fontset-spec "fontset-standard")
          (set 'idna-mapping-table (make-vector #x110000 nil))
          (set 'skkdic-okurigana-table '((#x3042 . ?a)))
          (set 'skkdic-okuri-ari nil)
          (set 'skkdic-okuri-nasi nil)
          (set 'skkdic-prefix nil)
          (set 'skkdic-postfix nil)
          (dolist (sym translation-symbols)
            (put sym 'translation-table (make-hash-table :test 'equal)))
          (cl-letf (((symbol-function 'require)
                     (lambda (feature &optional _filename _noerror)
                       (push feature required)
                       feature))
                    ((symbol-function 'decode-hex-string) (lambda (&rest _) 'ok))
                    ((symbol-function 'encode-hex-string) (lambda (&rest _) 'ok))
                    ((symbol-function 'map-y-or-n-p) (lambda (&rest _) 'ok))
                    ((symbol-function 'read-answer) (lambda (&rest _) 'ok))
                    ((symbol-function 'define-char-code-property) (lambda (&rest _) 'ok))
                    ((symbol-function 'get-char-code-property) (lambda (&rest _) 'ok))
                    ((symbol-function 'put-char-code-property) (lambda (&rest _) 'ok))
                    ((symbol-function 'unicode-property-table-internal) (lambda (&rest _) 'ok))
                    ((symbol-function 'char-code-property-description) (lambda (&rest _) 'ok))
                    ((symbol-function 'charscript--char-script) (lambda (&rest _) 'latin))
                    ((symbol-function 'iso-transl-define-keys) (lambda (&rest _) 'ok))
                    ((symbol-function 'iso-transl-set-language) (lambda (&rest _) 'ok))
                    ((symbol-function 'x-decompose-font-name) (lambda (&rest _) []))
                    ((symbol-function 'x-compose-font-name) (lambda (&rest _) "font"))
                    ((symbol-function 'set-font-encoding) (lambda (&rest _) 'ok))
                    ((symbol-function 'fontset-name-p) (lambda (&rest _) t))
                    ((symbol-function 'fontset-plain-name) (lambda (&rest _) "fontset"))
                    ((symbol-function 'generate-fontset-menu) (lambda (&rest _) '("Fontset")))
                    ((symbol-function 'setup-default-fontset) (lambda (&rest _) 'ok))
                    ((symbol-function 'create-default-fontset) (lambda (&rest _) 'ok))
                    ((symbol-function 'skkdic-lookup-key) (lambda (&rest _) '("candidate")))
                    ((symbol-function 'skkdic-merge-head-and-tail) (lambda (&rest _) '("candidate"))))
            (should (eq 'ok (vendor-core-smoke--check-hex-util)))
            (should (eq 'ok (vendor-core-smoke--check-map-ynp)))
            (should (eq 'ok (vendor-core-smoke--check-charprop)))
            (should (eq 'ok (vendor-core-smoke--check-charscript)))
            (should (eq 'ok (vendor-core-smoke--check-emoji-labels)))
            (should (eq 'ok (vendor-core-smoke--check-iso-transl)))
            (should (eq 'ok (vendor-core-smoke--check-cp51932)))
            (should (eq 'ok (vendor-core-smoke--check-eucjp-ms)))
            (should (eq 'ok (vendor-core-smoke--check-fontset)))
            (should (eq 'ok (vendor-core-smoke--check-idna-mapping)))
            (should (eq 'ok (vendor-core-smoke--check-ja-dic-utl)))
            (dolist (feature features-to-provide)
              (should (memq feature required)))
            (should-not (memq 'emacs-init required))))
      (setq features original-features)
      (vendor-core-smoke-test--restore-values value-snapshot)
      (vendor-core-smoke-test--restore-properties
       property-snapshot 'translation-table))))

(ert-deftest vendor-core-smoke-test/batch-returns-results-in-module-order ()
  (let ((vendor-core-smoke-modules
         '((files . vendor-core-smoke-test--pass-a)
           (simple . vendor-core-smoke-test--pass-b)))
        (vendor-core-smoke-strict t))
    (cl-letf (((symbol-function 'vendor-core-smoke-test--pass-a)
               (lambda () 'ok))
              ((symbol-function 'vendor-core-smoke-test--pass-b)
               (lambda () 'ok)))
      (should (equal (vendor-core-smoke-batch)
                     '((files pass "")
                       (simple pass "")))))))

(provide 'vendor-core-smoke-test)

;;; vendor-core-smoke-test.el ends here
