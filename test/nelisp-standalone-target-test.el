;;; nelisp-standalone-target-test.el --- tests for standalone target selection  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Host-side checks for standalone target/ABI selection.  These guard the
;; Windows-native path against mixing Win64 object cache entries with the
;; existing Linux/SysV standalone cache.

;;; Code:

(setq load-prefer-newer t)

(require 'ert)
(require 'cl-lib)

(let* ((this (or load-file-name buffer-file-name))
       (test-dir (and this (file-name-directory this)))
       (repo-root (and test-dir
                       (file-name-directory
                        (directory-file-name test-dir)))))
  (dolist (dir '("lisp" "src" "scripts"))
    (let ((path (and repo-root (expand-file-name dir repo-root))))
      (when (and path (file-directory-p path))
        (add-to-list 'load-path path)))))

(require 'nelisp-standalone-build)
(require 'nelisp-cc-sf-unwind-protect)
(require 'nelisp-cc-nlstr-direct-ops)
(require 'nelisp-cc-rootstack)
(require 'nelisp-cc-evalport-env-leaves-simple)
(require 'nelisp-cc-env-lookup-value)
(require 'nelisp-cc-env-set-value)
(require 'nelisp-cc-env-bind-local)
(require 'nelisp-cc-env-install-empty)
(require 'nelisp-cc-mirror-alloc-entry)
(require 'nelisp-cc-reader-lexer)
(require 'nelisp-cc-reader-parser)

(defconst nelisp-standalone-target-test--this-file
  (or load-file-name buffer-file-name)
  "Absolute path to this test file.")

(defvar nl_large_freelist_heads nil
  "Test-only dynamic binding target for allocator source-shape helpers.")

(defvar nl_gc_alloc_debt nil
  "Test-only dynamic binding target for generated `data-addr' calls.")

(defvar nl_gc_pending nil
  "Test-only dynamic binding target for deferred form-boundary GC requests.")

(defvar nl_gc_loop_ctx nil
  "Test-only dynamic binding target for generated `data-addr' calls.")

(defvar nl_gc_diag nil
  "Test-only dynamic binding target for generated `data-addr' calls.")

(defvar nl_bind_clone_force nil
  "Test-only dynamic binding target for generated `data-addr' calls.")

(defvar nl_rootstack_region nil
  "Test-only dynamic binding target for generated root-stack region calls.")

(defvar nl_rootstack_top nil
  "Test-only dynamic binding target for generated root-stack top calls.")

(defun nelisp-standalone-target-test--repo-root ()
  "Return the absolute path to the repo root."
  (let* ((test-dir (and nelisp-standalone-target-test--this-file
                        (file-name-directory
                         nelisp-standalone-target-test--this-file))))
    (and test-dir
         (file-name-directory
          (directory-file-name test-dir)))))

(defun nelisp-standalone-target-test--prelude-path ()
  "Return the absolute path to the standalone reader prelude."
  (let ((repo-root (nelisp-standalone-target-test--repo-root)))
    (expand-file-name "scripts/nelisp-stdlib-prelude.el" repo-root)))

(defvar nelisp-standalone-target-test--standalone-reader-built nil
  "Non-nil once `make standalone-reader' has been run in this session.")

(defun nelisp-standalone-target-test--ensure-standalone-reader ()
  "Build the standalone reader once for direct target probes."
  (unless nelisp-standalone-target-test--standalone-reader-built
    (let ((default-directory (nelisp-standalone-target-test--repo-root)))
      (unless (= (call-process "make" nil nil nil "standalone-reader") 0)
        (error "make standalone-reader failed"))))
  (setq nelisp-standalone-target-test--standalone-reader-built t))

(defun nelisp-standalone-target-test--with-prelude-write-region-stub (thunk)
  "Install the prelude's `write-region' stub, run THUNK, then restore.
This reads the exact `defun write-region' form from the prelude source so
the test executes the same body without loading the whole standalone file
into host Emacs."
  (let ((orig-write-region (symbol-function 'write-region)))
    (unwind-protect
        (progn
          (with-temp-buffer
            (insert-file-contents-literally
             (nelisp-standalone-target-test--prelude-path))
            (goto-char (point-min))
            (re-search-forward "(defun write-region" nil t)
            (goto-char (match-beginning 0))
            (eval (read (current-buffer))))
          (funcall thunk))
      (fset 'write-region orig-write-region))))

(defun nelisp-standalone-target-test--read-prelude-form (pattern)
  "Read the first top-level prelude form whose printed form matches PATTERN."
  (with-temp-buffer
    (insert-file-contents-literally
     (nelisp-standalone-target-test--prelude-path))
    (goto-char (point-min))
    (re-search-forward pattern nil t)
    (goto-char (match-beginning 0))
    (read (current-buffer))))

(defun nelisp-standalone-target-test--with-temporary-fdefinition (symbol value thunk)
  "Temporarily bind SYMBOL to VALUE, run THUNK, then restore the old binding."
  (let ((orig (and (fboundp symbol) (symbol-function symbol))))
    (unwind-protect
        (progn
          (fset symbol value)
          (funcall thunk))
      (if orig
          (fset symbol orig)
        (fmakunbound symbol)))))

(defun nelisp-standalone-target-test--with-prelude-rd-core (thunk)
  "Evaluate the exact reader core forms from the prelude and run THUNK."
  (let ((saved nil)
        (patterns '("(defun nelisp--rd-skip-ws"
                    "(defun nelisp--rd-atom-end"
                    "(defun nelisp--rd-escaped-atom"
                    "(defun nelisp--rd-numeric-token-p"
                    "(defun nelisp--rd-unescape"
                    "(defun nelisp--rd-string-end"
                    "(defun nelisp--rd-one")))
    (unwind-protect
        (progn
          (dolist (sym '(nelisp--rd-skip-ws
                         nelisp--rd-atom-end
                         nelisp--rd-escaped-atom
                         nelisp--rd-numeric-token-p
                         nelisp--rd-unescape
                         nelisp--rd-string-end
                         nelisp--rd-one))
            (push (cons sym (and (fboundp sym) (symbol-function sym))) saved)
            (when (fboundp sym)
              (fmakunbound sym)))
          (dolist (pattern patterns)
            (eval (nelisp-standalone-target-test--read-prelude-form pattern)))
          (funcall thunk))
      (dolist (entry saved)
        (if (cdr entry)
            (fset (car entry) (cdr entry))
          (fmakunbound (car entry)))))))

(defun nelisp-standalone-target-test--with-prelude-rd-unescape (thunk)
  "Evaluate the prelude `nelisp--rd-unescape' and run THUNK with it installed."
  (let ((orig (and (fboundp 'nelisp--rd-unescape)
                   (symbol-function 'nelisp--rd-unescape))))
    (unwind-protect
        (progn
          (when (fboundp 'nelisp--rd-unescape)
            (fmakunbound 'nelisp--rd-unescape))
          (eval (nelisp-standalone-target-test--read-prelude-form
                 "(defun nelisp--rd-unescape"))
          (funcall thunk))
      (if orig
          (fset 'nelisp--rd-unescape orig)
        (fmakunbound 'nelisp--rd-unescape)))))

(defun nelisp-standalone-target-test--gc-source-string ()
  "Return the printed GC source tree."
  (prin1-to-string nelisp-standalone--gc-source))

(defun nelisp-standalone-target-test--read-gc-form (pattern)
  "Read the first top-level GC form whose printed form matches PATTERN."
  (with-temp-buffer
    (insert (nelisp-standalone-target-test--gc-source-string))
    (goto-char (point-min))
    (re-search-forward pattern nil t)
    (goto-char (match-beginning 0))
    (read (current-buffer))))

(defun nelisp-standalone-target-test--arena-source-string ()
  "Return the printed arena source tree."
  (prin1-to-string (nelisp-standalone--target-arena-source)))

(defun nelisp-standalone-target-test--read-arena-form (pattern)
  "Read the first top-level arena form whose printed form matches PATTERN."
  (with-temp-buffer
    (insert (nelisp-standalone-target-test--arena-source-string))
    (goto-char (point-min))
    (re-search-forward (regexp-quote pattern) nil t)
    (goto-char (match-beginning 0))
    (read (current-buffer))))

(defun nelisp-standalone-target-test--read-arena-defun (name)
  "Read the first top-level arena `defun' whose name is NAME."
  (nelisp-standalone-target-test--read-arena-form
   (format "(defun %s " name)))

(defun nelisp-standalone-target-test--read-gc-defun (name)
  "Read the first top-level GC `defun' whose name is NAME."
  (nelisp-standalone-target-test--read-gc-form
   (format "(defun %s " name)))

(defun nelisp-standalone-target-test--find-defun (tree name)
  "Return the first `(defun NAME ...)' nested anywhere in TREE."
  (cond
   ((and (consp tree) (eq (car tree) 'defun) (eq (cadr tree) name))
    tree)
   ((consp tree)
    (or (nelisp-standalone-target-test--find-defun (car tree) name)
        (nelisp-standalone-target-test--find-defun (cdr tree) name)))))

(defun nelisp-standalone-target-test--run-reader-src (source)
  "Run the standalone reader with SOURCE passed through `--eval'."
  (let ((stdout-buf (generate-new-buffer " *nelisp-reader-stdout*"))
        (stderr-file (make-temp-file "nelisp-reader-stderr"))
        (exit-code nil))
    (unwind-protect
        (progn
          (setq exit-code
                (call-process (nelisp-standalone--output-path t)
                              nil (list stdout-buf stderr-file) nil
                              "--eval" source))
          (let ((stdout-text (with-current-buffer stdout-buf
                               (buffer-substring-no-properties
                                (point-min) (point-max))))
                (stderr-text (with-temp-buffer
                               (insert-file-contents stderr-file)
                               (buffer-substring-no-properties
                                (point-min) (point-max)))))
            (list :exit exit-code
                  :stdout stdout-text
                  :stderr stderr-text)))
      (when (buffer-live-p stdout-buf)
        (kill-buffer stdout-buf))
      (when (file-exists-p stderr-file)
        (delete-file stderr-file)))))

(defun nelisp-standalone-target-test--run-reader-env-src (source)
  "Run the standalone reader with SOURCE supplied via `NELISP_SRC'."
  (let ((stdout-buf (generate-new-buffer " *nelisp-reader-stdout*"))
        (stderr-file (make-temp-file "nelisp-reader-stderr"))
        (process-environment (cons (concat "NELISP_SRC=" source)
                                   process-environment))
        (exit-code nil))
    (unwind-protect
        (progn
          (setq exit-code
                (call-process (nelisp-standalone--output-path t)
                              nil (list stdout-buf stderr-file) nil))
          (let ((stdout-text (with-current-buffer stdout-buf
                               (buffer-substring-no-properties
                                (point-min) (point-max))))
                (stderr-text (with-temp-buffer
                               (insert-file-contents stderr-file)
                               (buffer-substring-no-properties
                                (point-min) (point-max)))))
            (list :exit exit-code
                  :stdout stdout-text
                  :stderr stderr-text)))
      (when (buffer-live-p stdout-buf)
        (kill-buffer stdout-buf))
      (when (file-exists-p stderr-file)
        (delete-file stderr-file)))))

(defun nelisp-standalone-target-test--run-reader-value (source)
  "Run SOURCE in the standalone reader and return the printed value."
  (let* ((result (nelisp-standalone-target-test--run-reader-src source))
         (exit (plist-get result :exit))
         (stdout (plist-get result :stdout))
         (stderr (plist-get result :stderr)))
    (should (= exit 0))
    (should (string-match-p "\\`[[:space:]\n]*\\'" stderr))
    (car (read-from-string stdout))))

(defun nelisp-standalone-target-test--build-script-source ()
  "Return the standalone build script as a plain source string."
  (let* ((test-dir (and nelisp-standalone-target-test--this-file
                        (file-name-directory
                         nelisp-standalone-target-test--this-file)))
         (repo-root (and test-dir
                         (file-name-directory
                          (directory-file-name test-dir))))
         (path (and repo-root
                    (expand-file-name "scripts/nelisp-standalone-build.el"
                                      repo-root))))
    (with-temp-buffer
      (insert-file-contents-literally path)
      (buffer-string))))

(defun nelisp-standalone-target-test--run-reader-env-value (source)
  "Run SOURCE via `NELISP_SRC' in the standalone reader and return the value."
  (let* ((result (nelisp-standalone-target-test--run-reader-env-src source))
         (exit (plist-get result :exit))
         (stdout (plist-get result :stdout))
         (stderr (plist-get result :stderr)))
    (should (= exit 0))
    (should (string-match-p "\\`[[:space:]\n]*\\'" stderr))
    (car (read-from-string stdout))))

(ert-deftest nelisp-standalone-target-defaults-to-linux-sysv ()
  "The default target remains Linux/SysV for compatibility on every host."
  (should (eq nelisp-standalone--target 'linux-x86_64))
  (should (eq (nelisp-standalone--target-abi 'linux-x86_64) 'sysv)))

(ert-deftest nelisp-standalone-target-boundp-passes-unbound-marker ()
  "Standalone `boundp' must keep forwarding the env+64 unbound pointer."
  (let ((source (nelisp-standalone-target-test--build-script-source)))
    (should (string-match-p
             (regexp-quote "(let* ((sym (wf_arg_ptr args 0))")
             source))
    (should (string-match-p
             (regexp-quote "(mirror (+ env 0))")
             source))
    (should (string-match-p
             (regexp-quote "(unbound (+ env 64))")
             source))
    (should (string-match-p
             (regexp-quote "(nelisp_mirror_is_bound mirror sym unbound)")
             source))))

(ert-deftest nelisp-standalone-target-prelude-bootstraps-emacs-version-vars ()
  "Loading the standalone prelude must install Emacs-compatible version vars."
  (nelisp-standalone-target-test--ensure-standalone-reader)
  (let ((value
         (nelisp-standalone-target-test--run-reader-value
          (format "(progn
                     (load %S)
                     (list (boundp 'emacs-version)
                           (stringp emacs-version)
                           (version< \"29\" emacs-version)
                           emacs-version
                           emacs-major-version
                           emacs-minor-version))"
                  (nelisp-standalone-target-test--prelude-path)))))
    (should (equal value '(t t t "30.1" 30 1)))))

(ert-deftest nelisp-standalone-target-append-writer-uses-o-append-without-read ()
  "The Phase-A append primitive must be an atomic open+write path."
  (let* ((nelisp-standalone--target 'linux-x86_64)
         (forms (nelisp-standalone--fileio-source))
         (append-form
          (cl-find-if (lambda (form)
                        (and (consp form)
                             (eq (car form) 'defun)
                             (eq (cadr form) 'nl_bi_append_file_t)))
                      forms))
         (os-source (prin1-to-string
                     (nelisp-standalone--reader-os-source-forms)))
         (append-source (prin1-to-string append-form))
         (builtins-source (prin1-to-string
                           (nelisp-standalone--applyfn-reader-table))))
    (should append-form)
    (should (string-match-p
             "(defun nl_os_open_write_append (path) (syscall-direct 2 path 1089 420"
             os-source))
    (should (string-match-p "nl_os_open_write_append" append-source))
    (should (string-match-p "nl_bi_write_decoded" append-source))
    (should-not (string-match-p "read" append-source))
    (should (string-match-p "nl-append-file" builtins-source))))

(ert-deftest nelisp-standalone-target-read-file-source-uses-stat-sized-loop ()
  "The standalone read-file builtin must stat first, allocate once, and loop partial reads."
  (let* ((source (nelisp-standalone-target-test--build-script-source))
         (beg (string-match (regexp-quote "(defun wf_copy32_strnil (out)") source))
         (end (string-match (regexp-quote "(defun nl_bi_slen (args out)") source beg))
         (snippet (substring source beg end)))
    (should (string-match-p (regexp-quote "(defun nl_bi_rf_stat_size (cpath)") snippet))
    (should (string-match-p (regexp-quote "(nl_os_stat_path cpath buf)") snippet))
    (should (string-match-p (regexp-quote "(ptr-read-u64 buf 48)") snippet))
    (should (string-match-p (regexp-quote "(defun nl_bi_rf_read_loop (fd ptr len off)") snippet))
    (should (string-match-p (regexp-quote "(= n -4)") snippet))
    (should (string-match-p (regexp-quote "(alloc-bytes (if (= size 0) 1 size) 1)") snippet))
    (should (string-match-p (regexp-quote "(nl_seq2 (nl_os_close_handle fd)") snippet))
    (should-not (string-match-p (regexp-quote "(alloc-bytes 4096 1)") snippet))
    (should-not (string-match-p (regexp-quote "(alloc-bytes 8388608 1)") snippet))))

(ert-deftest nelisp-standalone-target-stdlib-write-region-contracts-are-synced ()
  "The canonical stdlib and winning prelude use the same append primitive."
  (dolist (path '("lisp/nelisp-stdlib-misc.el"
                  "scripts/nelisp-stdlib-prelude.el"))
    (let ((source (with-temp-buffer
                    (insert-file-contents path)
                    (buffer-string))))
      (should (string-match-p "(nl-append-file filename bytes)" source))
      (should (string-match-p "(nl-write-file filename bytes)" source))
      (should-not (string-match-p
                   "write-region stub: APPEND not supported" source)))))

(ert-deftest nelisp-standalone-target-stdlib-package-resolution-contracts-are-synced ()
  "The canonical stdlib and winning prelude keep real package resolution."
  (dolist (path '("lisp/nelisp-stdlib-misc.el"
                  "scripts/nelisp-stdlib-prelude.el"))
    (let ((source (with-temp-buffer
                    (insert-file-contents path)
                    (buffer-string))))
      (should (string-match-p "(defun nelisp--normalize-posix-path" source))
      (should (string-match-p "(defun nelisp--expand-home-prefix" source))
      (should (string-match-p "(defun load-file (file)" source))
      (should (string-match-p "(defun require (feature &optional filename noerror)"
                              source))
      (should-not (string-match-p "nl-getenv" source))
      (should-not (string-match-p "(provide feature)" source)))))

(ert-deftest nelisp-standalone-target-require-contracts-are-synced ()
  "The canonical stdlib and winning prelude use the same `require' body."
  (let ((forms nil))
    (dolist (path '("lisp/nelisp-stdlib-misc.el"
                    "scripts/nelisp-stdlib-prelude.el"))
      (with-temp-buffer
        (insert-file-contents path)
        (goto-char (point-min))
        (re-search-forward
         "(defun require (feature &optional filename noerror)" nil t)
        (goto-char (match-beginning 0))
        (setq forms (cons (read (current-buffer)) forms))))
    (should (equal (car forms) (cadr forms)))))

(ert-deftest nelisp-standalone-target-load-publishes-and-restores-context ()
  "Native eval sees nested load context, which is restored on every exit."
  (nelisp-standalone-target-test--ensure-standalone-reader)
  (let* ((root (make-temp-file "nelisp-load-context-" t))
         (parent (file-name-as-directory root))
         (a-path (expand-file-name "a.el" root))
         (b-path (expand-file-name "b.el" root))
         (boom-path (expand-file-name "boom.el" root))
         (misc-path
          (expand-file-name
           "lisp/nelisp-stdlib-misc.el"
           (nelisp-standalone-target-test--repo-root))))
    (unwind-protect
        (progn
          (with-temp-file b-path
            (insert
             "(setq nelisp-load-context-b\n"
             "      (list load-file-name default-directory load-path))\n"
             "(provide 'b)\n"))
          (with-temp-file a-path
            (insert
             "(setq nelisp-load-context-a-before\n"
             "      (list load-file-name default-directory load-path))\n"
             "(require 'b)\n"
             "(setq nelisp-load-context-a-after\n"
             "      (list load-file-name default-directory load-path))\n"
             "(provide 'nelisp-load-context-a)\n"))
          (with-temp-file boom-path
            (insert
             "(setq nelisp-load-context-boom\n"
             "      (list load-file-name default-directory load-path))\n"
             "(error \"nelisp-load-context-boom-marker\")\n"))
          (let* ((source
                  (format
                   "(progn
                      (load %S)
                      (setq load-path '(sentinel-path))
                      (setq load-file-name 'sentinel-file)
                      (setq default-directory \"sentinel-directory/\")
                      (let ((load-path (list %S)))
                        (load %S))
                      (let ((after-success
                             (list load-path load-file-name
                                   default-directory)))
                        (nelisp--env-globals-op
                         'clear-value 'load-file-name)
                        (let ((load-path (list %S)))
                          (condition-case nil
                              (load %S)
                            (error t)))
                        (list nelisp-load-context-a-before
                              nelisp-load-context-b
                              nelisp-load-context-a-after
                              after-success
                              nelisp-load-context-boom
                              (list load-path
                                    (boundp 'load-file-name)
                                    default-directory))))"
                   misc-path root a-path root boom-path))
                 (result
                  (nelisp-standalone-target-test--run-reader-src source))
                 (stderr (plist-get result :stderr))
                 (value
                  (car (read-from-string (plist-get result :stdout)))))
            (should (= (plist-get result :exit) 0))
            ;; Loading the canonical misc source currently reports these
            ;; three pre-existing alias failures while continuing normally.
            ;; Each one now prints its error before the form: the replay loop
            ;; used to drop the error text, which left real bootstrap failures
            ;; with no diagnosable cause.  The error text itself is left
            ;; unconstrained here; what this asserts is that exactly these
            ;; three forms fail, each with an error, and nothing else.
            (should
             (string-match-p
              (concat
               "\\`nelisp: uncaught error: [^\n]*\n"
               "nelisp: failing source cursor=[0-9]+ "
               "form=(defalias (quote sxhash-equal) (quote sxhash))\n"
               "nelisp: uncaught error: [^\n]*\n"
               "nelisp: failing source cursor=[0-9]+ "
               "form=(defalias (quote sxhash-eq) (quote sxhash))\n"
               "nelisp: uncaught error: [^\n]*\n"
               "nelisp: failing source cursor=[0-9]+ "
               "form=(defalias (quote sxhash-eql) (quote sxhash))\n\\'")
              stderr))
            (should
             (equal
              value
              (list
               (list a-path parent (list root))
               (list b-path parent (list root))
               (list a-path parent (list root))
               '((sentinel-path) sentinel-file "sentinel-directory/")
               (list boom-path parent (list root))
               '((sentinel-path) nil "sentinel-directory/"))))))
      (delete-directory root t))))

(ert-deftest nelisp-standalone-target-core-fileio-runtime-order-is-real ()
  "Every command runtime loads the minimal UTF-8/file I/O modules before load."
  (let ((sources
         (list (cons (nelisp-standalone--artifact-command-runtime-src) nil)
               (cons (nelisp-standalone--artifact-command-cache-src) nil)
               (cons (nelisp-standalone--artifact-source-command-cache-src t)
                     t))))
    (dolist (entry sources)
      (let* ((source (car entry))
             (inline (cdr entry))
             (utf8 (string-match "nelisp-coding-utf8\\.el" source))
            (fileio (string-match "nelisp-core-fileio\\.el" source))
            (loader (string-match "nelisp-load\\.el" source))
            (provided
             (string-match
              (regexp-quote "(provide 'nelisp-core-fileio)")
              source)))
        (should (integerp utf8))
        (should (integerp fileio))
        (should (integerp loader))
        (should (< utf8 fileio))
        (should (< fileio loader))
        ;; Inline runtimes contain the module's real trailing provide.
        ;; A synthetic post-loader provide would violate this ordering.
        (if inline
            (progn
              (should (integerp provided))
              (should (< provided loader))
              (should-not
               (string-match
                (regexp-quote "(provide 'nelisp-core-fileio)")
                source (1+ provided))))
          (should-not provided))))))

(ert-deftest nelisp-standalone-target-artifact-cache-bounds-module-reads ()
  "Cache bootstrap bounds canonical fn/eval and legacy reader inputs."
  (let ((source (nelisp-standalone--artifact-command-cache-src)))
    (dolist (proof
             '("(= (aref source bcl-pos) 40)"
               "(consp (car bcl-read))"
               "(eq (car (car bcl-read)) 'nelisp-bcl)"
               "(symbolp name)"
               "(substring source value-pos (1- end))"
               "(= (cdr value-res) (length value-source))"
               "(substring source pos end)"
               "(= (cdr item-res) (length item-source))"))
      (should (string-match-p (regexp-quote proof) source)))))

(ert-deftest nelisp-standalone-target-core-fileio-has-minimal-utf8-dependency ()
  "The loader file I/O layer uses the split UTF-8 feature without CL shims."
  (let ((core-source
         (with-temp-buffer
           (insert-file-contents "src/nelisp-core-fileio.el")
           (buffer-string)))
        (utf8-source
         (with-temp-buffer
           (insert-file-contents "src/nelisp-coding-utf8.el")
           (buffer-string))))
    (should (string-match-p
             (regexp-quote "(require 'nelisp-coding-utf8)")
             core-source))
    (should-not (string-match-p
                 (regexp-quote "(require 'nelisp-coding)")
                 core-source))
    (dolist (obsolete '("(require 'cl-lib)"
                        "(require 'subr-x)"
                        "cl-position"
                        "string-empty-p"))
      (should-not (string-match-p (regexp-quote obsolete) core-source)))
    (should (string-match-p
             (regexp-quote "(provide 'nelisp-coding-utf8)")
             utf8-source))
    (should-not (string-match-p "nelisp-coding-jis-tables" utf8-source))))

(ert-deftest nelisp-standalone-target-feature-functions-are-not-native-stubs ()
  "Feature state and package loading must dispatch through prelude functions."
  (dolist (name '("featurep" "provide" "require"))
    (should-not
     (assoc (list :lit name) nelisp-standalone--applyfn-bf-arms))
    (should-not (member name nelisp-standalone--applyfn-bf-builtins))
    (should-not (member name nelisp-standalone--reader-builtins)))
  (let ((source (with-temp-buffer
                  (insert-file-contents "scripts/nelisp-stdlib-prelude.el")
                  (buffer-string))))
    (should (string-match-p
             (regexp-quote "(defun featurep (feature)") source))
    (should (string-match-p
             (regexp-quote "(defun provide (feature)") source))
    (should (string-match-p
             (regexp-quote
              "(defun require (feature &optional filename noerror)")
             source))
    (should (string-match-p
             (regexp-quote
              "(let ((resolved (locate-library")
             source))
    (should (string-match-p
             (regexp-quote "(if (featurep feature)") source))
    (let ((feature-definition
           (string-match (regexp-quote "(defun provide (feature)") source))
          (first-bootstrap-provide
           (string-match (regexp-quote "(provide 'cl-lib)") source)))
      (should (integerp feature-definition))
      (should (integerp first-bootstrap-provide))
      (should (< feature-definition first-bootstrap-provide)))))

(ert-deftest nelisp-standalone-target-prelude-throw-has-callable-bridge ()
  "The prelude gives alias users a function cell backed by throw syntax."
  (let ((orig-helper
         (and (fboundp 'nelisp--throw-function)
              (symbol-function 'nelisp--throw-function)))
        (source (with-temp-buffer
                  (insert-file-contents
                   "scripts/nelisp-stdlib-prelude.el")
                  (buffer-string))))
    (unwind-protect
        (progn
          (eval (nelisp-standalone-target-test--read-prelude-form
                 "(defun nelisp--throw-function"))
          ;; Host Emacs already has a callable `throw'; this executes the
          ;; exact bridge body while the source-shape assertions below fix
          ;; the standalone-only guarded alias.
          (should (= (catch 'nelisp-throw-bridge
                       (nelisp--throw-function 'nelisp-throw-bridge 7))
                     7))
          (should (string-match-p
                   (regexp-quote "(unless (fboundp 'throw)")
                   source))
          (should (string-match-p
                   (regexp-quote
                    "(defalias 'throw 'nelisp--throw-function)")
                   source)))
      (if orig-helper
          (fset 'nelisp--throw-function orig-helper)
        (fmakunbound 'nelisp--throw-function)))))

(ert-deftest nelisp-standalone-target-prelude-provides-real-seq-surface ()
  "The prelude provides `seq' only after its compatibility API is present."
  (let* ((source (with-temp-buffer
                   (insert-file-contents
                    "scripts/nelisp-stdlib-prelude.el")
                   (buffer-string)))
         (provided (string-match
                    (regexp-quote "(provide 'seq)") source))
         (apis '(seq-doseq seq-setq seq-let seq-first seq-rest seqp
                 seq-copy seq-subseq seq-map-indexed seq-do-indexed
                 seq-drop-while seq-take-while seq-sort-by
                 seq-remove-at-position seq-contains seq-set-equal-p
                 seq-positions seq-union seq-intersection seq-difference
                 seq-random-elt seq-split seq-keep)))
    (should (integerp provided))
    (dolist (api apis)
      (let ((definition
             (string-match
              (regexp-quote
               (format "(unless (fboundp '%s)" api))
              source)))
        (should (integerp definition))
        (should (< definition provided))))))

(ert-deftest nelisp-standalone-target-prelude-seq-org-compat-behavior ()
  "Pure prelude seq functions cover Org's list, vector, and string uses."
  (let* ((symbols '(nelisp-seq--to-list
                    seq-first seq-do-indexed seq-difference))
         (saved
          (mapcar (lambda (symbol)
                    (cons symbol
                          (and (fboundp symbol)
                               (symbol-function symbol))))
                  symbols))
         (saved-features features))
    (unwind-protect
        (progn
          (dolist (symbol symbols)
            (when (fboundp symbol)
              (fmakunbound symbol)))
          (eval (nelisp-standalone-target-test--read-prelude-form
                 "(defun nelisp-seq--to-list"))
          (dolist (symbol '(seq-first seq-do-indexed seq-difference))
            (eval
             (nelisp-standalone-target-test--read-prelude-form
              (format "(unless (fboundp '%s)" symbol))))
          (should (eq (seq-first '(alpha beta)) 'alpha))
          (should (= (seq-first [10 20]) 10))
          (should (= (seq-first "ab") ?a))
          (let ((seen nil))
            (should-not
             (seq-do-indexed
              (lambda (item index)
                (setq seen (cons (list item index) seen)))
              [?x ?y]))
            (should (equal (nreverse seen) '((120 0) (121 1)))))
          (should (equal (seq-difference '(a b c b) [b]) '(a c)))
          (should (equal (seq-difference "abca" "b") '(97 99 97)))
          (setq features (delq 'seq features))
          (eval (nelisp-standalone-target-test--read-prelude-form
                 "(provide 'seq)"))
          (should (featurep 'seq)))
      (dolist (entry saved)
        (if (cdr entry)
            (fset (car entry) (cdr entry))
          (fmakunbound (car entry))))
      (setq features saved-features))))

(ert-deftest nelisp-standalone-target-prelude-require-loads-and-verifies-feature ()
  "The prelude feature functions maintain state and enforce `require'."
  (let ((orig-featurep (symbol-function 'featurep))
        (orig-provide (symbol-function 'provide))
        (orig-require (symbol-function 'require))
        (orig-locate-library (symbol-function 'locate-library))
        (orig-locate-probe
         (and (fboundp 'nelisp--locate-probe)
              (symbol-function 'nelisp--locate-probe)))
        (orig-syscall-stat
         (and (fboundp 'nelisp--syscall-stat)
              (symbol-function 'nelisp--syscall-stat)))
        (saved-features features)
        (temp-dir (make-temp-file "nelisp-prelude-require-" t))
        (feature 'nelisp-prelude-require-test-feature)
        (native-comp-enable-subr-trampolines nil))
    (unwind-protect
        (progn
          (eval (nelisp-standalone-target-test--read-prelude-form
                 "(defun provide"))
          (eval (nelisp-standalone-target-test--read-prelude-form
                 "(defun featurep"))
          (eval (nelisp-standalone-target-test--read-prelude-form
                 "(defun nelisp--locate-probe"))
          (eval (nelisp-standalone-target-test--read-prelude-form
                 "(defun locate-library"))
          (eval (nelisp-standalone-target-test--read-prelude-form
                 "(defun require"))
          (fset 'nelisp--syscall-stat
                (lambda (path)
                  (if (and (file-exists-p path)
                           (not (file-directory-p path)))
                      'file
                    'absent)))
          (setq features nil)
          (should-not (featurep feature))
          (should (eq (provide feature) feature))
          (should (featurep feature))
          (setq features nil)
          (provide 'cl-lib)
          (provide 'nelisp-cl-macros)
          (should (featurep 'cl-lib))
          (should (featurep 'nelisp-cl-macros))
          (setq features nil)
          (let ((path (expand-file-name "provided.el" temp-dir)))
            (with-temp-file path
              (insert "(provide 'nelisp-prelude-require-test-feature)\n"))
            (with-temp-file (expand-file-name "provided.elc" temp-dir)
              (insert "not standalone-readable bytecode\n"))
            (let ((load-path (list temp-dir))
                  (default-directory temp-dir))
              (should (equal (locate-library "provided") path))
              (should (file-name-absolute-p
                       (locate-library "provided")))
              (should (eq (require feature "provided") feature)))
            (should (featurep feature)))
          (setq features nil)
          (should-not
           (require 'nelisp-prelude-require-missing
                    (expand-file-name "missing.el" temp-dir) t))
          (should-error
           (require 'nelisp-prelude-require-missing
                    (expand-file-name "missing.el" temp-dir)))
          (let ((path (expand-file-name "boom.el" temp-dir)))
            (with-temp-file path
              (insert "(error \"boom-marker\")\n"))
            (let ((failure
                   (should-error
                    (require 'nelisp-prelude-require-boom path t))))
              (should (string-match-p
                       "boom-marker"
                       (error-message-string failure)))))
          (let ((path (expand-file-name "no-provide.el" temp-dir)))
            (with-temp-file path
              (insert "(setq nelisp-prelude-require-loaded t)\n"))
            (let ((failure
                   (should-error
                    (require 'nelisp-prelude-require-no-provide path t))))
              (should (string-match-p
                       (regexp-quote path)
                       (error-message-string failure)))
              (should (string-match-p
                       "failed to provide feature"
                       (error-message-string failure))))))
      (fset 'featurep orig-featurep)
      (fset 'provide orig-provide)
      (fset 'require orig-require)
      (fset 'locate-library orig-locate-library)
      (if orig-locate-probe
          (fset 'nelisp--locate-probe orig-locate-probe)
        (fmakunbound 'nelisp--locate-probe))
      (if orig-syscall-stat
          (fset 'nelisp--syscall-stat orig-syscall-stat)
        (fmakunbound 'nelisp--syscall-stat))
      (setq features saved-features)
      (delete-directory temp-dir t))))

(ert-deftest nelisp-standalone-target-setf-composite-accessors-are-synced ()
  "The lisp and standalone prelude copies expose the same composite `setf' support."
  (dolist (path '("lisp/nelisp-cl-macros.el"
                  "scripts/nelisp-stdlib-prelude.el"))
    (let ((source (with-temp-buffer
                    (insert-file-contents path)
                    (buffer-string))))
      (should (string-match-p
               (regexp-quote
                "standard composite list accessors c[ad]{2,4}r")
               source))
      (should (string-match-p
               (regexp-quote "nelisp--setf-composite-accessor-p")
               source))
      (should (string-match-p
               (regexp-quote "(null (cddr place))")
               source))
      (should (string-match-p
               (regexp-quote "nested `setcar' / `setcdr'")
               source)))))

(ert-deftest nelisp-standalone-target-raw-byte-accessor-is-registered ()
  "Standalone builds install the raw byte accessor without changing `aref'."
  (should (member "nelisp--string-byte-at" nelisp-standalone--reader-builtins))
  (should (string-match-p "nelisp--string-byte-at"
                          (prin1-to-string
                           (nelisp-standalone--applyfn-reader-table))))
  (should (= (nelisp--string-byte-at (unibyte-string 208 0 255) 0) 208))
  (should (= (aref (string 1024) 0) 1024)))

(ert-deftest nelisp-standalone-target-ptr-copy-string-bytes-is-registered ()
  "Standalone builds install the byte copy helper and dispatch it natively."
  (should (member "nelisp--ptr-copy-string-bytes" nelisp-standalone--reader-builtins))
  (should (string-match-p
           (regexp-quote "(:lit \"nelisp--ptr-copy-string-bytes\")")
           (prin1-to-string (nelisp-standalone--applyfn-reader-table))))
  (should (string-match-p
           (regexp-quote "(defun nelisp--ptr-copy-string-bytes")
           (prin1-to-string nelisp-standalone--applyfn-m5-helpers))))

(ert-deftest nelisp-standalone-target-size-census-uses-large-block-buckets ()
  "The reader-only size census keeps exact large BLOCK_TOTAL buckets and output order."
  (let ((source (prin1-to-string nelisp-standalone--applyfn-census-helpers)))
    (should (string-match-p "(alloc-bytes 88 8)" source))
    (should (string-match-p
             (regexp-quote
              "(if (= bt 88) (ptr-write-u64 (+ acc 16) 0 (+ (ptr-read-u64 (+ acc 16) 0) bt)) 0)")
             source))
    (should (string-match-p "(if (< bt 33)" source))
    (should (string-match-p "(if (< bt 65)" source))
    (should (string-match-p "(if (< bt 257)" source))
    (should (string-match-p "(if (< bt 4097)" source))
    (should (string-match-p "(if (< bt 262145)" source))
    (should (string-match-p "(if (< bt 2097153)" source))
    (should (string-match-p
             (regexp-quote
              "(if (= bt 88) (ptr-write-u64 (+ acc 16) 0 (+ (ptr-read-u64 (+ acc 16) 0) bt)) 0) (if (< bt 33)")
             source))
    (dolist (fragment '("(wf_cons_int (ptr-read-u64 (+ acc 80) 0) s11 s10)"
                        "(wf_cons_int (ptr-read-u64 (+ acc 72) 0) s10 s9)"
                        "(wf_cons_int (ptr-read-u64 (+ acc 64) 0) s9 s8)"
                        "(wf_cons_int (ptr-read-u64 (+ acc 56) 0) s8 s7)"
                        "(wf_cons_int (ptr-read-u64 (+ acc 48) 0) s7 s6)"
                        "(wf_cons_int (ptr-read-u64 (+ acc 40) 0) s6 s5)"
                        "(wf_cons_int (ptr-read-u64 (+ acc 32) 0) s5 s4)"
                        "(wf_cons_int (ptr-read-u64 (+ acc 24) 0) s4 s3)"
                        "(wf_cons_int (ptr-read-u64 (+ acc 16) 0) s3 s2)"
                        "(wf_cons_int (ptr-read-u64 (+ acc 8) 0) s2 s1)"))
      (should (string-match-p (regexp-quote fragment) source)))
    (should (string-match-p "(ptr-read-u64 268436296 0) (ptr-read-u64 268436288 0)" source))))

(ert-deftest nelisp-standalone-target-free-size-census-uses-guarded-block-walk ()
  "The free-size census walks arena blocks with the same malformed-block guard."
  (let ((source (prin1-to-string nelisp-standalone--applyfn-census-helpers)))
    (should (member "nelisp--free-size-census" nelisp-standalone--reader-builtins))
    (should (string-match-p
             (regexp-quote "(:lit \"nelisp--free-size-census\")")
             (prin1-to-string (nelisp-standalone--applyfn-reader-table))))
    (should (string-match-p "(alloc-bytes 96 8)" source))
    (should (string-match-p
             (regexp-quote
              "(if (= (nl_gc_bt_ok hdr (nl_hdr_bt hdr) end) 0) 0 (nl_seq2 (bf_free_size_census_block hdr acc) (+ hdr (nl_hdr_bt hdr))))")
             source))
    (should (string-match-p "(bf_free_size_census_chunks (ptr-read-u64 (+ chunk 48) 0) acc)" source))
    (should-not (string-match-p "nl_large_freelist_heads" source))
    (dolist (fragment '("(if (< bt 473)"
                        "(if (< bt 4097)"
                        "(if (< bt 262145)"
                        "(if (< bt 2097153)"
                        "(if (< bt 16777217)"
                        "(wf_cons_int (ptr-read-u64 (+ acc 88) 0) nil-slot s12)"
                        "(wf_cons_int (ptr-read-u64 (+ acc 80) 0) s12 s11)"
                        "(wf_cons_int (ptr-read-u64 (+ acc 72) 0) s11 s10)"
                        "(wf_cons_int (ptr-read-u64 (+ acc 64) 0) s10 s9)"
                        "(wf_cons_int (ptr-read-u64 (+ acc 56) 0) s9 s8)"
                        "(wf_cons_int (ptr-read-u64 (+ acc 48) 0) s8 s7)"
                        "(wf_cons_int (ptr-read-u64 (+ acc 40) 0) s7 s6)"
                        "(wf_cons_int (ptr-read-u64 (+ acc 32) 0) s6 s5)"
                        "(wf_cons_int (ptr-read-u64 (+ acc 24) 0) s5 s4)"
                        "(wf_cons_int (ptr-read-u64 (+ acc 16) 0) s4 s3)"
                        "(wf_cons_int (ptr-read-u64 (+ acc 8) 0) s3 s2)"
                        "(wf_cons_int (ptr-read-u64 acc 0) s2 out)"))
      (should (string-match-p (regexp-quote fragment) source)))))

(ert-deftest nelisp-standalone-target-free-size-census-direct-runtime ()
  "The built standalone runtime returns the requested 12-field free census."
  (nelisp-standalone-target-test--ensure-standalone-reader)
  (let* ((value (nelisp-standalone-target-test--run-reader-value
                 "(progn
                    (nelisp--arena-force-grow-smoke)
                    (garbage-collect)
                    (let ((free (nelisp--free-size-census)))
                      (garbage-collect)
                      (list free (nth 1 (nelisp--size-census)))))"))
         (free (car value))
         (free-total (cadr value))
         (byte-sum 0))
    (should (= (length free) 12))
    (dolist (field free)
      (should (integerp field))
      (should (>= field 0)))
    (dolist (idx '(0 2 4 6 8 10))
      (setq byte-sum (+ byte-sum (nth idx free))))
    (should (= byte-sum free-total))))

(ert-deftest nelisp-standalone-target-runtime-symbol-resolver-is-wired-through-applyfn ()
  "The runtime symbol resolver is installed once and dispatched in the reader applyfn."
  (cl-labels ((tree-member-p
               (needle tree)
               (cond
                ((equal needle tree) t)
                ((consp tree)
                 (or (tree-member-p needle (car tree))
                     (tree-member-p needle (cdr tree)))))))
    (should (= (cl-count "nelisp--runtime-symbol-address"
                         nelisp-standalone--reader-builtins
                         :test #'equal)
               1))
    (should (= (cl-count "nelisp--runtime-symbol-address"
                         nelisp-standalone--applyfn-bf-builtins
                         :test #'equal)
               0))
    (should (= (cl-count '((:lit "nelisp--runtime-symbol-address")
                           . (bf_runtime_symbol_address args out))
                         nelisp-standalone--applyfn-bf-arms
                         :test #'equal)
               1))
    (should (tree-member-p
             '((:lit "nelisp--runtime-symbol-address")
               . (bf_runtime_symbol_address args out))
             nelisp-standalone--applyfn-bf-arms))
    (should-not (tree-member-p
                 '((:lit "nelisp--runtime-symbol-address")
                   . (bf_runtime_symbol_address args out))
                 nelisp-standalone--applyfn-bf-builtins))
    (should-not (tree-member-p
                 '("runtime-symbol-address.o" nelisp-cc-nlstr-direct-ops
                   nelisp-cc-nlstr-direct-ops--runtime-symbol-address-source)
                 nelisp-standalone--reader-extra-manifest))))

(ert-deftest nelisp-standalone-target-runtime-symbol-resolver-fboundp-is-true ()
  "The standalone reader reports the resolver as a built-in function."
  (should (eq (nelisp-standalone-target-test--run-reader-value
               "(fboundp 'nelisp--runtime-symbol-address)")
              t)))

(ert-deftest nelisp-standalone-target-allocation-debt-is-registered ()
  "Standalone builds register the allocation debt builtin in reader tables."
  (should (member "nelisp--allocation-debt" nelisp-standalone--reader-builtins))
  (should (member "nelisp--allocation-debt" nelisp-standalone--applyfn-bf-builtins))
  (should (string-match-p
           (regexp-quote "(:lit \"nelisp--allocation-debt\")")
           (prin1-to-string (nelisp-standalone--applyfn-reader-table))))
  (should (string-match-p
           (regexp-quote
            "(wf_write_int out (ptr-read-u64 (data-addr nl_gc_alloc_debt) 0))")
           (nelisp-standalone-target-test--build-script-source))))

(ert-deftest nelisp-standalone-target-allocation-debt-direct-runtime ()
  "The built standalone runtime reports an integer debt that grows after allocation."
  (nelisp-standalone-target-test--ensure-standalone-reader)
  (let* ((value (nelisp-standalone-target-test--run-reader-value
                 "(let* ((before (nelisp--allocation-debt))
                         (_a (make-string 8192 65))
                         (_b (make-vector 1024 nil))
                         (after (nelisp--allocation-debt)))
                    (list (integerp before) before
                          (integerp after) after
                          (>= after before) (> after before)))")))
    (should (eq (nth 0 value) t))
    (should (integerp (nth 1 value)))
    (should (>= (nth 1 value) 0))
    (should (eq (nth 2 value) t))
    (should (integerp (nth 3 value)))
    (should (>= (nth 3 value) 0))
    (should (eq (nth 4 value) t))
    (should (eq (nth 5 value) t))))

(ert-deftest nelisp-standalone-target-runtime-symbol-resolver-honors-whitelist ()
  "The resolver returns live addresses for every whitelisted runtime bridge."
  (let* ((values
          (nelisp-standalone-target-test--run-reader-value
           "(list (nelisp--runtime-symbol-address \"nl_alloc_symbol\")
                  (nelisp--runtime-symbol-address \"nl_alloc_str\")
                  (nelisp--runtime-symbol-address \"nl_alloc_mut_str\")
                  (nelisp--runtime-symbol-address \"nl_mut_str_push_byte\")
                  (nelisp--runtime-symbol-address \"nl_mut_str_finalize\")
                  (nelisp--runtime-symbol-address \"nl_vector_slot_ptr\")
                  (nelisp--runtime-symbol-address \"nl_val_clone_into\")
                  (nelisp--runtime-symbol-address \"nelisp_env_lookup_value\")
                  (nelisp--runtime-symbol-address \"nl_alloc_consbox\")
                  (nelisp--runtime-symbol-address \"nelisp_env_set_value\")
                  (nelisp--runtime-symbol-address \"nl_alloc_vector\")
                  (nelisp--runtime-symbol-address \"nl_alloc_bytes\")
                  (nelisp--runtime-symbol-address \"nl_vector_set_slot\")
                  (nelisp--runtime-symbol-address \"nelisp_aot_builtin_call1\")
                  (nelisp--runtime-symbol-address \"nelisp_aot_builtin_calln\")
                  (nelisp--runtime-symbol-address \"nelisp_aot_errorn\")
                  (nelisp--runtime-symbol-address \"nelisp_aot_funcall1\")
                  (nelisp--runtime-symbol-address \"nelisp_aot_funcall2\")
                  (nelisp--runtime-symbol-address \"nelisp_aot_funcall3\")
                  (nelisp--runtime-symbol-address \"nelisp_aot_funcalln\")
                  (nelisp--runtime-symbol-address \"nelisp_aot_apply\")
                  (nelisp--runtime-symbol-address \"nelisp_aot_applyn\")
                  (nelisp--runtime-symbol-address \"nelisp_aot_listn\"))")))
    (should (= (length values) 23))
    (should (= (length values) (length (delete-dups (copy-sequence values)))))
    (dolist (value values)
      (should (integerp value))
      (should (> value 4096)))))

(ert-deftest nelisp-standalone-target-runtime-symbol-raw-bridge-arities ()
  "Raw resolver wrappers preserve each linked generated ABI signature."
  (let ((specs
         '((nl_runtime_symbol_address_vector_slot_ptr_bridge
            (sexp-ptr idx)
            (extern-call nl_vector_slot_ptr sexp-ptr idx))
           (nl_runtime_symbol_address_val_clone_into_bridge
            (src-slot dst-word-ptr)
            (extern-call nl_val_clone_into src-slot dst-word-ptr))
           (nl_runtime_symbol_address_env_lookup_value_bridge
            (mirror-ptr frames-ptr name-ptr out-ptr)
            (extern-call nelisp_env_lookup_value
                         mirror-ptr frames-ptr name-ptr out-ptr))
           (nl_runtime_symbol_address_alloc_consbox_bridge
            nil
            (extern-call nl_alloc_consbox))
           (nl_runtime_symbol_address_env_set_value_bridge
            (mirror-ptr frames-ptr name-ptr val-ptr scratch-ptr pad)
            (extern-call nelisp_env_set_value
                         mirror-ptr frames-ptr name-ptr val-ptr
                         scratch-ptr pad))
           (nl_runtime_symbol_address_alloc_vector_bridge
            (capacity)
            (extern-call nl_alloc_vector capacity))
           (nl_runtime_symbol_address_alloc_bytes_bridge
            (size align)
            (extern-call nl_alloc_bytes size align))
           (nl_runtime_symbol_address_vector_set_slot_bridge
            (vec-ptr idx val-ptr)
            (extern-call nl_vector_set_slot vec-ptr idx val-ptr)))))
    (dolist (spec specs)
      (let ((form (nelisp-standalone-target-test--find-defun
                   nelisp-standalone--applyfn-bf-helpers (nth 0 spec))))
        (should form)
        (should (equal (nth 2 form) (nth 1 spec)))
        (should (equal (nth 3 form) (nth 2 spec)))))))

(ert-deftest nelisp-standalone-target-runtime-symbol-call-bridge-arities ()
  "Ordinary call bridges preserve the Doc 129.7 native ABI signatures."
  (let ((specs
         '((nelisp_aot_funcall1
            (mirror frames fn arg out scratch))
           (nelisp_aot_funcall2
            (mirror frames fn arg0 arg1 out))
           (nelisp_aot_funcall3
            (mirror frames fn arg0 arg1 arg2 out))
           (nelisp_aot_funcalln
            (mirror frames fn argc out scratch a0 a1 a2 a3 a4 a5 a6 a7))
           (nelisp_aot_apply
            (mirror frames fn args-list out scratch))
           (nelisp_aot_applyn
            (mirror frames fn argc out scratch a0 a1 a2 a3 a4 a5 a6 a7))
           (nelisp_aot_listn
            (mirror frames argc out scratch a0 a1 a2 a3 a4 a5 a6 a7)))))
    (dolist (spec specs)
      (let ((form (nelisp-standalone-target-test--find-defun
                   nelisp-standalone--applyfn-bf-helpers (car spec))))
        (should form)
        (should (equal (nth 2 form) (cadr spec)))))
    (let ((source (prin1-to-string nelisp-standalone--applyfn-bf-helpers)))
      (dolist (pattern '("(nl_root_track out)"
                         "(nl_root_track args-slot)"
                         "(nl_apply_do_funcall full-slot mirror out)"
                         "(nl_apply_do_apply full-slot mirror out)"
                         "(> argc 8)"))
        (should (string-match-p (regexp-quote pattern) source))))))

(ert-deftest nelisp-standalone-target-runtime-symbol-raw-unit-generation ()
  "The applyfn unit exports every address target and relocates real callees."
  (let* ((unit
          (nelisp-standalone--compile-to-unit
           "resolver-host-check.o"
           (nelisp-standalone--applyfn-source)))
         (symbols
          (mapcar (lambda (entry) (plist-get entry :name))
                  (plist-get unit :symbols)))
         (relocs
          (mapcar (lambda (entry) (plist-get entry :symbol))
                  (plist-get unit :relocs))))
    (dolist
        (name '("nelisp_aot_errorn"
                "nelisp_aot_funcall1" "nelisp_aot_funcall2"
                "nelisp_aot_funcall3" "nelisp_aot_funcalln"
                "nelisp_aot_apply" "nelisp_aot_applyn"
                "nelisp_aot_listn"
                "nl_runtime_symbol_address_vector_slot_ptr_bridge"
                "nl_runtime_symbol_address_val_clone_into_bridge"
                "nl_runtime_symbol_address_env_lookup_value_bridge"
                "nl_runtime_symbol_address_alloc_consbox_bridge"
                "nl_runtime_symbol_address_env_set_value_bridge"
                "nl_runtime_symbol_address_alloc_vector_bridge"
                "nl_runtime_symbol_address_alloc_bytes_bridge"
                "nl_runtime_symbol_address_vector_set_slot_bridge"))
      (should (member name symbols)))
    (dolist
        (name '("nl_vector_slot_ptr" "nl_val_clone_into"
                "nelisp_env_lookup_value" "nl_alloc_consbox"
                "nelisp_env_set_value" "nl_alloc_vector"
                "nl_alloc_bytes" "nl_vector_set_slot"
                "nl_apply_do_funcall" "nl_apply_do_apply"
                "nl_root_track" "nl_root_release"))
      (should (member name relocs)))))

(ert-deftest nelisp-standalone-target-runtime-symbol-raw-unavailable-classified ()
  "Known bootstrap ABIs absent from the real reader binary remain hard errors."
  (let ((source
         (prin1-to-string nelisp-standalone--applyfn-bf-helpers)))
    (dolist
        (name '("nelisp_aot_make_closure"
                "nelisp_aot_pop_handler" "nelisp_aot_push_catch"
                "nelisp_aot_push_condition" "nelisp_aot_signal"
                "nelisp_aot_throw"))
      (should
       (string-match-p
        (regexp-quote (format "(sexp-name-eq name \"%s\")" name))
        source)))
    (should
     (string-match-p
      (regexp-quote
       "(bf_runtime_symbol_error name)")
      source))))

(ert-deftest nelisp-standalone-target-runtime-symbol-resolver-ptr-call-raw-small ()
  "Resolved allocation/value/vector ABIs are callable with their real contracts."
  ;; Keep each raw-pointer lifetime inside the smallest possible target
  ;; process.  A pointer returned through `ptr-call' is represented as an
  ;; Elisp integer and is deliberately not a GC root across later allocations.
  (should
   (equal
    (nelisp-standalone-target-test--run-reader-value
     "(let* ((addr (nelisp--runtime-symbol-address \"nl_alloc_bytes\"))
             (raw (ptr-call addr 16 8 0 0 0 0)))
        (progn
          (ptr-write-u64 raw 0 123456)
          (ptr-read-u64 raw 0)))")
    123456))
  (should
   (equal
    (nelisp-standalone-target-test--run-reader-value
     "(let* ((addr (nelisp--runtime-symbol-address \"nl_alloc_consbox\"))
             (box (ptr-call addr 0 0 0 0 0 0)))
        (list (ptr-read-u64 box 0)
              (ptr-read-u64 box 8)
              (ptr-read-u64 box 16)))")
    '(3 3 1)))
  (should
   (equal
    (nelisp-standalone-target-test--run-reader-value
     "(let* ((addr (nelisp--runtime-symbol-address \"nl_val_clone_into\"))
             (src (alloc-bytes 32 8))
             (dst (alloc-bytes 8 8)))
        (progn
          (ptr-write-u64 src 0 2)
          (ptr-write-u64 (+ src 8) 0 42)
          (list (ptr-call addr src dst 0 0 0 0)
                (ptr-read-u64 dst 0))))")
    '(169 169)))
  (should
   (equal
    (nelisp-standalone-target-test--run-reader-value
     "(let* ((alloc-addr
              (nelisp--runtime-symbol-address \"nl_alloc_vector\"))
             (set-addr
              (nelisp--runtime-symbol-address \"nl_vector_set_slot\"))
             (slot-addr
              (nelisp--runtime-symbol-address \"nl_vector_slot_ptr\"))
             (vecbox (ptr-call alloc-addr 2 0 0 0 0 0))
             (intslot (alloc-bytes 32 8))
             (vecslot (alloc-bytes 32 8)))
        (progn
          (ptr-write-u64 intslot 0 2)
          (ptr-write-u64 (+ intslot 8) 0 42)
          (ptr-write-u64 vecslot 0 8)
          (ptr-write-u64 (+ vecslot 8) 0 vecbox)
          (let ((set-word (ptr-call set-addr vecbox 0 intslot 0 0 0)))
            (let ((item (ptr-call slot-addr vecslot 0 0 0 0 0)))
              (list set-word
                    (ptr-read-u64 item 0)
                    (ptr-read-u64 (+ item 8) 0))))))")
    '(169 2 42))))

(ert-deftest nelisp-standalone-target-runtime-symbol-call-bridges-ptr-call-small ()
  "The shared funcall/apply/listn cores execute real boxed values."
  ;; Raw pointers represented as Lisp integers are not GC roots in the test
  ;; surface, so disable collection while the harness assembles a `(builtin
  ;; 1+)' function object.  The bridges themselves install their own rootstack
  ;; frame around every intermediate list allocation.
  (should
   (equal
    (nelisp-standalone-target-test--run-reader-value
     "(progn
        (nelisp--debug-switch 7)
        (let* ((symaddr
                (nelisp--runtime-symbol-address \"nl_alloc_symbol\"))
               (consaddr
                (nelisp--runtime-symbol-address \"nl_alloc_consbox\"))
               (cloneaddr
                (nelisp--runtime-symbol-address \"nl_val_clone_into\"))
               (calladdr
                (nelisp--runtime-symbol-address \"nelisp_aot_funcall1\"))
               (applyaddr
                (nelisp--runtime-symbol-address \"nelisp_aot_apply\"))
               (bb (alloc-bytes 8 1))
               (nb (alloc-bytes 8 1))
               (bs (alloc-bytes 32 8))
               (ns (alloc-bytes 32 8))
               (nilp (alloc-bytes 32 8))
               (inner (alloc-bytes 32 8))
               (func (alloc-bytes 32 8))
               (arg (alloc-bytes 32 8))
               (tail (alloc-bytes 32 8))
               (out1 (alloc-bytes 32 8))
               (out2 (alloc-bytes 32 8))
               (scratch (alloc-bytes 32 8))
               (ibox (ptr-call consaddr 0 0 0 0 0 0))
               (fbox (ptr-call consaddr 0 0 0 0 0 0))
               (tbox (ptr-call consaddr 0 0 0 0 0 0)))
          (progn
            (ptr-write-u64 bb 0 31078196194145634)
            (ptr-write-u8 nb 0 49)
            (ptr-write-u8 nb 1 43)
            (ptr-call symaddr bb 7 bs 0 0 0)
            (ptr-call symaddr nb 2 ns 0 0 0)
            (ptr-write-u64 nilp 0 0)
            (ptr-call cloneaddr ns ibox 0 0 0 0)
            (ptr-call cloneaddr nilp (+ ibox 8) 0 0 0 0)
            (ptr-write-u64 inner 0 7)
            (ptr-write-u64 (+ inner 8) 0 ibox)
            (ptr-call cloneaddr bs fbox 0 0 0 0)
            (ptr-call cloneaddr inner (+ fbox 8) 0 0 0 0)
            (ptr-write-u64 func 0 7)
            (ptr-write-u64 (+ func 8) 0 fbox)
            (ptr-write-u64 arg 0 2)
            (ptr-write-u64 (+ arg 8) 0 41)
            (ptr-call cloneaddr arg tbox 0 0 0 0)
            (ptr-call cloneaddr nilp (+ tbox 8) 0 0 0 0)
            (ptr-write-u64 tail 0 7)
            (ptr-write-u64 (+ tail 8) 0 tbox)
            (ptr-call calladdr 0 0 func arg out1 scratch)
            (ptr-call applyaddr 0 0 func tail out2 scratch)
            (list (ptr-read-u64 out1 0)
                  (ptr-read-u64 (+ out1 8) 0)
                  (ptr-read-u64 out2 0)
                  (ptr-read-u64 (+ out2 8) 0)))))")
    '(2 42 2 42)))
  (should
   (equal
    (nelisp-standalone-target-test--run-reader-value
     "(progn
        (nelisp--debug-switch 7)
        (let* ((addr
                (nelisp--runtime-symbol-address \"nelisp_aot_listn\"))
               (out (alloc-bytes 32 8))
               (scratch (alloc-bytes 32 8))
               (arg (alloc-bytes 32 8)))
          (progn
            (ptr-write-u64 arg 0 2)
            (ptr-write-u64 (+ arg 8) 0 42)
            (ptr-call addr 0 0 1 out scratch arg)
            (let ((box (ptr-read-u64 (+ out 8) 0)))
              (list (ptr-read-u64 out 0)
                    (ptr-read-u64 box 0)
                    (ptr-read-u64 box 8))))))")
    '(7 169 3)))
  (should
   (equal
    (nelisp-standalone-target-test--run-reader-value
     "(progn
        (nelisp--debug-switch 7)
        (let* ((addr
                (nelisp--runtime-symbol-address \"nelisp_aot_listn\"))
               (out (alloc-bytes 32 8))
               (scratch (alloc-bytes 32 8)))
          (ptr-call addr 0 0 9 out scratch 0)))")
    1)))

(ert-deftest nelisp-standalone-target-runtime-symbol-resolver-ptr-call-alloc-str ()
  "Calling the resolved `nl_alloc_str' bridge must materialize the expected string."
  (let ((value
         (nelisp-standalone-target-test--run-reader-value
          "(let* ((bytes (alloc-bytes 5 1))
                  (out (alloc-bytes 32 8))
                  (addr (nelisp--runtime-symbol-address \"nl_alloc_str\")))
             (progn
               (ptr-write-u8 bytes 0 104)
               (ptr-write-u8 bytes 1 101)
               (ptr-write-u8 bytes 2 108)
               (ptr-write-u8 bytes 3 108)
               (ptr-write-u8 bytes 4 111)
               (ptr-call addr bytes 5 out 0 0 0)
               (list (ptr-read-u64 out 0)
                     (ptr-read-u64 out 8)
                     (ptr-read-u64 out 24)
                     (ptr-read-u8 (ptr-read-u64 out 16) 0)
                     (ptr-read-u8 (ptr-read-u64 out 16) 1)
                     (ptr-read-u8 (ptr-read-u64 out 16) 2)
                     (ptr-read-u8 (ptr-read-u64 out 16) 3)
                     (ptr-read-u8 (ptr-read-u64 out 16) 4))))")))
    (should (equal value '(5 5 5 104 101 108 108 111)))))

(ert-deftest nelisp-standalone-target-runtime-symbol-resolver-rejects-unknown ()
  "Unknown runtime extern names must fail through the real error path."
  (let* ((result (nelisp-standalone-target-test--run-reader-src
                  "(nelisp--runtime-symbol-address \"no-such-runtime-extern\")"))
         (exit (plist-get result :exit))
         (stderr (plist-get result :stderr)))
    (should (/= exit 0))
    (should (> (length stderr) 0))))

(ert-deftest nelisp-standalone-target-runtime-symbol-unavailable-errors-cleanly ()
  "Known but unlinked runtime ABIs must raise normally, never segfault."
  (dolist
      (name '("nelisp_aot_make_closure"
              "nelisp_aot_pop_handler" "nelisp_aot_push_catch"
              "nelisp_aot_push_condition" "nelisp_aot_signal"
              "nelisp_aot_throw"))
    (let* ((source
            (format "(nelisp--runtime-symbol-address %S)" name))
           (result (nelisp-standalone-target-test--run-reader-src source))
           (exit (plist-get result :exit))
           (stderr (plist-get result :stderr)))
      (should (= exit 1))
      (should (string-match-p "uncaught error: error:" stderr))
      (should (string-match-p (regexp-quote name) stderr)))))

(ert-deftest nelisp-standalone-target-prelude-base64-decodes-raw-bytes ()
  "The prelude fallback base64 decoder must preserve raw byte values."
  (let* ((payload "AF WJ/w==\n")
         (expected (unibyte-string 0 85 137 255))
         (host-base64-decode-bytes (and (fboundp 'nelisp--base64-decode-bytes)
                                        (symbol-function 'nelisp--base64-decode-bytes))))
    (unwind-protect
        (progn
          (dolist (pattern '("(defun nelisp--base64-decode-bytes"
                             "(unless (fboundp 'nelisp--base64-value)"
                             "(unless (fboundp 'nelisp--base64-flush-chunk)"
                             "(defun nelisp--string-byte-at"
                             "(unless (fboundp 'base64-encode-string)"))
            (eval (nelisp-standalone-target-test--read-prelude-form pattern)))
          (let ((decoded (nelisp--base64-decode-bytes payload)))
            (should (= (string-bytes decoded) 4))
            (should (equal decoded expected))
            (should (= (nelisp--string-byte-at decoded 0) 0))
            (should (= (nelisp--string-byte-at decoded 1) 85))
            (should (= (nelisp--string-byte-at decoded 2) 137))
            (should (= (nelisp--string-byte-at decoded 3) 255))))
      (if host-base64-decode-bytes
          (fset 'nelisp--base64-decode-bytes host-base64-decode-bytes)
        (fmakunbound 'nelisp--base64-decode-bytes)))))

(ert-deftest nelisp-standalone-target-prelude-base64-dispatches-native-when-available ()
  "The public prelude decoder must prefer the native builtin when present."
  (let ((host-base64-decode-bytes (and (fboundp 'nelisp--base64-decode-bytes)
                                       (symbol-function 'nelisp--base64-decode-bytes))))
    (unwind-protect
        (progn
          (eval (nelisp-standalone-target-test--read-prelude-form
                 "(defun nelisp--base64-decode-bytes"))
          (nelisp-standalone-target-test--with-temporary-fdefinition
           'nelisp--base64-decode-native
           (lambda (_string) "native-dispatch")
           (lambda ()
             (should (equal (nelisp--base64-decode-bytes "TQ==")
                            "native-dispatch")))))
      (if host-base64-decode-bytes
          (fset 'nelisp--base64-decode-bytes host-base64-decode-bytes)
        (fmakunbound 'nelisp--base64-decode-bytes))
      (fmakunbound 'nelisp--base64-decode-native))))

(ert-deftest nelisp-standalone-target-base64-native-wiring-exists ()
  "The reader builtin table and dispatch arm include the native base64 decoder."
  (should (member "nelisp--base64-decode-native"
                  nelisp-standalone--reader-builtins))
  (should (string-match-p
           (regexp-quote "(:lit \"nelisp--base64-decode-native\")")
           (prin1-to-string (nelisp-standalone--applyfn-reader-table))))
  (should (string-match-p
           (regexp-quote "(defun m5_base64_decode_native")
           (prin1-to-string nelisp-standalone--applyfn-m5-helpers))))

(ert-deftest nelisp-standalone-target-base64-native-roots-mutstr-temporary ()
  "The native base64 MutStr temporary lives in a nested-safe root frame."
  (let* ((form (cl-find-if
                (lambda (item)
                  (and (consp item)
                       (eq (car item) 'defun)
                       (eq (cadr item) 'm5_base64_decode_native)))
                nelisp-standalone--applyfn-m5-helpers))
         (source (prin1-to-string form))
         (init (string-match (regexp-quote "(nl_rootstack_init)") source))
         (mark (string-match (regexp-quote "(marker (nl_root_mark))") source))
         (reserve (string-match (regexp-quote "(ms (nl_root_reserve))") source))
         (make (string-match (regexp-quote "(mut-str-make-empty ms") source))
         (finalize (string-match (regexp-quote "(mut-str-finalize ms out)") source))
         (release (string-match (regexp-quote "(nl_root_release marker)") source)))
    (should form)
    (dolist (position (list init mark reserve make finalize release))
      (should (integerp position)))
    (should (< init mark))
    (should (< mark reserve))
    (should (< reserve make))
    (should (< make finalize))
    (should (< finalize release))
    (should-not (string-match-p
                 (regexp-quote "(ms (alloc-bytes 32 8))")
                 source))))

(ert-deftest nelisp-standalone-target-source-container-end-wiring-exists ()
  "The reader builtin table and dispatch arm include the source container scan."
  (should (member "nelisp--source-container-end"
                  nelisp-standalone--reader-builtins))
  (should (string-match-p
           (regexp-quote "(:lit \"nelisp--source-container-end\")")
           (prin1-to-string (nelisp-standalone--applyfn-reader-table))))
  (should (string-match-p
           (regexp-quote "(defun m5_source_container_end")
           (prin1-to-string nelisp-standalone--applyfn-m5-helpers))))

(ert-deftest nelisp-standalone-target-rd-string-end-wiring-exists ()
  "The reader builtin table and dispatch arm include the string terminator scan."
  (should (member "nelisp--rd-string-end-native"
                  nelisp-standalone--reader-builtins))
  (should (member "nelisp--rd-string-end"
                  nelisp-standalone--reader-builtins))
  (should (string-match-p
           (regexp-quote "(:lit \"nelisp--rd-string-end-native\")")
           (prin1-to-string (nelisp-standalone--applyfn-reader-table))))
  (should (string-match-p
           (regexp-quote "(:lit \"nelisp--rd-string-end\")")
           (prin1-to-string (nelisp-standalone--applyfn-reader-table))))
  (should (string-match-p
           (regexp-quote "(defun bf_rd_string_end")
           (prin1-to-string nelisp-standalone--applyfn-m5-helpers))))

(ert-deftest nelisp-standalone-target-rd-string-end-native-direct-builtin-behavior ()
  "The native alias is bound in the built reader and matches the legacy entry."
  (nelisp-standalone-target-test--ensure-standalone-reader)
  (let* ((escaped "\"a\\\"b\"")
         (source
          (format
           "(let* ((escaped %S)
                   (body (make-string 600000 97))
                   (large (concat \"\\\"\" body \"\\\"\")))
              (list (fboundp 'nelisp--rd-string-end-native)
                    (nelisp--rd-string-end-native escaped 1 (length escaped))
                    (nelisp--rd-string-end escaped 1 (length escaped))
                    (nelisp--rd-string-end-native large 1 (length large))
                    (nelisp--rd-string-end large 1 (length large))))"
           escaped))
         (value (nelisp-standalone-target-test--run-reader-value source)))
    (should (equal value '(t (5 . t) (5 . t) (600001) (600001))))))

(ert-deftest nelisp-standalone-target-read-batch-native-wiring-exists ()
  "The reader builtin table and dispatch arm include the batch native reader."
  (should (member "nelisp--read-batch-from-string-native"
                  nelisp-standalone--reader-builtins))
  (should (member "nelisp--read-batch-vector-from-string-native"
                  nelisp-standalone--reader-builtins))
  (should (string-match-p
           (regexp-quote "(:lit \"nelisp--read-batch-from-string-native\")")
           (prin1-to-string (nelisp-standalone--applyfn-reader-table))))
  (should
   (string-match-p
    (regexp-quote
     "(:lit \"nelisp--read-batch-vector-from-string-native\")")
    (prin1-to-string (nelisp-standalone--applyfn-reader-table)))))

(ert-deftest nelisp-standalone-target-read-batch-native-bounded-cursor ()
  "The batch reader preserves order, stops at MAX-ITEMS, and uses byte cursors."
  (nelisp-standalone-target-test--ensure-standalone-reader)
  (let* ((f1 "(alpha (beta gamma))")
         (f2 "\"a\\\\b\"")
         (f3 "'foo")
         (f4 "\"あ\"")
         (src (concat f1 " " f2 " " f3 " " f4))
         (c1 (string-bytes f1))
         (c2 (+ c1 1 (string-bytes f2)))
         (c3 (+ c2 1 (string-bytes f3)))
         (c4 (+ c3 1 (string-bytes f4)))
         (value
          (nelisp-standalone-target-test--run-reader-value
           (format
            "(let* ((src %S)
                    (r1 (nelisp--read-batch-from-string-native src 0 2))
                    (r2 (nelisp--read-batch-from-string-native src (cdr r1) 2))
                    (r3 (nelisp--read-batch-from-string-native src (cdr r2) 2)))
               (list (car r1) (cdr r1) (car r2) (cdr r2) (car r3) (cdr r3)))"
            src))))
    (should (< 0 c1 c2 c3 c4))
    (should (equal value
                   (list (list '(alpha (beta gamma)) "a\\b")
                         c2
                         (list '(quote foo) "あ")
                         c4
                         nil
                         c4)))))

(ert-deftest nelisp-standalone-target-read-batch-native-preserves-32-relocations ()
  "Recursive batch construction retains every relocation plist in order."
  (nelisp-standalone-target-test--ensure-standalone-reader)
  (let* ((src
          (mapconcat
           (lambda (n)
             (format "(:offset %d :type plt32 :symbol \"reloc-%02d\" :addend -4)"
                     (* n 16) n))
           (number-sequence 0 31)
           " "))
         (value
          (nelisp-standalone-target-test--run-reader-value
           (format
            "(let* ((src %S)
                    (r (nelisp--read-batch-from-string-native src 0 32))
                    (items (car r))
                    (first (nth 0 items))
                    (last-item (nth 31 items)))
               (list (length items)
                     (plist-get first :offset)
                     (plist-get first :symbol)
                     (plist-get last-item :offset)
                     (plist-get last-item :symbol)
                     (cdr r)))"
            src))))
    (should (equal value
                   (list 32 0 "reloc-00" 496 "reloc-31"
                         (string-bytes src))))))

(ert-deftest nelisp-standalone-target-read-batch-vector-native-preserves-32-relocations ()
  "Vector batch transport retains 32 relocation plists across explicit GC."
  (nelisp-standalone-target-test--ensure-standalone-reader)
  (let* ((src
          (mapconcat
           (lambda (n)
             (format "(:offset %d :type plt32 :symbol \"reloc-%02d\" :addend -4)"
                     (* n 16) n))
           (number-sequence 0 31)
           " "))
         (value
          (nelisp-standalone-target-test--run-reader-value
           (format
            "(let* ((src %S)
                    (i 0)
                    (r nil)
                    (first nil)
                    (last-item nil))
               (while (< i 80)
                 (setq r
                       (nelisp--read-batch-vector-from-string-native src 0 32))
                 (garbage-collect)
                 (setq i (1+ i)))
               (setq first (aref r 2))
               (setq last-item (aref r 33))
               (list (length r)
                     (aref r 0)
                     (aref r 1)
                     (plist-get first :offset)
                     (plist-get first :symbol)
                     (plist-get last-item :offset)
                     (plist-get last-item :symbol)))"
            src))))
    (should (equal value
                   (list 34 (string-bytes src) 32
                         0 "reloc-00" 496 "reloc-31")))))

(ert-deftest nelisp-standalone-target-read-batch-native-rejects-bad-max-items ()
  "The batch reader rejects zero MAX-ITEMS through the normal type error path."
  (nelisp-standalone-target-test--ensure-standalone-reader)
  (let* ((result (nelisp-standalone-target-test--run-reader-src
                  "(nelisp--read-batch-from-string-native \"(a)\" 0 0)"))
         (exit (plist-get result :exit)))
    (should (/= exit 0))))

(ert-deftest nelisp-standalone-target-read-batch-native-dynamic-cap-small-input ()
  "Batch reader uses dynamic cap scaled to remaining bytes, not fixed 262144.
For a 64-byte input, cap should be max(256, 4*64)=256 not 262144."
  (nelisp-standalone-target-test--ensure-standalone-reader)
  (let* ((src "(a) (b) (c) (d) (e) (f) (g) (h) (i) (j) (k) (l) (m) (n)")
         (value
          (nelisp-standalone-target-test--run-reader-value
           (format
            "(let* ((src %S)
                    (r1 (nelisp--read-batch-from-string-native src 0 5))
                    (r2 (nelisp--read-batch-from-string-native src (cdr r1) 5)))
               (list (length (car r1)) (cdr r1) (length (car r2)) (cdr r2)))"
            src))))
    (should (equal (nth 0 value) 5))
    (should (> (nth 1 value) 0))
    (should (equal (nth 2 value) 5))
    (should (> (nth 3 value) (nth 1 value)))))

(ert-deftest nelisp-standalone-target-read-batch-native-repeated-tiny-reads ()
  "Tiny batch reads preserve order without the historical 8MiB pool debt."
  (nelisp-standalone-target-test--ensure-standalone-reader)
  (let* ((forms (mapconcat (lambda (n) (format "%d" n)) (number-sequence 0 19) " "))
         (value
          (nelisp-standalone-target-test--run-reader-value
           (format
            "(let* ((src %S)
                    (before (nelisp--allocation-debt))
                    (r0 (nelisp--read-batch-from-string-native src 0 1))
                    (r1 (nelisp--read-batch-from-string-native src (cdr r0) 1))
                    (r2 (nelisp--read-batch-from-string-native src (cdr r1) 1))
                    (r3 (nelisp--read-batch-from-string-native src (cdr r2) 1))
                    (r4 (nelisp--read-batch-from-string-native src (cdr r3) 1))
                    (debt (- (nelisp--allocation-debt) before)))
               (list (car (car r0)) (car (car r1)) (car (car r2))
                     (car (car r3)) (car (car r4)) debt))"
            forms))))
    (should (equal (butlast value) '(0 1 2 3 4)))
    ;; Five historical fixed pools alone requested 40 MiB.  Leave a generous
    ;; 1 MiB ceiling for result cloning and call scaffolding.
    (should (< (car (last value)) 1048576))))

(ert-deftest nelisp-standalone-target-read-batch-native-cursor-fidelity ()
  "Byte cursor advances correctly with dynamic cap for various input sizes."
  (nelisp-standalone-target-test--ensure-standalone-reader)
  (let* ((f1 "(x)")
         (f2 "(y)")
         (f3 "(z)")
         (src (concat f1 " " f2 " " f3))
         (c1 (string-bytes f1))
         (c2 (+ c1 1 (string-bytes f2)))
         (value
          (nelisp-standalone-target-test--run-reader-value
           (format
            "(let* ((src %S)
                    (r1 (nelisp--read-batch-from-string-native src 0 1))
                    (cur1 (cdr r1))
                    (r2 (nelisp--read-batch-from-string-native src cur1 1))
                    (cur2 (cdr r2)))
               (list cur1 cur2))"
            src))))
    (should (= (nth 0 value) c1))
    (should (= (nth 1 value) c2))))

(ert-deftest nelisp-standalone-target-prelude-base64-uses-raw-bytes ()
  "The prelude fallback base64 encoder must match host Emacs on byte payloads."
  (let* ((payload (unibyte-string 208 0 255 65 66))
         (host-base64 (symbol-function 'base64-encode-string))
         (host-byte-at (and (fboundp 'nelisp--string-byte-at)
                            (symbol-function 'nelisp--string-byte-at))))
    (unwind-protect
        (progn
          (fmakunbound 'base64-encode-string)
          (when (fboundp 'nelisp--string-byte-at)
            (fmakunbound 'nelisp--string-byte-at))
          (dolist (pattern '("(unless (fboundp 'nelisp--base64-value)"
                             "(unless (fboundp 'nelisp--base64-flush-chunk)"
                             "(defun nelisp--string-byte-at"
                             "(unless (fboundp 'base64-encode-string)"))
            (eval (nelisp-standalone-target-test--read-prelude-form pattern)))
          (should (equal (base64-encode-string payload)
                         (funcall host-base64 payload))))
      (fset 'base64-encode-string host-base64)
      (when host-byte-at
        (fset 'nelisp--string-byte-at host-byte-at)))))

(ert-deftest nelisp-standalone-target-prelude-rd-string-end-delegates-to-native-on-host ()
  "When the native alias is present, the host wrapper must delegate once."
  (nelisp-standalone-target-test--with-prelude-rd-core
   (lambda ()
     (let ((native-calls 0)
           (search-calls 0)
           seen-args)
       (nelisp-standalone-target-test--with-temporary-fdefinition
        'nelisp--rd-string-end-native
        (lambda (s start n)
          (setq native-calls (1+ native-calls)
                seen-args (list s start n))
          (cons 123 t))
        (lambda ()
          (nelisp-standalone-target-test--with-temporary-fdefinition
           'nelisp--string-search
           (lambda (&rest _)
             (setq search-calls (1+ search-calls))
             (error "fallback string-search should not be called"))
           (lambda ()
             (should (equal (nelisp--rd-string-end "abc" 1 3) '(123 . t)))
             (should (= native-calls 1))
             (should (equal seen-args '("abc" 1 3)))
             (should (= search-calls 0))))))))))

(ert-deftest nelisp-standalone-target-base64-decodes-direct-target-inputs ()
  "The built standalone reader should decode canonical and large inputs natively."
  (nelisp-standalone-target-test--ensure-standalone-reader)
  (dolist (case '(("TQ==" . "M")
                  ("TWE=" . "Ma")))
    (let ((decoded (nelisp-standalone-target-test--run-reader-value
                    (format "(nelisp--base64-decode-bytes %S)" (car case)))))
      (should (equal decoded (cdr case)))))
  (let* ((bytes nil)
         (i 0))
    (while (< i 8192)
      (push (logand (+ (* i 73) 19) 255) bytes)
      (setq i (1+ i)))
    (let* ((raw (apply #'unibyte-string (nreverse bytes)))
           (encoded (base64-encode-string raw t))
           (decoded (nelisp-standalone-target-test--run-reader-value
                     (format "(nelisp--base64-decode-bytes %S)" encoded))))
      (should (equal decoded raw))
      (should (= (string-bytes decoded) (string-bytes raw))))))

(ert-deftest nelisp-standalone-target-reader-preserves-symbol-shaped-numerics-in-built-binary ()
  "The built standalone reader keeps `1-' and `1+' as symbols and still runs `1-'."
  (nelisp-standalone-target-test--ensure-standalone-reader)
  (let ((value
         (nelisp-standalone-target-test--run-reader-value
          "(list (car (read-from-string \"1-\"))
                 (cdr (read-from-string \"1-\"))
                 (car (read-from-string \"1+\"))
                 (cdr (read-from-string \"1+\"))
                 (progn
                   (defun nelisp-standalone-target-test--dec1 (skip)
                     (1- skip))
                   (nelisp-standalone-target-test--dec1 3)))")))
    (should (equal value (list (intern "1-") 2 (intern "1+") 2 2)))))

(ert-deftest nelisp-standalone-target-reader-char-literal-regression-in-built-binary ()
  "The built standalone reader must parse plain `?X' literals and stop at atom boundaries."
  (nelisp-standalone-target-test--ensure-standalone-reader)
  (let ((value
         (nelisp-standalone-target-test--run-reader-value
          "(list (read-from-string \"?/\" 0)
                 (read-from-string \"?a\" 0)
                 (read-from-string \"?\\\\n\" 0)
                 (read-from-string \"? \" 0)
                 (car (read-from-string (concat \"?/\" (make-string 100 ?z)) 0)))")))
    (should (equal value '((47 . 2) (97 . 2) (10 . 3) (? . 1) 47)))))

(ert-deftest nelisp-standalone-target-load-char-literal-regression-across-32768-boundary ()
  "Standalone `load' must keep `?/' correct on both sides of the 32768-byte floor."
  (nelisp-standalone-target-test--ensure-standalone-reader)
  (let* ((tail "(princ (format \"%S\" ?/))")
         (make-source
          (lambda (size)
            (let ((path (make-temp-file "nelisp-load-char-literal-" nil ".el"))
                  (pad (- size (length tail))))
              (with-temp-file path
                (insert (make-string pad ?\s))
                (insert tail))
              path)))
         (file-32768 (funcall make-source 32768))
         (file-32769 (funcall make-source 32769)))
    (unwind-protect
        (progn
          (dolist (path (list file-32768 file-32769))
            (let* ((result
                    (nelisp-standalone-target-test--run-reader-src
                     (format "(load %S)" path)))
                   (exit (plist-get result :exit))
                   (stdout (plist-get result :stdout))
                   (stderr (plist-get result :stderr)))
              (should (= exit 0))
              (should (string-match-p "\\`[[:space:]\n]*\\'" stderr))
              ;; `--eval' appends the command's own return value after the
              ;; file's output, so a successful load of the gate form prints
              ;; `47' followed by the load result `t' (= `47t').  The contract
              ;; under test is that the file itself prints `47' and does not
              ;; signal.
              (should (string-prefix-p "47" stdout)))))
      (when (file-exists-p file-32768)
        (delete-file file-32768))
      (when (file-exists-p file-32769)
        (delete-file file-32769)))))

(ert-deftest nelisp-standalone-target-pcase-exact-nested-pattern-direct-runtime ()
  "The built target/nelisp runtime returns the expected nested pcase structure."
  (nelisp-standalone-target-test--ensure-standalone-reader)
  (let ((value
         (nelisp-standalone-target-test--run-reader-value
          "(pcase '(let ((x 1)) (foo x))
             (`(,(and fun (or 'let 'let*)) . ,(or `(,bindings . ,body) pcase--dontcare))
              (list fun bindings body))
             (_ nil))")))
    (should (equal value '(let ((x 1)) ((foo x)))))))

(ert-deftest nelisp-standalone-target-source-container-end-canonical-case ()
  "Nested containers, strings, escapes, and comments must all be skipped."
  (nelisp-standalone-target-test--ensure-standalone-reader)
  (let* ((source (concat "("
                         "[a "
                         "\"x\\\"y[)]\""
                         " ; comment [ignored]\n "
                         "(b [c])])"))
         (end (nelisp-standalone-target-test--run-reader-value
               (format "(nelisp--source-container-end %S 0)" source))))
    (should (= end (length source)))))

(ert-deftest nelisp-standalone-target-source-container-end-honors-escaped-symbols ()
  "Escaped quote, paren, and semicolon symbols outside strings must not stop the scan."
  (nelisp-standalone-target-test--ensure-standalone-reader)
  (let* ((source (concat "("
                         "\"alpha\" "
                         "\\\" "
                         "\\) "
                         "\\; "
                         "(beta [gamma]))"))
         (end (nelisp-standalone-target-test--run-reader-value
               (format "(nelisp--source-container-end %S 0)" source))))
    (should (= end (length source)))))

(ert-deftest nelisp-standalone-target-source-container-end-unterminated-returns-nil ()
  "Unterminated input must return nil rather than overrun the buffer."
  (nelisp-standalone-target-test--ensure-standalone-reader)
  (let* ((source (concat "("
                         "[a "
                         "\"x\\\"y[)]\""
                         " ; comment [ignored]\n "
                         "(b [c])"))
         (end (nelisp-standalone-target-test--run-reader-value
               (format "(nelisp--source-container-end %S 0)" source))))
    (should-not end)))

(ert-deftest nelisp-standalone-target-source-container-end-large-ascii-body ()
  "A 500000-byte ASCII body must scan through the target and return the close."
  (nelisp-standalone-target-test--ensure-standalone-reader)
  (let* ((body (make-string 500000 ?a))
         (source (concat "(" body ")"))
         (source-file (make-temp-file "nelisp-source-container-end-" nil ".el")))
    (unwind-protect
        (progn
          (with-temp-file source-file
            (insert source))
          (should (= (nelisp-standalone-target-test--run-reader-value
                      (format "(nelisp--source-container-end (nelisp--syscall-read-file %S) 0)"
                              source-file))
                     500002)))
      (when (file-exists-p source-file)
        (delete-file source-file)))))

(ert-deftest nelisp-standalone-target-string-search-char-index-semantics ()
  "The native string-search builtin must accept and return character indices."
  (nelisp-standalone-target-test--ensure-standalone-reader)
  (dolist (case '(("ababa" "ba" 2 3)
                  ("a日本b日本" "日本" 2 4)))
    (let* ((haystack (nth 0 case))
           (needle (nth 1 case))
           (start (nth 2 case))
           (expected (nth 3 case))
           (value (nelisp-standalone-target-test--run-reader-value
                   (format "(nelisp--string-search %S %S %d)"
                           needle haystack start))))
      (should (= value expected)))))

(ert-deftest nelisp-standalone-target-source-container-end-multibyte-direct ()
  "Japanese bytes before, inside, and after strings/comments must be skipped."
  (nelisp-standalone-target-test--ensure-standalone-reader)
  (let* ((source (concat "("
                         "日本"
                         " \"x日本\" ; コメント 日本\n "
                         "(b [c]))"))
         (end (nelisp-standalone-target-test--run-reader-value
               (format "(nelisp--source-container-end %S 0)" source))))
    (should (= end (length source)))))

(ert-deftest nelisp-standalone-target-source-container-end-large-multibyte-body ()
  "A large multibyte body should scan from a temp file without argv bloat."
  (nelisp-standalone-target-test--ensure-standalone-reader)
  (let* ((chunk "日本")
         (body (apply #'concat (make-list 50000 chunk)))
         (source (concat "(" body " \"x日本\" ; コメント 日本\n)"))
         (source-file (make-temp-file "nelisp-source-container-end-" nil ".el")))
    (unwind-protect
        (progn
          (with-temp-file source-file
            (insert source))
          (should (= (nelisp-standalone-target-test--run-reader-value
                      (format "(nelisp--source-container-end (nelisp--syscall-read-file %S) 0)"
                              source-file))
                     (length source))))
      (when (file-exists-p source-file)
        (delete-file source-file)))))

(ert-deftest nelisp-standalone-target-syscall-read-file-large-temp-file-is-exact ()
  "The built standalone reader must return the entire contents of a >8 MiB file."
  (nelisp-standalone-target-test--ensure-standalone-reader)
  (let* ((prefix "BEGIN:")
         (suffix ":END")
         (body (make-string (+ (* 8 1024 1024) 4096) ?x))
         (source (concat prefix body suffix))
         (expected-len (length source))
         (source-file (make-temp-file "nelisp-syscall-read-file-" nil ".el")))
    (unwind-protect
        (progn
          (with-temp-file source-file
            (insert source))
          (let ((value
                 (nelisp-standalone-target-test--run-reader-value
                  (format "(let ((s (nelisp--syscall-read-file %S))) (list (length s) (substring s 0 %d) (substring s (- (length s) %d) (length s))))"
                          source-file
                          (length prefix)
                          (length suffix)))))
            (should (equal value (list expected-len prefix suffix)))))
      (when (file-exists-p source-file)
        (delete-file source-file)))))

(ert-deftest nelisp-standalone-target-ptr-copy-string-bytes-direct-mmap-page ()
  "The target builtin copies raw bytes into a mapped page and returns the count."
  (nelisp-standalone-target-test--ensure-standalone-reader)
  (let ((value
         (nelisp-standalone-target-test--run-reader-value
          "(let* ((page (syscall-direct 9 0 4096 3 34 -1 0))
                  (src (unibyte-string 0 128 255)))
             (list (nelisp--ptr-copy-string-bytes page src)
                   (ptr-read-u8 page 0)
                   (ptr-read-u8 page 1)
                   (ptr-read-u8 page 2)))")))
    (should (equal value '(3 0 128 255)))))

(ert-deftest nelisp-standalone-target-ptr-copy-string-bytes-direct-large ()
  "The target builtin copies a 500000-byte target-built string without argv bloat."
  (nelisp-standalone-target-test--ensure-standalone-reader)
  (let* ((source "(let* ((dst (alloc-bytes 500000 1))
                  (src (make-string 500000 97))) (list (nelisp--ptr-copy-string-bytes dst src) (ptr-read-u8 dst 0) (ptr-read-u8 dst 499999)))")
         (value (nelisp-standalone-target-test--run-reader-value source)))
    (should (equal value '(500000 97 97)))))

(ert-deftest nelisp-standalone-target-prelude-rd-unescape-preserves-literals-and-escapes ()
  "The reader escape helper must stay linear and preserve literal strings."
  (let ((literal (make-string 200000 ?a)))
    (nelisp-standalone-target-test--with-prelude-rd-unescape
     (lambda ()
       (should (eq (nelisp--rd-unescape literal) literal))
       (should (equal (nelisp--rd-unescape "\\n") "\n"))
       (should (equal (nelisp--rd-unescape "\\t") "\t"))
       (should (equal (nelisp--rd-unescape "\\r") "\r"))
       (should (equal (nelisp--rd-unescape "\\\\") "\\"))
       (should (equal (nelisp--rd-unescape "\\x") "x"))
       (should (equal (nelisp--rd-unescape "abc\\") "abc\\"))))))

(ert-deftest nelisp-standalone-target-prelude-rd-string-end-native-path-reaches-quote ()
  "The native string-search path must find long ASCII strings without escapes."
  (let ((body (make-string 200000 ?a))
        (search-calls 0)
        (host-string-search (symbol-function 'string-search)))
    (nelisp-standalone-target-test--with-prelude-rd-core
     (lambda ()
       (nelisp-standalone-target-test--with-temporary-fdefinition
        'nelisp--string-search
        (lambda (needle haystack &optional start)
          (setq search-calls (1+ search-calls))
          (funcall host-string-search needle haystack start))
        (lambda ()
          (let* ((s (concat "\"" body "\""))
                 (result (nelisp--rd-string-end s 1 (length s))))
            (should (= (car result) (1- (length s))))
            (should-not (cdr result))
            (should (= search-calls 2)))))))))

(ert-deftest nelisp-standalone-target-prelude-rd-string-end-flags-escapes-and-truncation ()
  "Escaped quotes, escaped slashes, and trailing slashes all set has-escape."
  (nelisp-standalone-target-test--with-prelude-rd-core
   (lambda ()
     (dolist (case (list (list "\"a\\\"b\"" 5 t)
                         (list "\"a\\\\b\"" 5 t)
                         (list (concat "\"" "abc" "\\") 5 t)))
       (let* ((s (car case))
              (expected-end (cadr case))
              (expected-escape (caddr case))
              (result (nelisp--rd-string-end s 1 (length s))))
         (should (= (car result) expected-end))
         (should (eq (cdr result) expected-escape)))))))

(ert-deftest nelisp-standalone-target-rd-string-end-direct-escaped-and-unterminated ()
  "The standalone reader builtin must return the end index and escape flag directly."
  (nelisp-standalone-target-test--ensure-standalone-reader)
  (let* ((escaped "\"a\\\"b\"")
         (escaped-source
          (format "(let ((s %S)) (nelisp--rd-string-end s 1 (length s)))" escaped))
         (unterminated (concat "\"" "abc"))
         (unterminated-source
          (format "(let ((s %S)) (nelisp--rd-string-end s 1 (length s)))" unterminated)))
    (should (equal (nelisp-standalone-target-test--run-reader-value escaped-source)
                   '(5 . t)))
    (should (equal (nelisp-standalone-target-test--run-reader-value unterminated-source)
                   '(4)))))

(ert-deftest nelisp-standalone-target-rd-string-end-direct-large-ascii-body ()
  "The standalone reader builtin must scan a 600000-byte ASCII body natively."
  (nelisp-standalone-target-test--ensure-standalone-reader)
  (let* ((source "(let ((body (make-string 600000 97))) (let ((s (concat \"\\\"\" body \"\\\"\"))) (nelisp--rd-string-end s 1 (length s))))")
         (value (nelisp-standalone-target-test--run-reader-value source)))
    (should (= (car value) 600001))
    (should-not (cdr value))))

(ert-deftest nelisp-standalone-target-prelude-rd-one-returns-ascii-body-without-unescape ()
  "ASCII string bodies should bypass `nelisp--rd-unescape' entirely."
  (nelisp-standalone-target-test--with-prelude-rd-core
   (lambda ()
     (nelisp-standalone-target-test--with-temporary-fdefinition
      'nelisp--rd-unescape
      (lambda (&rest _)
        (error "rd-unescape should not be called for ASCII bodies"))
      (lambda ()
        (let* ((s "\"abc\"")
               (result (nelisp--rd-one s 0 (length s))))
          (should (equal (car result) "abc"))
          (should (= (cdr result) (length s)))))))))

(ert-deftest nelisp-standalone-target-prelude-rd-numeric-token-p-is-strict-whole-token-shape ()
  "The numeric token predicate must only accept full numeric shapes."
  (nelisp-standalone-target-test--with-prelude-rd-core
   (lambda ()
     (dolist (tok '("0" "7" "+7" "-7" ".5" "-.5" "1." "3.14"
                    "1e2" "1e-2" "1E+2" "+.5" "0.e1"))
       (should (nelisp--rd-numeric-token-p tok)))
     (dolist (tok '("" "+" "-" "." "1-" "1+" "123abc" "1e" "1e+"
                    "1.2.3" "1e2.3" "1e-" "1.e" "e3" ".e3"
                    "_1" "1_2"))
       (should-not (nelisp--rd-numeric-token-p tok))))))

(ert-deftest nelisp-standalone-target-prelude-rd-one-keeps-symbol-shaped-numerics-as-symbols ()
  "Reader atoms like `1-' and `1+' must stay symbols while numeric shapes remain numbers."
  (nelisp-standalone-target-test--with-prelude-rd-core
   (lambda ()
     (let* ((minus (nelisp--rd-one "1-" 0 2))
            (plus (nelisp--rd-one "1+" 0 2))
            (int (nelisp--rd-one "42" 0 2))
            (float (nelisp--rd-one "1.5" 0 3))
            (leading-dot (nelisp--rd-one ".5" 0 2))
            (trailing-dot (nelisp--rd-one "1." 0 2))
            (exp (nelisp--rd-one "1e-2" 0 4)))
       (should (eq (car minus) (intern "1-")))
       (should (= (cdr minus) 2))
       (should (eq (car plus) (intern "1+")))
       (should (= (cdr plus) 2))
       (should (integerp (car int)))
       (should (= (car int) 42))
       (should (numberp (car float)))
       (should (= (car float) 1.5))
       (should (numberp (car leading-dot)))
       (should (= (car leading-dot) 0.5))
       (should (numberp (car trailing-dot)))
       (should (= (car trailing-dot) 1))
       (should (numberp (car exp)))
       (should (= (car exp) 0.01))))))

(ert-deftest nelisp-standalone-target-prelude-rd-one-unescapes-symbol-atoms ()
  "Escaped punctuation and whitespace stay inside one symbol token."
  (nelisp-standalone-target-test--with-prelude-rd-core
   (lambda ()
     (dolist (case '(("\\," "," 2)
                     ("\\,@" ",@" 3)
                     ("foo\\ bar" "foo bar" 8)
                     ("\\1" "1" 2)))
       (let ((result (nelisp--rd-one (car case) 0 (length (car case)))))
         (should (symbolp (car result)))
         (should (equal (symbol-name (car result)) (cadr case)))
         (should (= (cdr result) (caddr case)))))
     (should-error (nelisp--rd-one "foo\\" 0 4)))))

(ert-deftest nelisp-standalone-target-prelude-rd-unescape-source-has-no-concat ()
  "The prelude source for `nelisp--rd-unescape' must not use concat in the loop."
  (let ((source (prin1-to-string
                 (nelisp-standalone-target-test--read-prelude-form
                  "(defun nelisp--rd-unescape"))))
    (should-not (string-match-p "(concat" source))
    (should (string-match-p "(make-string n 0)" source))
    (should (string-match-p "(aset out out-i" source))))

(ert-deftest nelisp-standalone-target-write-region-prefers-nl-write-file-for-bytes ()
  "High-byte strings must reach `nl-write-file' unchanged and bypass `wrf'."
  (let ((payload (string 65 128 161 255 66))
        (target (make-temp-file "nelisp-standalone-write-region-" nil ".o"))
        (captured nil)
        (wrf-called nil))
    (unwind-protect
        (nelisp-standalone-target-test--with-prelude-write-region-stub
         (lambda ()
           (cl-letf (((symbol-function 'nl-write-file)
                      (lambda (filename bytes)
                        (setq captured (list filename bytes))
                        t))
                     ((symbol-function 'wrf)
                      (lambda (&rest _)
                        (setq wrf-called t)
                        (error "wrf should not be called"))))
             (should (null (write-region payload nil target nil 'silent)))
             (should (equal (car captured) target))
             (should (equal (cadr captured)
                            (string-as-unibyte payload)))
             (should-not wrf-called))))
      (when (file-exists-p target)
        (delete-file target)))))

(ert-deftest nelisp-standalone-target-write-region-nl-write-file-non-t-errors ()
  "A non-`t' `nl-write-file' return must signal an error."
  (let ((payload (string 65 128 161 255 66))
        (target (make-temp-file "nelisp-standalone-write-region-" nil ".o"))
        (wrf-called nil)
        (error-message nil))
    (unwind-protect
        (nelisp-standalone-target-test--with-prelude-write-region-stub
         (lambda ()
           (cl-letf (((symbol-function 'nl-write-file)
                      (lambda (&rest _)
                        0))
                     ((symbol-function 'wrf)
                      (lambda (&rest _)
                        (setq wrf-called t)
                        (error "wrf should not be called"))))
             (condition-case err
                 (write-region payload nil target nil 'silent)
               (error
                (setq error-message (error-message-string err))))
             (should error-message)
             (should (string-match-p "nl-write-file returned 0" error-message))
             (should-not wrf-called))))
      (when (file-exists-p target)
        (delete-file target)))))

(ert-deftest nelisp-standalone-target-windows-uses-win64 ()
  "The Windows-native target maps to the Microsoft x64 ABI."
  (should (eq (nelisp-standalone--target-abi 'windows-x86_64) 'win64)))

(ert-deftest nelisp-standalone-target-macos-uses-aarch64-darwin ()
  "The macOS standalone target maps to arm64/Darwin code generation."
  (should (eq (nelisp-standalone--target-abi 'macos-aarch64) 'aapcs64))
  (should (eq (nelisp-standalone--target-arch 'macos-aarch64) 'aarch64))
  (should (eq (nelisp-standalone--target-os 'macos-aarch64) 'darwin)))

(ert-deftest nelisp-standalone-target-object-name-is-platform-specific ()
  "Windows build logs/cache use .obj names; Linux keeps the historical .o."
  (should (equal (nelisp-standalone--target-object-name
                  "driver.o" 'linux-x86_64)
                 "driver.o"))
  (should (equal (nelisp-standalone--target-object-name
                  "driver.o" 'windows-x86_64)
                 "driver.obj"))
  (should (equal (nelisp-standalone--target-object-name
                  "already.obj" 'windows-x86_64)
                 "already.obj")))

(ert-deftest nelisp-standalone-target-cache-is-target-qualified ()
  "Unit cache paths include the target name to avoid ABI mixing."
  (let ((base (file-name-as-directory nelisp-standalone--cache-dir))
        (nelisp-standalone--windows-arena-base #x70000000))
    (should (string-prefix-p
             base
             (nelisp-standalone--target-cache-dir 'linux-x86_64)))
    (should (string-suffix-p
             "linux-x86_64"
             (directory-file-name
              (nelisp-standalone--target-cache-dir 'linux-x86_64))))
    (should (string-suffix-p
             "windows-x86_64-arena-70000000"
             (directory-file-name
              (nelisp-standalone--target-cache-dir 'windows-x86_64))))
    (should (string-suffix-p
             "macos-aarch64"
             (directory-file-name
              (nelisp-standalone--target-cache-dir 'macos-aarch64))))))

(ert-deftest nelisp-standalone-target-cache-preserves-section-bytes ()
  "Standalone unit cache stores raw section bytes independent of host coding."
  (let* ((text (unibyte-string #x00 #x7f #x80 #x90 #xe8 #xff))
         (unit (nelisp-link-unit-make
                "probe.o"
                (list (cons 'text text))
                (list (list :name "probe" :section 'text :value 0))
                (list (list :offset 1 :type 'pc32 :symbol "ext"
                            :addend 0 :section 'text))))
         (encoded (nelisp-standalone--unit-cache-encode unit))
         (decoded (nelisp-standalone--unit-cache-decode encoded))
         (decoded-text (cdr (assq 'text (plist-get decoded :sections)))))
    (should (not (multibyte-string-p decoded-text)))
    (should (= (string-bytes decoded-text) (length text)))
    (should (equal decoded-text text))
    (should (equal (plist-get decoded :symbols) (plist-get unit :symbols)))
    (should (equal (plist-get decoded :relocs) (plist-get unit :relocs)))))

(ert-deftest nelisp-standalone-target-rejects-unknown-target ()
  "Unsupported targets fail before producing a mixed-ABI object cache."
  (should-error (nelisp-standalone--target-abi 'plan9-x86_64)
                :type 'error))

(ert-deftest nelisp-standalone-target-windows-output-uses-exe ()
  "Windows-native standalone outputs use a PE-friendly .exe path."
  (let ((nelisp-standalone--target 'windows-x86_64))
    (should (string-suffix-p ".exe" (nelisp-standalone--output-path nil)))
    (should (string-suffix-p ".exe" (nelisp-standalone--output-path t)))))

(ert-deftest nelisp-standalone-target-reader-cli-name-is-short ()
  "The user-facing standalone reader is target/nelisp(.exe)."
  (let ((nelisp-standalone--target 'linux-x86_64))
    (should (string-suffix-p "target/nelisp"
                             (nelisp-standalone--output-path t))))
  (let ((nelisp-standalone--target 'windows-x86_64))
    (should (string-suffix-p "target/nelisp.exe"
                             (nelisp-standalone--output-path t)))))

(ert-deftest nelisp-standalone-target-reader-cli-uses-long-options ()
  "The standalone reader exposes Lisp-like no-args REPL plus long options."
  (let* ((forms (nelisp-standalone--reader-driver-source))
         (flat (flatten-tree forms)))
    (cl-labels ((defun-source
                  (name)
                  (prin1-to-string
                   (cl-find-if
                    (lambda (form)
                      (and (consp form)
                           (eq (car form) 'defun)
                           (eq (cadr form) name)))
                    forms)))
                (starts-with-dash-dash-p
                  (name)
                  (let ((source (defun-source name)))
                    (and (string-match-p "(ptr-read-u8 ptr 0) 45" source)
                         (string-match-p "(ptr-read-u8 ptr 1) 45" source)))))
      (should (starts-with-dash-dash-p 'nl_cstr_eq_eval))
      (should (starts-with-dash-dash-p 'nl_cstr_eq_load))
      (should (starts-with-dash-dash-p 'nl_cstr_eq_neln_selftest))
      (should (starts-with-dash-dash-p 'nl_cstr_eq_repl))
      (should (starts-with-dash-dash-p 'nl_cstr_eq_embedded))
      (should (memq 'nl_cstr_eq_help flat))
      (should-not (memq 'nl_cstr_eq_dash_e flat))
      (should-not (memq 'nl_cstr_eq_dash_h flat)))))

(ert-deftest nelisp-standalone-target-reader-cli-dispatches-neln-selftest ()
  "The standalone reader dispatch table includes `--neln-selftest'."
  (let ((source (prin1-to-string (nelisp-standalone--reader-driver-source))))
    (should (string-match-p "nl_cstr_eq_neln_selftest" source))
    (should (string-match-p "(nl_neln_demo_exec ctx 41)" source))))

(ert-deftest nelisp-standalone-target-reader-neln-demo-bridges-real-helpers ()
  "The embedded native demo reaches the real helpers through local bridges."
  (let* ((nelisp-standalone--target 'linux-x86_64)
         (source (prin1-to-string
                  (nelisp-standalone--reader-neln-demo-source))))
    (should (string-match-p
             "(defun nl_neln_demo_alloc_symbol_bridge"
             source))
    (should (string-match-p
             "(extern-call nl_alloc_symbol bytes-ptr len result-slot)"
             source))
    (should (string-match-p
             "(defun nl_neln_demo_call1_bridge"
             source))
    (should (string-match-p
             "(extern-call nelisp_aot_builtin_call1"
             source))
    (should (string-match-p
             "(addr-of nl_neln_demo_alloc_symbol_bridge)"
             source))
    (should (string-match-p
             "(addr-of nl_neln_demo_call1_bridge)"
             source))
    (should-not (string-match-p "(defun nelisp_aot_builtin_call1" source))
    (should-not (string-match-p "(defun nl_alloc_symbol" source))))

(ert-deftest nelisp-standalone-target-macos-reader-cli-name-is-short ()
  "The macOS user-facing standalone reader is target/nelisp."
  (let ((nelisp-standalone--target 'macos-aarch64))
    (should (string-suffix-p "target/nelisp"
                             (nelisp-standalone--output-path t)))))

(ert-deftest nelisp-standalone-target-macos-start-is-main ()
  "The macOS eval start unit exports _main and calls driver."
  (let* ((nelisp-standalone--target 'macos-aarch64)
         (unit (nelisp-standalone--target-start-unit))
         (text (cdr (assq 'text (plist-get unit :sections))))
         (relocs (plist-get unit :relocs))
         (svc80 (unibyte-string #x01 #x10 #x00 #xd4))
         (svc-count 0)
         (pos 0))
    (should (equal (plist-get unit :name) "start.o"))
    (should (cl-find "_main" (plist-get unit :symbols)
                     :key (lambda (s) (plist-get s :name))
                     :test #'equal))
    (while (string-match (regexp-quote svc80) text pos)
      (setq svc-count (1+ svc-count)
            pos (match-end 0)))
    (should (> (length text) 16))
    (should (= svc-count 1))
    (should (cl-find "driver" relocs
                     :key (lambda (r) (plist-get r :symbol))
                     :test #'equal))
    (should (cl-find 'b26-pc relocs
                     :key (lambda (r) (plist-get r :type))))))

(ert-deftest nelisp-standalone-target-macos-reader-start-uses-native-stack ()
  "The macOS reader start unit switches onto an explicit native stack."
  (let* ((nelisp-standalone--target 'macos-aarch64)
         (unit (nelisp-standalone--target-start-unit t))
         (text (cdr (assq 'text (plist-get unit :sections))))
         (relocs (plist-get unit :relocs))
         (svc80 (unibyte-string #x01 #x10 #x00 #xd4))
         (svc-count 0)
         (pos 0))
    (should (equal (plist-get unit :name) "start.o"))
    (should (cl-find "_main" (plist-get unit :symbols)
                     :key (lambda (s) (plist-get s :name))
                     :test #'equal))
    (while (string-match (regexp-quote svc80) text pos)
      (setq svc-count (1+ svc-count)
            pos (match-end 0)))
    (should (> (length text) 80))
    (should (= svc-count 2))
    (should (cl-find "driver" relocs
                     :key (lambda (r) (plist-get r :symbol))
                     :test #'equal))
    (should (cl-find 'b26-pc relocs
                     :key (lambda (r) (plist-get r :type))))))

(ert-deftest nelisp-standalone-target-windows-start-imports-exitprocess ()
  "The Windows start unit calls driver, then KERNEL32!ExitProcess."
  (let* ((nelisp-standalone--target 'windows-x86_64)
         (unit (nelisp-standalone--target-start-unit))
         (text (cdr (assq 'text (plist-get unit :sections))))
         (relocs (plist-get unit :relocs)))
    (should (equal (plist-get unit :name) "start.obj"))
    (should (equal (substring text 0 6)
                   (unibyte-string #x48 #x83 #xe4 #xf0 #x48 #x83)))
    (should (equal (substring text 6 10)
                   (unibyte-string #xec #x20 #x31 #xc9)))
    (should (= (aref text 10) #xe8))
    (should (= (aref text 17) #xe8))
    (should (cl-find "driver" relocs
                     :key (lambda (r) (plist-get r :symbol))
                     :test #'equal))
    (should (cl-find "ExitProcess" relocs
                     :key (lambda (r) (plist-get r :symbol))
                     :test #'equal))))

(ert-deftest nelisp-standalone-target-windows-reader-uses-wide-file-api ()
  "Windows reader opens files with CreateFileW and UTF-8/UTF-16 conversion."
  (let* ((nelisp-standalone--target 'windows-x86_64)
         (imports (cdr (assoc "KERNEL32.dll"
                              nelisp-standalone--windows-reader-imports)))
         (source-tree (flatten-tree
                       (nelisp-standalone--reader-os-source-forms))))
    (should (member "CreateFileW" imports))
    (should (member "WideCharToMultiByte" imports))
    (should (member "MultiByteToWideChar" imports))
    (should-not (member "CreateFileA" imports))
    (should (memq 'CreateFileW source-tree))
    (should (memq 'WideCharToMultiByte source-tree))
    (should (memq 'MultiByteToWideChar source-tree))
    (should-not (memq 'CreateFileA source-tree))))

(ert-deftest nelisp-standalone-target-macos-reader-uses-darwin-syscalls ()
  "macOS reader file/stdin/stdout helpers use Darwin syscall numbers."
  (let ((nelisp-standalone--target 'macos-aarch64))
    (cl-labels ((tree-member-p
                 (needle tree)
                 (cond
                  ((equal needle tree) t)
                  ((consp tree)
                   (or (tree-member-p needle (car tree))
                       (tree-member-p needle (cdr tree)))))))
      (let ((forms (nelisp-standalone--reader-os-source-forms)))
        (should (tree-member-p '(syscall-direct 5 path 0 0 0 0 0) forms))
        (should (tree-member-p '(syscall-direct 5 path 1537 420 0 0 0) forms))
        (should (tree-member-p '(syscall-direct 6 fd 0 0 0 0 0) forms))
        (should (tree-member-p '(syscall-direct 3 fd ptr len 0 0 0) forms))
        (should (tree-member-p '(syscall-direct 4 fd ptr len 0 0 0) forms))
        (should (tree-member-p '(ptr-write-u32 mib 4 49) forms))
        (should (tree-member-p '(syscall-direct 202 mib 3 buf lenp 0 0) forms))
        (should-not (tree-member-p '(syscall-direct 2 path 0 0 0 0 0) forms))
        (should-not (tree-member-p '(syscall-direct 0 fd ptr len 0 0 0) forms))
        (should-not (tree-member-p '(syscall-direct 1 fd ptr len 0 0 0) forms))))))

(ert-deftest nelisp-standalone-target-reader-installs-process-builtin ()
  "The reader exposes the synchronous process substrate primitive."
  (cl-labels ((tree-member-p
               (needle tree)
               (cond
                ((equal needle tree) t)
                ((consp tree)
                 (or (tree-member-p needle (car tree))
                     (tree-member-p needle (cdr tree)))))))
    (should (member "nelisp-process-call-process"
                    nelisp-standalone--reader-builtins))
    (should (member "nelisp-process-start"
                    nelisp-standalone--reader-builtins))
    (should (member "nelisp-process-object-p"
                    nelisp-standalone--reader-builtins))
    (should (member "nelisp-portable-syscall"
                    nelisp-standalone--reader-builtins))
    (should (member "nelisp-process-call-process"
                    nelisp-standalone--applyfn-bf-builtins))
    (should (member "nelisp-process-start"
                    nelisp-standalone--applyfn-bf-builtins))
    (should (member "nelisp-process-async-ready-p"
                    nelisp-standalone--applyfn-bf-builtins))
    (should (tree-member-p
             '((:lit "nelisp-process-call-process") .
               (nl_bi_process_call_process args out))
             nelisp-standalone--applyfn-bf-arms))
    (should (tree-member-p
             '((:lit "nelisp-process-start") .
               (nl_bi_process_start_process args out))
             nelisp-standalone--applyfn-bf-arms))
    (should (tree-member-p
             '((:lit "nelisp-portable-syscall") .
               (wf_write_int out (nl_bi_portable_syscall args)))
             nelisp-standalone--applyfn-bf-arms))
    ;; Stale-literal note (2026-06-10): assert the load-bearing SHAPES of
    ;; nl_bi_process_call_process instead of the full defun literal — the
    ;; exact-form assertion went stale when 41ea76d7 added the M11
    ;; env-inherit branch and broke CI for every later commit.
    (should (tree-member-p
             '(setq envp (ptr-read-u64 268435600 0))
             nelisp-standalone--fileio-source))
    (should (tree-member-p
             '(nl_os_process_execve path argv envp)
             nelisp-standalone--fileio-source))
    (should (tree-member-p
             '(wf_write_int out (nl_bi_process_wait_exit_code pid))
             nelisp-standalone--fileio-source))
    (should (tree-member-p
             '(defun nl_bi_process_make_object (pid outfd out)
                (seq
                 (vector-make 5 out)
                 (nl_bi_process_set_int out 0 1886547811)
                 (nl_bi_process_set_int out 1 pid)
                 (nl_bi_process_set_int out 2 outfd)
                 (nl_bi_process_set_int out 3 0)
                 (nl_bi_process_set_int out 4 -1)
                 0))
             nelisp-standalone--fileio-source))
    ;; Same stale-literal note as call-process above: assert the
    ;; load-bearing shapes, not the full defun (the 41ea76d7 env-inherit
    ;; branch invalidated the old exact literal).
    (should (tree-member-p
             '(setq pipe_rc (nl_os_process_pipe pipev))
             nelisp-standalone--fileio-source))
    (should (tree-member-p
             '(nl_os_process_set_nonblock readfd)
             nelisp-standalone--fileio-source))
    (should (tree-member-p
             '(nl_bi_process_make_object pid readfd
                                         out)
             nelisp-standalone--fileio-source))))

(ert-deftest nelisp-standalone-target-reader-process-syscalls-are-targeted ()
  "Process helper syscall numbers stay target-specific."
  (cl-labels ((tree-member-p
               (needle tree)
               (cond
                ((equal needle tree) t)
                ((consp tree)
                 (or (tree-member-p needle (car tree))
                     (tree-member-p needle (cdr tree)))))))
    (let ((nelisp-standalone--target 'linux-x86_64))
      (let ((forms (nelisp-standalone--reader-os-source-forms)))
        (should (tree-member-p '(syscall-direct 57 0 0 0 0 0 0) forms))
        (should (tree-member-p '(syscall-direct 59 path argv envp 0 0 0)
                               forms))
        (should (tree-member-p '(syscall-direct 61 pid statusp options 0 0 0)
                               forms))
        (should (tree-member-p '(syscall-direct 33 oldfd newfd 0 0 0 0)
                               forms))
        (should (tree-member-p '(syscall-direct 22 pipev 0 0 0 0 0)
                               forms))
        (should (tree-member-p '(syscall-direct 72 fd 4 2048 0 0 0)
                               forms))
        (should (tree-member-p '(syscall-direct 62 pid sig 0 0 0 0)
                               forms))
        (should (tree-member-p '(syscall-direct 60 127 0 0 0 0 0)
                               forms))))
    (let ((nelisp-standalone--target 'macos-aarch64))
      (let ((forms (nelisp-standalone--reader-os-source-forms)))
        (should (tree-member-p '(syscall-direct 2 0 0 0 0 0 0) forms))
        (should (tree-member-p '(syscall-direct 59 path argv envp 0 0 0)
                               forms))
        (should (tree-member-p '(syscall-direct 7 pid statusp options 0 0 0)
                               forms))
        (should (tree-member-p '(syscall-direct 90 oldfd newfd 0 0 0 0)
                               forms))
        (should (tree-member-p '(syscall-direct 42 pipev 0 0 0 0 0)
                               forms))
        (should (tree-member-p '(syscall-direct 92 fd 4 4 0 0 0)
                               forms))
        (should (tree-member-p '(syscall-direct 37 pid sig 0 0 0 0)
                               forms))
        (should (tree-member-p '(syscall-direct 1 127 0 0 0 0 0)
                               forms))))
    (let ((nelisp-standalone--target 'windows-x86_64))
      (let ((forms (nelisp-standalone--reader-os-source-forms)))
        (should (tree-member-p '(defun nl_os_process_fork nil -1) forms))
        (should (tree-member-p '(defun nl_os_process_wait4
                                  (pid statusp options) -1)
                               forms))
        (should (tree-member-p '(defun nl_os_process_pipe (pipev) -1)
                               forms))))))

(ert-deftest nelisp-standalone-target-reader-detects-shifted-argv ()
  "The reader driver can detect and normalize macOS LC_MAIN argv+1."
  (let ((nelisp-standalone--target 'macos-aarch64))
    (cl-labels ((tree-member-p
                 (needle tree)
                 (cond
                  ((equal needle tree) t)
                  ((consp tree)
                   (or (tree-member-p needle (car tree))
                       (tree-member-p needle (cdr tree)))))))
      (let ((forms (nelisp-standalone--reader-driver-source)))
        (should (tree-member-p
                 '(defun nl_cli_argv_shifted_p (argc slot0 slot1)
                    (if (> argc 1)
                        (if (= slot1 0)
                            1
                          (nl_cli_command_p slot0))
                      0))
                 forms))
        (should (tree-member-p
                 '(ptr-write-u64 sp0 16 slot0)
                 forms))
        (should (tree-member-p
                 '(ptr-write-u64 sp0 24 slot1)
                 forms))
        (should (tree-member-p
                 '(ptr-write-u64 sp0 32 slot2)
                 forms))))))

(ert-deftest nelisp-standalone-target-reader-repl-prelude-avoids-stack-literal ()
  "REPL prelude is copied through the arena buffer, not a huge stack literal."
  (let* ((forms (nelisp-standalone--reader-repl-prelude-forms
                 'fbuf 'src 'cursor 'result 'pool 'out 'ctx 'builtin_sym))
         (flat (flatten-tree forms))
         (copy-def (nelisp-standalone--copy-lit-u64-defun 'probe "abcdefghi"))
         (copy-flat (flatten-tree copy-def))
         (chunk-defs (nelisp-standalone--copy-lit-u64-defuns
                      'big-probe "abcdefghijklmnopqr" 8))
         (chunk-flat (flatten-tree chunk-defs)))
    (should (memq 'nl_repl_prelude_source flat))
    (should (memq 'nl_alloc_str flat))
    (should-not (memq 'sexp-write-str-lit flat))
    (should (memq 'ptr-write-u64 copy-flat))
    (should (memq 'ptr-write-u8 copy-flat))
    (should (memq 'big-probe chunk-flat))
    (should (memq 'big-probe_chunk_000 chunk-flat))
    (should (memq 'big-probe_chunk_001 chunk-flat))
    (should (memq 'big-probe_chunk_002 chunk-flat))))

(ert-deftest nelisp-standalone-target-reader-load-uses-direct-source-printer ()
  "`--load' evaluates raw file source and prints the resulting value."
  (cl-labels ((tree-member-p
               (needle tree)
               (cond
                ((equal needle tree) t)
                ((consp tree)
                 (or (tree-member-p needle (car tree))
                     (tree-member-p needle (cdr tree)))))))
    (let ((forms (nelisp-standalone--reader-driver-source)))
      (should (tree-member-p
               '(nl_alloc_str fbuf n src)
               forms))
      (should (tree-member-p
               '(nl_cli_write_value fbuf out)
               forms))
      (should (tree-member-p
               '(defun nl_cli_value_to_buf (fbuf off out)
                  (let* ((tag (ptr-read-u64 out 0)))
                    (cond
                     ((= tag 0) (nl_cli_put_nil fbuf off))
                     ((= tag 1) (nl_cli_put_byte fbuf off 116))
                     ((= tag 2) (nl_cli_put_dec fbuf off (ptr-read-u64 out 8)))
                     ((= tag 4) (nl_cli_put_string_value fbuf off out 0))
                     ((= tag 5) (nl_cli_put_string_value fbuf off out 1))
                     ((= tag 6) (nl_cli_put_string_value fbuf off out 1))
                     ((= tag 7) (nl_cli_put_list_tail fbuf
                                                       (nl_cli_put_byte fbuf off 40)
                                                       out 1))
                     ((= tag 8) (nl_cli_put_vector_loop fbuf
                                                        (nl_cli_put_byte fbuf off 91)
                                                        out 0 (vector-len out)))
                     (t (nl_cli_put_object fbuf off)))))
               forms))
      (should-not (tree-member-p
                   '(nl_cli_wrap_source_at fbuf (+ off n) src)
                   forms)))))

(ert-deftest nelisp-standalone-target-unwind-cleanup-errors-propagate ()
  "Cleanup uses stashing eval; body nonlocal exits preserve the M6 kind flag."
  (cl-labels ((tree-member-p
               (needle tree)
               (cond
                ((equal needle tree) t)
                ((consp tree)
                 (or (tree-member-p needle (car tree))
                     (tree-member-p needle (cdr tree)))))))
    (let ((forms nelisp-cc-sf-unwind-protect--source))
      (should (tree-member-p
               '(defun nl_sf_uw_do_cleanup (car cdr body-rc env out _pad6)
                  (if (= body-rc 0)
                      (let* ((scratch (alloc-bytes 32 8)))
                        (nl_sf_uw_do_cleanup_preserve scratch car cdr body-rc env out))
                    (let* ((flag-save (ptr-read-u64 268435472 0))
                           (tag-save (alloc-bytes 32 8))
                           (val-save (alloc-bytes 32 8)))
                      (seq
                       (nl_sexp_clone_into 268435480 tag-save)
                       (nl_sexp_clone_into 268435512 val-save)
                       (nl_sf_uw_do_cleanup_body_exit
                        flag-save tag-save val-save car cdr body-rc env out)))))
               forms))
      (should (tree-member-p
               '(defun nl_sf_uw_do_cleanup_preserve (scratch car cdr body-rc env out)
                  (nl_sf_uw_cleanup_evaled
                   (extern-call nelisp_eval_call car env scratch)
                   cdr body-rc env out 0))
               forms))
      (should (tree-member-p
               '(defun nl_sf_uw_cleanup_evaled (cleanup-rc cdr body-rc env out _pad6)
                  (if (= cleanup-rc 0)
                      (nl_sf_uw_cleanup cdr body-rc env out 0 0)
                    cleanup-rc))
               forms))
      (should (tree-member-p
               '(defun nl_sf_uw_cleanup_after_body_exit
                    (cleanup-rc flag-save tag-save val-save cdr body-rc env out)
                  (if (= cleanup-rc 0)
                      (seq
                       (nl_sexp_clone_into tag-save 268435480)
                       (nl_sexp_clone_into val-save 268435512)
                       (ptr-write-u64 268435472 0 flag-save)
                       (dealloc-bytes tag-save 32 8)
                       (dealloc-bytes val-save 32 8)
                       (nl_sf_uw_cleanup cdr body-rc env out 0 0))
                    (seq
                     (dealloc-bytes tag-save 32 8)
                     (dealloc-bytes val-save 32 8)
                     cleanup-rc)))
               forms))
      (should (tree-member-p
               '(defun nl_sf_uw_do_cleanup_body_exit
                    (flag-save tag-save val-save car cdr body-rc env out)
                  (let* ((scratch (alloc-bytes 32 8)))
                    (nl_sf_uw_cleanup_after_body_exit
                     (extern-call nelisp_eval_call car env scratch)
                     flag-save tag-save val-save cdr body-rc env out)))
               forms)))))

(ert-deftest nelisp-standalone-target-reader-boundary-reclaim-is-conservative ()
  "Doc 140 Stage 5 reclaims only safe immediate non-mutating boundaries."
  (cl-labels ((tree-member-p
               (needle tree)
               (cond
                ((equal needle tree) t)
                ((consp tree)
                 (or (tree-member-p needle (car tree))
                     (tree-member-p needle (cdr tree)))))))
    (let ((forms (nelisp-standalone--reader-driver-source))
          (boundary nelisp-standalone--reader-boundary-source)
          (eval-source nelisp-standalone--reader-eval-source-source))
      (should (tree-member-p
               '(defun nl_boundary_immediate_result_p (out)
                  (if (<= (ptr-read-u64 out 0) 3) 1 0))
               boundary))
      (should (tree-member-p
               '(if (= (ptr-read-u64 268436216 0) 1)
                    (if (= (nl_boundary_immediate_result_p out) 1)
                        (if (= (ptr-read-u64 268435544 0) epoch0)
                            (if (= (ptr-read-u64 268435472 0) 0)
                                (if (= (ptr-read-u64 268435464 0) 0)
                                    (nl_boundary_reclaim mark_chunk mark_cursor)
                                  0)
                              0)
                          0)
                      0)
                  0)
               boundary))
      (should (tree-member-p
               '(ptr-write-u64 268436216 0 0)
               forms))
      (should (tree-member-p
               '(ptr-write-u64 268436216 0 1)
               forms))
      (should (tree-member-p
               '(ptr-write-u64 268435552 0 0)
               boundary))
      (should (tree-member-p
               '(ptr-write-u64 268436168 0 mark_chunk)
               boundary))
      (should (tree-member-p
               '(nl_boundary_maybe_reclaim mark_chunk mark_cursor epoch0 out)
               eval-source)))))

(ert-deftest nelisp-standalone-target-reader-repl-suffix-uses-runtime-base ()
  "REPL runtime wrapper must read the quit slot via the live RUNTIME arena base
on EVERY target (Doc 140 Stage 8), never a baked fixed arena-base immediate.
The runtime-PARSED suffix cannot use `data-addr', and the pre-Stage-8
windows/macos fixed bases (e.g. #x70000000 + 8) point at unmapped VA after the
rebase -> SIGSEGV in the REPL print path's quit-flag check."
  (dolist (target '(macos-aarch64 windows-x86_64 linux-x86_64))
    (let* ((nelisp-standalone--target target)
           (suffix (nelisp-standalone--reader-repl-eval-suffix)))
      ;; Reads the quit flag via the live runtime base.
      (should (string-match-p
               (regexp-quote "(ptr-read-u64 (+ (car (nelisp--arena-stats)) 8) 0)")
               suffix))
      ;; Embeds NO fixed arena-base metadata immediate for any target.
      (should-not (string-match-p
                   (number-to-string
                    (nelisp-standalone--target-arena-metadata-address 8))
                   suffix)))))

(ert-deftest nelisp-standalone-target-windows-arena-init-uses-null-virtualalloc ()
  "Windows chunk-0 init uses VirtualAlloc(NULL, ...) and stores `nl_arena_base'."
  (let ((nelisp-standalone--target 'windows-x86_64))
    (cl-labels ((tree-member-p
                 (needle tree)
                 (cond
                  ((equal needle tree) t)
                  ((consp tree)
                   (or (tree-member-p needle (car tree))
                       (tree-member-p needle (cdr tree)))))))
      (let ((arena (nelisp-standalone--target-arena-source)))
        (should (tree-member-p
                 '(nl_os_alloc_chunk #x4000000)
                 arena))
        (should (tree-member-p
                 '(extern-call VirtualAlloc 0 size 8192 4)
                 arena))
        (should (tree-member-p
                 '(extern-call VirtualAlloc base 4096 4096 4)
                 arena))
        (should (tree-member-p
                 '(ptr-write-u64 (data-addr nl_arena_base) 0 base)
                 arena))
        (should (tree-member-p
                 '(ptr-write-u64 (+ base 704) 0 (+ base 768))
                 arena))
        (should-not (tree-member-p
                     '(extern-call VirtualAlloc #x70000000 #x4000000 12288 4)
                     arena))))))

(ert-deftest nelisp-standalone-target-windows-arena-reserves-64m ()
  "Windows chunk-0 keeps a bounded 64 MiB reservation without committing it up front."
  (let ((nelisp-standalone--target 'windows-x86_64)
        (nelisp-standalone--windows-arena-base #x70000000))
    (cl-labels ((tree-member-p
                 (needle tree)
                 (cond
                  ((equal needle tree) t)
                  ((consp tree)
                   (or (tree-member-p needle (car tree))
                       (tree-member-p needle (cdr tree)))))))
      (let ((arena (nelisp-standalone--target-arena-source)))
        (should (tree-member-p
                 '(nl_os_alloc_chunk #x4000000)
                 arena))
        (should (tree-member-p
                 '(extern-call VirtualAlloc 0 size 8192 4)
                 arena))
        (should-not (tree-member-p
                     '(extern-call VirtualAlloc 268435456 #x10000000 12288 4)
                     arena))
        (should-not (tree-member-p
                     '(extern-call VirtualAlloc 0 #x40000000 12288 4)
                     arena))))))

(ert-deftest nelisp-standalone-target-windows-stage8-rewrites-arena-slots ()
  "Windows Stage 8 rewrites rebased arena metadata to `nl_arena_base' loads."
  (let ((nelisp-standalone--target 'windows-x86_64)
        (nelisp-standalone--windows-arena-base #x70000000))
    (should (equal
             (nelisp-standalone--chunk-arena-rewrite
              (nelisp-standalone--rebase-arena-source
               '(seq (ptr-write-u64 268435472 0 1)
                     (atomic-fetch-add 268435544 1)
                     (ptr-write-u64 4096 0 268435456))))
             '(seq
               (ptr-write-u64
                (+ (ptr-read-u64 (data-addr nl_arena_base) 0) 16) 0 1)
               (atomic-fetch-add
                (+ (ptr-read-u64 (data-addr nl_arena_base) 0) 88) 1)
               (ptr-write-u64
                4096 0 (+ (ptr-read-u64 (data-addr nl_arena_base) 0) 0)))))))

(ert-deftest nelisp-standalone-target-linux-arena-uses-anonymous-mmap ()
  "Doc 140 Stage 8: linux reserves chunk 0 with mmap(NULL) — no fixed base.
The kernel-chosen base is stored in the driver-owned `nl_arena_base' bss slot
and reached at run time through it; there is no MAP_FIXED / MAP_FIXED_NOREPLACE
reservation at 0x10000000 left in the normal runtime path."
  (let ((nelisp-standalone--target 'linux-x86_64))
    (cl-labels ((tree-member-p
                 (needle tree)
                 (cond
                  ((equal needle tree) t)
                  ((consp tree)
                   (or (tree-member-p needle (car tree))
                       (tree-member-p needle (cdr tree)))))))
      (let ((arena (nelisp-standalone--target-arena-source)))
        ;; chunk 0 is reserved through the anonymous chunk allocator.
        (should (tree-member-p '(nl_os_alloc_chunk #x10000000) arena))
        ;; its kernel-chosen base is stored in the driver-owned bss slot.
        (should (tree-member-p
                 '(ptr-write-u64 (data-addr nl_arena_base) 0 base) arena))
        ;; `nl_os_alloc_chunk' uses MAP_PRIVATE|MAP_ANONYMOUS mmap at NULL.
        (should (tree-member-p '(syscall-direct 9 0 size 3 34 -1 0) arena))
        ;; the OOM path still exits cleanly.
        (should (tree-member-p '(syscall-direct 60 88 0 0 0 0 0) arena))
        ;; NO fixed-base reservation remains in the normal runtime path.
        (should-not (tree-member-p
                     '(syscall-direct 9 #x10000000 #x10000000 3 #x100022 -1 0)
                     arena))))))

(ert-deftest nelisp-standalone-target-linux-arena-size-stays-pressure-visible ()
  "Linux must not hide arena pressure by growing the fixed virtual reservation.
Doc 140 Stage 7: the fixed first chunk is 256 MiB (=#x10000000=), not the
historical 8 GiB — pressure beyond it is handled by chunk growth."
  (should (= (nelisp-standalone--target-arena-size 'linux-x86_64)
             #x10000000)))

(ert-deftest nelisp-standalone-target-arena-size-slot-is-initialized ()
  "All native standalone targets expose reservation size through arena metadata."
  (cl-labels ((tree-member-p
               (needle tree)
               (cond
                ((equal needle tree) t)
                ((consp tree)
                 (or (tree-member-p needle (car tree))
                     (tree-member-p needle (cdr tree)))))))
    ;; Doc 140 Stage 8: linux seeds the reservation-size slot relative to the
    ;; runtime mmap base (`(+ base 216)') rather than a fixed immediate.
    (let ((nelisp-standalone--target 'linux-x86_64))
      (should (tree-member-p
               '(ptr-write-u64 (+ base 216) 0 #x10000000)
               (nelisp-standalone--target-arena-source))))
    (let ((nelisp-standalone--target 'windows-x86_64)
          (nelisp-standalone--windows-arena-base #x70000000))
      (should (tree-member-p
               '(ptr-write-u64 (+ base 216) 0 #x4000000)
               (nelisp-standalone--target-arena-source))))
    (let ((nelisp-standalone--target 'macos-aarch64))
      (should (tree-member-p
               '(ptr-write-u64 (+ base 216) 0 #x20000000)
               (nelisp-standalone--target-arena-source))))))

(ert-deftest nelisp-standalone-target-arena-registers-first-chunk ()
  "Registers chunk 0's descriptor + control slots at init.
Doc 140 Stage 8 (linux): the writes are relative to the runtime mmap base
(`(+ base OFF)') instead of a fixed immediate — chunk 0 is no longer pinned at
0x10000000.  windows/macos now follow the same runtime-base scheme through the
shared `nl_arena_base' slot."
  (cl-labels ((tree-member-p
               (needle tree)
               (cond
                ((equal needle tree) t)
                ((consp tree)
                 (or (tree-member-p needle (car tree))
                     (tree-member-p needle (cdr tree)))))))
    (let ((nelisp-standalone--target 'linux-x86_64))
      (let ((arena (nelisp-standalone--target-arena-source)))
        (should (tree-member-p '(ptr-write-u64 base 0 #x400) arena))
        (should (tree-member-p '(ptr-write-u64 (+ base 704) 0 (+ base 768)) arena))
        (should (tree-member-p '(ptr-write-u64 (+ base 712) 0 (+ base 768)) arena))
        (should (tree-member-p '(ptr-write-u64 (+ base 720) 0 1) arena))
        (should (tree-member-p '(ptr-write-u64 (+ base 728) 0 #x10000000) arena))
        (should (tree-member-p '(ptr-write-u64 (+ base 768) 0 base) arena))
        (should (tree-member-p '(ptr-write-u64 (+ base 776) 0 #x10000000) arena))
        (should (tree-member-p '(ptr-write-u64 (+ base 792) 0 (+ base #x400)) arena))
        (should (tree-member-p '(ptr-write-u64 (+ base 816) 0 0) arena))))
    (let ((nelisp-standalone--target 'windows-x86_64)
          (nelisp-standalone--windows-arena-base #x70000000))
      (let ((arena (nelisp-standalone--target-arena-source)))
        (should (tree-member-p '(ptr-write-u64 (+ base 704) 0 (+ base 768)) arena))
        (should (tree-member-p '(ptr-write-u64 (+ base 720) 0 1) arena))
        (should (tree-member-p '(ptr-write-u64 (+ base 728) 0 #x4000000) arena))
        (should (tree-member-p '(ptr-write-u64 (+ base 792) 0 (+ base #x400)) arena))))
    (let ((nelisp-standalone--target 'macos-aarch64))
      (let ((arena (nelisp-standalone--target-arena-source)))
        (should (tree-member-p '(ptr-write-u64 (+ base 704) 0 (+ base 768)) arena))
        (should (tree-member-p '(ptr-write-u64 (+ base 720) 0 1) arena))
        (should (tree-member-p '(ptr-write-u64 (+ base 728) 0 #x20000000) arena))
        (should (tree-member-p '(ptr-write-u64 (+ base 792) 0 (+ base #x400)) arena))))))

(ert-deftest nelisp-standalone-target-stage8-arena-base-slot-unit ()
  "Doc 140 Stage 8: chunked native targets export driver-owned bss globals.
Windows uses the target-correct `.obj' unit name; linux/macOS keep `.o'."
  (dolist (case '((linux-x86_64 "arena-base.o")
                  (windows-x86_64 "arena-base.obj")
                  (macos-aarch64 "arena-base.o")))
    (pcase-let ((`(,target ,name) case))
      (let* ((nelisp-standalone--target target)
             (u (nelisp-standalone--arena-base-slot-unit))
             (syms (plist-get u :symbols))
             (by-name (mapcar (lambda (sym)
                                (cons (plist-get sym :name) sym))
                              syms)))
        (should (equal name (plist-get u :name)))
        (dolist (expected '(("nl_arena_base" . 0)
                            ("nl_rootstack_top" . 8)
                            ("nl_rootstack_region" . 16)
                            ("nl_gc_diag" . 1048592)
                            ("nl_gc_loop_ctx" . 1048656)
                            ("nl_fa_tbl_base" . 1106064)
                            ("nl_large_freelist_heads" . 1106200)
                            ("nl_gc_alloc_debt" . 1107496)
                            ("nl_gc_pending" . 1107504)))
          (let ((sym (cdr (assoc (car expected) by-name))))
            (should sym)
            (should (equal (cdr expected) (plist-get sym :value)))
            (should (eq 'bss (plist-get sym :section)))))
        (should (equal 1107512
                       (cdr (assq 'bss (plist-get u :sections)))))))))

(ert-deftest nelisp-standalone-target-stage8-build-appends-arena-base-slot-unit ()
  "Doc 140 Stage 8: standalone link units append the `nl_arena_base' slot unit."
  (dolist (target '(linux-x86_64 windows-x86_64 macos-aarch64))
    (let ((nelisp-standalone--target target)
          (nelisp-standalone--manifest '(("probe.o" :helper nil)))
          captured)
      (cl-letf (((symbol-function 'nelisp-standalone--unit-for)
                 (lambda (_entry)
                   (nelisp-link-unit-make "probe.o" nil nil nil)))
                ((symbol-function 'nelisp-standalone--arena-base-slot-unit)
                 (lambda ()
                   (nelisp-link-unit-make
                    (nelisp-standalone--target-object-name "arena-base.o")
                    (list (cons 'bss 8)) nil nil)))
                ((symbol-function 'nelisp-standalone--output-path)
                 (lambda (&optional _reader-p) "/tmp/nelisp-target-test"))
                ((symbol-function 'nelisp-link-units)
                 (lambda (_out units &rest _)
                   (setq captured units)))
                ((symbol-function 'nelisp-link-units-pe32)
                 (lambda (_out units _entry _imports &optional _opts)
                   (setq captured units)))
                ((symbol-function 'nelisp-link-units-macho-exec)
                 (lambda (_out units _entry _arch)
                   (setq captured units)))
                ((symbol-function 'set-file-modes)
                 (lambda (&rest _) nil))
                ((symbol-function 'nelisp-standalone--codesign-macos-adhoc)
                 (lambda (&rest _) nil))
                ((symbol-function 'message)
                 (lambda (&rest _) nil)))
        (nelisp-standalone-build)
        (should captured)
        (should (equal
                 (nelisp-standalone--target-object-name "arena-base.o")
                 (plist-get (car (last captured)) :name)))))))

(ert-deftest nelisp-standalone-target-stage8-chunk-arena-rewrite-cross-platform ()
  "Doc 140 Stage 8: chunk-arena rewrite fires for linux, windows, and macOS.
It leaves the base-establishing `nl_arena_init' untouched while rewriting
rebased fixed metadata immediates to `nl_arena_base' loads + offsets."
  ;; linux: free-list-head (arena-base + 96) -> runtime base load + 96.
  (let ((nelisp-standalone--target 'linux-x86_64))
    (should (equal
             (nelisp-standalone--chunk-arena-rewrite
              '(defun f () (ptr-read-u64 268435552 0)))
             '(defun f ()
                (ptr-read-u64
                 (+ (ptr-read-u64 (data-addr nl_arena_base) 0) 96) 0))))
    (should (equal
             (nelisp-standalone--chunk-arena-rewrite
              '(defun nl_arena_init () (nl_os_alloc_chunk 268435456)))
             '(defun nl_arena_init () (nl_os_alloc_chunk 268435456)))))
  ;; windows/macOS: rebase first, then rewrite the target-relative immediates.
  (dolist (case '((windows-x86_64 #x70000000)
                  (macos-aarch64 #x800000000)))
    (pcase-let ((`(,target ,base) case))
      (let ((nelisp-standalone--target target))
        (should (equal
                 (nelisp-standalone--chunk-arena-rewrite
                  (nelisp-standalone--rebase-arena-source
                   '(defun f ()
                      (seq (ptr-read-u64 268435552 0)
                           (ptr-read-u64 268435456 0)))))
                 '(defun f ()
                    (seq
                     (ptr-read-u64
                      (+ (ptr-read-u64 (data-addr nl_arena_base) 0) 96) 0)
                     (ptr-read-u64
                      (+ (ptr-read-u64 (data-addr nl_arena_base) 0) 0) 0)))))
        (should (equal
                 (nelisp-standalone--rebase-arena-source
                  '(defun f () (ptr-read-u64 268435552 0)))
                 `(defun f () (ptr-read-u64 ,(+ base 96) 0))))))))

(ert-deftest nelisp-standalone-target-stage6-generation-split ()
  "Doc 140 Stage 6: chunk 0 (boot generation) is tagged persistent and the
top-level boundary reclaimer skips persistent chunks — only temporary per-form
scratch chunks have their cursor reset."
  (cl-labels ((tree-member-p
               (needle tree)
               (cond
                ((equal needle tree) t)
                ((consp tree)
                 (or (tree-member-p needle (car tree))
                     (tree-member-p needle (cdr tree)))))))
    ;; the reclaimer gates the reset on the persistent flag bit.
    (should (tree-member-p '(logand flags 2)
                           nelisp-standalone--reader-boundary-source))
    ;; chunk-0 init writes desc.flags = (logior 1 persistent) = 3 on every
    ;; dynamic-base chunk-0 init path.
    (let ((dyn (nelisp-standalone--arena-init-metadata-forms-dynamic 'base 256)))
      (should (tree-member-p
               (list 'ptr-write-u64
                     (list '+ 'base (+ nelisp-standalone--arena-chunk0-desc-offset
                                       nelisp-standalone--arena-chunk-desc-flags-offset))
                     0 3)
               dyn)))
    (should (= 3 (logior 1 nelisp-standalone--arena-chunk-flag-persistent)))))

(ert-deftest nelisp-standalone-target-gc-walks-chunk-descriptors ()
  "Doc 140 Stage 3 makes GC membership and sweep chunk-aware."
  (cl-labels ((tree-member-p
               (needle tree)
               (cond
                ((equal needle tree) t)
                ((consp tree)
               (or (tree-member-p needle (car tree))
                     (tree-member-p needle (cdr tree)))))))
    (let ((flat (flatten-tree nelisp-standalone--gc-source))
          (source (prin1-to-string nelisp-standalone--gc-source)))
      (should (memq 'nl_gc_chunk_contains_any flat))
      (should (memq 'nl_gc_sweep_chunks flat))
      (should (member 268436160 flat))
      (should (memq 'nl_gc_in_arena flat))
      (should (string-match-p "nl_gc_chunk_contains_any" source))
      (should-not
       (tree-member-p
        '(defun nl_gc_sweep
             nil
           (let ((hdr (ptr-read-u64 268435568 0))
                 (end (+ 268435456 (ptr-read-u64 268435456 0))))
             (while (and (> hdr 0) (< hdr end))
               (setq hdr (nl_gc_sweep_step hdr end)))
             0))
        nelisp-standalone--gc-source)))))

(ert-deftest nelisp-standalone-target-gc-validates-buffer-shape-before-slot-walks ()
  "The GC mark path checks box size and slot-buffer capacity before deref."
  (cl-labels ((tree-member-p
               (needle tree)
               (cond
                ((equal needle tree) t)
                ((consp tree)
                 (or (tree-member-p needle (car tree))
                     (tree-member-p needle (cdr tree)))))))
    (let ((flat (flatten-tree nelisp-standalone--gc-source))
          (source (nelisp-standalone-target-test--gc-source-string)))
      (should (memq 'nl_gc_addr_chunk_end flat))
      (should (memq 'nl_gc_vec_slots_ok flat))
      (should (memq 'nl_gc_box_bt_ok flat))
      (should (string-match-p "(defun nl_gc_addr_chunk_end" source))
      (should (string-match-p "(nl_gc_bt_ok hdr bt end)" source))
      (should (string-match-p "(= (nl_hdr_mark hdr) 2)" source))
      (should (string-match-p "(if (= len 0) 1" source))
      (should (string-match-p "(if (= data_ptr 0) 0" source))
      (should (string-match-p "(/ (- bt 8) 8)" source))
      (should (string-match-p
               "(if (= (nl_gc_box_bt_ok box 40) 0) 0[[:space:]\n]*(if (= (nl_gc_mark_block box) 0) 0"
               source))
      (should (string-match-p
               "(if (= (nl_gc_box_bt_ok box 72) 0) 0[[:space:]\n]*(if (= (nl_gc_mark_block box) 0) 0"
               source))
      (should-not (string-match-p
                   "(if (= (nl_gc_mark_block box) 0) 0[[:space:]\n]*(if (= (nl_gc_box_bt_ok box 40) 0) 0"
                   source))
      (should-not (string-match-p
                   "(if (= (nl_gc_mark_block box) 0) 0[[:space:]\n]*(if (= (nl_gc_box_bt_ok box 72) 0) 0"
                   source)))))

(ert-deftest nelisp-standalone-target-gc-shape-guards-short-circuit-and-reject-free-blocks ()
  "The new GC guards short-circuit empty buffers and reject free blocks."
  (let ((orig-vec (and (fboundp 'nl_gc_vec_slots_ok)
                       (symbol-function 'nl_gc_vec_slots_ok)))
        (orig-box (and (fboundp 'nl_gc_box_bt_ok)
                       (symbol-function 'nl_gc_box_bt_ok))))
    (unwind-protect
        (progn
          (eval (nelisp-standalone-target-test--read-gc-form
                 "(defun nl_gc_vec_slots_ok"))
          (eval (nelisp-standalone-target-test--read-gc-form
                 "(defun nl_gc_box_bt_ok"))
          (let ((called nil))
            (cl-letf (((symbol-function 'nl_gc_in_arena)
                       (lambda (&rest _)
                         (setq called t)
                         (error "should not be called")))
                      ((symbol-function 'nl_gc_addr_chunk_end)
                       (lambda (&rest _)
                         (setq called t)
                         (error "should not be called")))
                      ((symbol-function 'nl_hdr_bt)
                       (lambda (&rest _)
                         (setq called t)
                         (error "should not be called")))
                      ((symbol-function 'nl_gc_bt_ok)
                       (lambda (&rest _)
                         (setq called t)
                         (error "should not be called")))
                      ((symbol-function 'nl_hdr_mark)
                       (lambda (&rest _)
                         (setq called t)
                         (error "should not be called"))))
              (should (= (nl_gc_vec_slots_ok 0 0) 1))
              (should-not called)))
          (cl-letf (((symbol-function 'nl_gc_in_arena) (lambda (_addr) 1))
                    ((symbol-function 'nl_gc_addr_chunk_end) (lambda (_hdr) 4096))
                    ((symbol-function 'nl_hdr_bt) (lambda (_hdr) 64))
                    ((symbol-function 'nl_gc_bt_ok) (lambda (&rest _) 1))
                    ((symbol-function 'nl_hdr_mark) (lambda (_hdr) 2)))
            (should (= (nl_gc_vec_slots_ok 8192 1) 0))
            (should (= (nl_gc_box_bt_ok 8192 40) 0))))
      (if orig-vec
          (fset 'nl_gc_vec_slots_ok orig-vec)
        (fmakunbound 'nl_gc_vec_slots_ok))
      (if orig-box
          (fset 'nl_gc_box_bt_ok orig-box)
        (fmakunbound 'nl_gc_box_bt_ok)))))

(ert-deftest nelisp-standalone-target-gc-conservative-owner-accepts-large-free-remainder ()
  "A >16 MiB split remainder remains a valid next-header shape for an owner."
  (let ((orig-split (and (fboundp 'nl_freelist_split_block)
                         (symbol-function 'nl_freelist_split_block)))
        (orig-owner (and (fboundp 'nl_gc_conserv_owner)
                         (symbol-function 'nl_gc_conserv_owner)))
        (memory (make-hash-table :test #'eql))
        (hdr 1000)
        (want 40)
        (large-bt (+ 16777216 104))
        (chunk-end nil))
    (unwind-protect
        (progn
          (setq chunk-end (+ hdr large-bt))
          (eval (nelisp-standalone-target-test--read-arena-defun
                 "nl_freelist_split_block"))
          (eval (nelisp-standalone-target-test--read-gc-defun
                 "nl_gc_conserv_owner"))
          (cl-letf (((symbol-function 'ptr-read-u64)
                     (lambda (addr _off) (gethash addr memory 0)))
                    ((symbol-function 'ptr-write-u64)
                     (lambda (addr _off value)
                       (puthash addr value memory)
                       0))
                    ((symbol-function 'nl_hdr_bt)
                     (lambda (addr)
                       (let ((word (gethash addr memory 0)))
                         (- word (logand word 7)))))
                    ((symbol-function 'nl_hdr_mark)
                     (lambda (addr) (logand (gethash addr memory 0) 7)))
                    ((symbol-function 'nl_hdr_set_mark)
                     (lambda (addr mark)
                       (let ((word (gethash addr memory 0)))
                         (puthash addr (logior (- word (logand word 7)) mark)
                                  memory))
                       0))
                    ((symbol-function 'nl_gc_free_block_head)
                     (lambda (_bt) 9000))
                    ((symbol-function 'nl_gc_free_block_link)
                     (lambda (_free-hdr _head) 0))
                    ((symbol-function 'nl_gc_in_arena)
                     (lambda (addr)
                       (if (and (<= hdr addr) (< addr chunk-end))
                           1
                         0)))
                    ((symbol-function 'nl_gc_is_boot) (lambda (_addr) 0))
                    ((symbol-function 'nl_gc_addr_chunk_end)
                     (lambda (addr)
                       (if (and (<= hdr addr) (< addr chunk-end))
                           chunk-end
                         0)))
                    ((symbol-function 'nl_gc_bt_ok)
                     (lambda (block block-total end)
                       (if (and (<= 16 block-total)
                                (= (logand block-total 7) 0)
                                (<= (+ block block-total) end))
                           1
                         0)))
                    ((symbol-function 'seq)
                     (lambda (&rest forms) (car (last forms))))
                    ((symbol-function 'nl_seq2) (lambda (_a b) b)))
            (puthash hdr (logior large-bt 2) memory)
            (should (= (nl_freelist_split_block hdr want large-bt)
                       (+ hdr 8)))
            (should (= (gethash hdr memory) want))
            (should (= (logand (gethash (+ hdr want) memory) 7) 2))
            (should (> (- (gethash (+ hdr want) memory) 2) 16777216))
            (should (= (nl_gc_conserv_owner (+ hdr 8)) 1))
            (should (= (logand (gethash hdr memory) 7) 4))
            ;; A normal small next block still validates.
            (puthash hdr want memory)
            (puthash (+ hdr want) 16 memory)
            (should (= (nl_gc_conserv_owner (+ hdr 8)) 1))
            (should (= (logand (gethash hdr memory) 7) 4))
            ;; Malformed small next headers remain rejected.
            (puthash hdr want memory)
            (puthash (+ hdr want) 15 memory)
            (should (= (nl_gc_conserv_owner (+ hdr 8)) 0))
            (should (= (logand (gethash hdr memory) 7) 0))
            ;; A small aligned next block that crosses its chunk end is invalid.
            (setq chunk-end (+ hdr want 16))
            (puthash (+ hdr want) 24 memory)
            (should (= (nl_gc_conserv_owner (+ hdr 8)) 0))
            (should (= (logand (gethash hdr memory) 7) 0))))
      (if orig-split
          (fset 'nl_freelist_split_block orig-split)
        (fmakunbound 'nl_freelist_split_block))
      (if orig-owner
          (fset 'nl_gc_conserv_owner orig-owner)
        (fmakunbound 'nl_gc_conserv_owner)))))

(ert-deftest nelisp-standalone-target-gc-conservative-word-pins-raw-owner-before-tag ()
  "Conservative words pin valid raw owners, tracing children only for Sexps."
  (let ((orig-word (and (fboundp 'nl_gc_conserv_word)
                        (symbol-function 'nl_gc_conserv_word)))
        (events nil)
        (pinned nil))
    (unwind-protect
        (progn
          (eval (nelisp-standalone-target-test--read-gc-defun
                 "nl_gc_conserv_word"))
          (cl-letf (((symbol-function 'nl_gc_in_arena)
                     (lambda (addr)
                       (if (= addr 120) 0 1)))
                    ((symbol-function 'nl_gc_conserv_owner)
                     (lambda (addr)
                       (push (list 'owner addr) events)
                       (push addr pinned)
                       1))
                    ((symbol-function 'ptr-read-u8)
                     (lambda (addr _off)
                       (push (list 'tag addr) events)
                       (if (= addr 104) 15 12)))
                    ((symbol-function 'nl_gc_mark_slot)
                     (lambda (addr)
                       (push (list 'mark addr) events)
                       1)))
            ;; A raw allocation payload has no Sexp children, but its exact
            ;; payload start must still pin the owning arena block.
            (should (= (nl_gc_conserv_word 104) 1))
            (should (memq 104 pinned))
            (should (equal (nreverse events)
                           '((owner 104) (tag 104))))
            ;; A plausible Sexp pins its owner before tracing children.
            (setq events nil)
            (should (= (nl_gc_conserv_word 112) 1))
            (should (memq 112 pinned))
            (should (equal (nreverse events)
                           '((owner 112) (tag 112) (mark 112))))
            ;; Out-of-arena words are rejected before owner or child marking.
            (setq events nil)
            (should (= (nl_gc_conserv_word 120) 0))
            (should-not (memq 120 pinned))
            (should-not events)))
      (if orig-word
          (fset 'nl_gc_conserv_word orig-word)
        (fmakunbound 'nl_gc_conserv_word)))))

(ert-deftest nelisp-standalone-target-arena-adds-target-chunk-allocator ()
  "Doc 140 Stage 4 adds target-specific non-fixed chunk allocation."
  (cl-labels ((tree-member-p
               (needle tree)
               (cond
                ((equal needle tree) t)
                ((consp tree)
                 (or (tree-member-p needle (car tree))
                     (tree-member-p needle (cdr tree)))))))
    (let ((nelisp-standalone--target 'linux-x86_64))
      (should (tree-member-p
               '(syscall-direct 9 0 size 3 34 -1 0)
               (nelisp-standalone--target-arena-source))))
    (let ((nelisp-standalone--target 'windows-x86_64)
          (nelisp-standalone--windows-arena-base #x70000000))
      (let ((arena (nelisp-standalone--target-arena-source)))
        (should (tree-member-p
                 '(extern-call VirtualAlloc 0 size 8192 4)
                 arena))
        (should (tree-member-p
                 '(nl_os_commit_range base old new)
                 arena))))
    (let ((nelisp-standalone--target 'macos-aarch64))
      (should (tree-member-p
               '(syscall-direct 197 0 size 3 4098 -1 0)
               (nelisp-standalone--target-arena-source))))))

(ert-deftest nelisp-standalone-target-arena-adds-target-chunk-reclaimer ()
  "Growth chunk reclamation is wired on every standalone native target."
  (cl-labels ((tree-member-p
               (needle tree)
               (cond
                ((equal needle tree) t)
                ((consp tree)
                 (or (tree-member-p needle (car tree))
                     (tree-member-p needle (cdr tree)))))))
    (let ((nelisp-standalone--target 'linux-x86_64))
      (should (tree-member-p
               '(syscall-direct 11 base size 0 0 0 0)
               (nelisp-standalone--target-arena-source))))
    (let ((nelisp-standalone--target 'windows-x86_64)
          (nelisp-standalone--windows-arena-base #x70000000))
      (let ((arena (nelisp-standalone--target-arena-source)))
        (should (tree-member-p
                 '(extern-call VirtualFree base 0 32768)
                 arena))
        (should-not (tree-member-p
                     '(defun nl_os_free_chunk (_base _size) 0)
                     arena))))
    (let ((nelisp-standalone--target 'macos-aarch64))
      (let ((arena (nelisp-standalone--target-arena-source)))
        (should (tree-member-p
                 '(syscall-direct 73 base size 0 0 0 0)
                 arena))
        (should-not (tree-member-p
                     '(defun nl_os_free_chunk (_base _size) 0)
                     arena))))))

(ert-deftest nelisp-standalone-target-intern-region-is-target-aware ()
  "Symbol-name intern region setup uses each target's allocation surface."
  (cl-labels ((tree-member-p
               (needle tree)
               (cond
                ((equal needle tree) t)
                ((consp tree)
                 (or (tree-member-p needle (car tree))
                     (tree-member-p needle (cdr tree)))))))
    (let ((nelisp-standalone--target 'linux-x86_64))
      (let ((arena (nelisp-standalone--target-arena-source)))
        (should (tree-member-p '(nl_intern_region_init) arena))
        (should (tree-member-p
                 '(syscall-direct 9 0 67108864 3 34 -1 0)
                 arena))))
    (let ((nelisp-standalone--target 'windows-x86_64)
          (nelisp-standalone--windows-arena-base #x70000000))
      (let ((arena (nelisp-standalone--target-arena-source)))
        (should (tree-member-p '(nl_intern_region_init) arena))
        (should (tree-member-p
                 '(extern-call VirtualAlloc 0 67108864 12288 4)
                 arena))
        (should-not (tree-member-p
                     '(syscall-direct 9 0 67108864 3 34 -1 0)
                     arena))))
    (let ((nelisp-standalone--target 'macos-aarch64))
      (let ((arena (nelisp-standalone--target-arena-source)))
        (should (tree-member-p '(nl_intern_region_init) arena))
        (should (tree-member-p
                 '(syscall-direct 197 0 67108864 3 4098 -1 0)
                 arena))
        (should-not (tree-member-p
                     '(syscall-direct 9 0 67108864 3 34 -1 0)
                     arena))))))

(ert-deftest nelisp-standalone-target-arena-allocation-is-chunk-aware ()
  "Doc 140 Stage 4 routes allocation through current chunk descriptors."
  (cl-labels ((tree-member-p
               (needle tree)
               (cond
                ((equal needle tree) t)
                ((consp tree)
                 (or (tree-member-p needle (car tree))
                     (tree-member-p needle (cdr tree)))))))
    (let ((flat (flatten-tree (nelisp-standalone--target-arena-source))))
      (should (memq 'nl_chunk_alloc_new flat))
      (should (memq 'nl_chunk_try_alloc flat))
      (should (memq 'atomic-compare-exchange flat))
      (should (member 268436168 flat))
      (should (tree-member-p
               '(= (atomic-compare-exchange cursor_addr old new) 1)
               (nelisp-standalone--target-arena-source)))
      (should (tree-member-p
               '(defun nl_os_alloc_chunk (size)
                  (let ((p (syscall-direct 9 0 size 3 34 -1 0)))
                    (if (< p 4096) 0 p)))
               (let ((nelisp-standalone--target 'linux-x86_64))
                 (nelisp-standalone--target-arena-source)))))))

(ert-deftest nelisp-standalone-target-arena-large-bt-normalizes-boundaries ()
  "Large BLOCK_TOTAL requests normalize to the fixed canonical capacity tiers."
  (let ((orig (and (fboundp 'nl_bt_normalize)
                   (symbol-function 'nl_bt_normalize))))
    (unwind-protect
        (progn
          (eval (nelisp-standalone-target-test--read-arena-defun
                 "nl_bt_normalize"))
          (dolist (case '((16 16)
                          (472 472)
                          (473 512)
                          (4096 4096)
                          (4097 8192)
                          (262144 262144)
                          (262145 327680)
                          (2097152 2097152)
                          (2097153 3145728)
                          (16777216 16777216)
                          (16777217 16777217)))
            (should (= (nl_bt_normalize (nth 0 case))
                       (nth 1 case)))))
      (if orig
          (fset 'nl_bt_normalize orig)
        (fmakunbound 'nl_bt_normalize)))))

(ert-deftest nelisp-standalone-target-arena-large-bt-class-index-validates-canonical-bts ()
  "Only exact canonical large BLOCK_TOTAL values map to the 162 class heads."
  (let ((orig-idx (and (fboundp 'nl_large_bt_class_index)
                       (symbol-function 'nl_large_bt_class_index)))
        (orig-head (and (fboundp 'nl_large_freelist_head_addr)
                        (symbol-function 'nl_large_freelist_head_addr))))
    (unwind-protect
        (progn
          (eval (nelisp-standalone-target-test--read-arena-defun
                 "nl_large_bt_class_index"))
          (eval (nelisp-standalone-target-test--read-arena-defun
                 "nl_large_freelist_head_addr"))
          (dolist (case '((512 0)
                          (4096 56)
                          (8192 57)
                          (262144 119)
                          (327680 120)
                          (2097152 147)
                          (3145728 148)
                          (16777216 161)))
            (should (= (nl_large_bt_class_index (nth 0 case))
                       (nth 1 case))))
          (dolist (bt '(473 4097 262145 2097153 16777217))
            (should (= (nl_large_bt_class_index bt) -1)))
          (let ((nl_large_freelist_heads 'nl_large_freelist_heads))
            (cl-letf (((symbol-function 'data-addr)
                       (lambda (_sym) 40960)))
              (should (= (nl_large_freelist_head_addr 512) 40960))
              (should (= (nl_large_freelist_head_addr 8192) (+ 40960 (* 57 8))))
              (should (= (nl_large_freelist_head_addr 16777216) (+ 40960 (* 161 8))))
              (should (= (nl_large_freelist_head_addr 473) 0)))))
      (if orig-idx
          (fset 'nl_large_bt_class_index orig-idx)
        (fmakunbound 'nl_large_bt_class_index))
      (if orig-head
          (fset 'nl_large_freelist_head_addr orig-head)
        (fmakunbound 'nl_large_freelist_head_addr)))))

(ert-deftest nelisp-standalone-target-arena-bt-floor-class-boundaries-and-gaps ()
  "Noncanonical BTs floor into the nearest supported class up to 16 MiB."
  (let ((orig (and (fboundp 'nl_bt_floor_class)
                   (symbol-function 'nl_bt_floor_class))))
    (unwind-protect
        (progn
          (eval (nelisp-standalone-target-test--read-arena-defun
                 "nl_bt_floor_class"))
          (dolist (case '((15 0)
                          (16 16)
                          (472 472)
                          (473 472)
                          (504 472)
                          (511 472)
                          (512 512)
                          (576 576)
                          (4096 4096)
                          (4097 4096)
                          (5000 4096)
                          (8192 8192)
                          (262144 262144)
                          (262145 262144)
                          (300000 262144)
                          (327680 327680)
                          (2097152 2097152)
                          (2097153 2097152)
                          (2500000 2097152)
                          (3145728 3145728)
                          (16777216 16777216)
                          (16777217 0)))
            (should (= (nl_bt_floor_class (nth 0 case))
                       (nth 1 case)))))
      (if orig
          (fset 'nl_bt_floor_class orig)
        (fmakunbound 'nl_bt_floor_class)))))

(ert-deftest nelisp-standalone-target-arena-large-freelist-routing-shape ()
  "Allocator/free paths normalize large BTs and route floor classes to BSS."
  (let ((source (nelisp-standalone-target-test--arena-source-string))
        (flat (flatten-tree (nelisp-standalone--target-arena-source)))
        (gc-flat (flatten-tree nelisp-standalone--gc-source))
        (gc-source (nelisp-standalone-target-test--gc-source-string)))
    (should (memq 'nl_bt_normalize flat))
    (should (memq 'nl_bt_floor_class flat))
    (should (memq 'nl_large_bt_class_index flat))
    (should (memq 'nl_large_freelist_head_addr flat))
    (should (memq 'nl_large_freelist_head_addr_by_index flat))
    (should (memq 'nl_freelist_pop_guarded flat))
    (should (memq 'nl_large_freelist_take_upward flat))
    (should (memq 'nl_gc_free_block_head gc-flat))
    (should (string-match-p
             (regexp-quote "(let* ((raw (nl_block_total size)) (want (nl_bt_normalize raw)))")
             source))
    (should (string-match-p
             (regexp-quote "(nl_freelist_pop_guarded (+ 268435696 (- want 16)) want)")
             source))
    (should (string-match-p
             (regexp-quote "(let ((head (nl_large_freelist_head_addr want)))")
             source))
    (should (string-match-p
             (regexp-quote "(nl_large_freelist_take_upward (+ idx 1) want)")
             source))
    (should (string-match-p
             (regexp-quote "(let ((cls (nl_bt_floor_class bt)))")
             gc-source))
    (should (string-match-p
             (regexp-quote "(nl_gc_free_block_link hdr (nl_gc_free_block_head (nl_hdr_bt hdr)))")
             gc-source))))

(ert-deftest nelisp-standalone-target-gc-rebuild-source-shape ()
  "Non-compacting collectors rebuild free-lists after sweep."
  (let ((gc-source (nelisp-standalone-target-test--gc-source-string))
        (gc-flat (flatten-tree nelisp-standalone--gc-source)))
    (should (memq 'nl_gc_rebuild_clear_fl gc-flat))
    (should (memq 'nl_gc_rebuild_free_run gc-flat))
    (should (memq 'nl_gc_rebuild_chunk gc-flat))
    (should (memq 'nl_gc_rebuild_chunks gc-flat))
    (should (memq 'nl_gc_rebuild_free_lists gc-flat))
    (should (string-match-p
             (regexp-quote "(let ((r (nl_gc_sweep)))")
             gc-source))
    (should (string-match-p
             (regexp-quote "(nl_seq2 (nl_gc_rebuild_free_lists) r)")
             gc-source))))

(ert-deftest nelisp-standalone-target-arena-alloc-bytes-zero-fills-requested-payload-only ()
  "Reuse clears only the requested aligned payload while capacity stays normalized."
  (let ((orig-alloc (and (fboundp 'nl_alloc_bytes)
                         (symbol-function 'nl_alloc_bytes)))
        (nl_gc_alloc_debt 'nl_gc_alloc_debt)
        (zero-fill-calls nil)
        (freelist-wants nil)
        (chunk-wants nil)
        (take-results '(4242 0 0)))
    (unwind-protect
        (progn
          (eval (nelisp-standalone-target-test--read-arena-defun
                 "nl_alloc_bytes"))
          (cl-letf (((symbol-function 'nl_block_total)
                     (lambda (size)
                       (pcase size
                         (984 1000)
                         (464 472)
                         (_ (error "unexpected size %S" size)))))
                    ((symbol-function 'nl_bt_normalize)
                     (lambda (bt)
                       (pcase bt
                         (1000 1024)
                         (472 472)
                         (_ bt))))
                    ((symbol-function 'ptr-read-u64)
                     (lambda (addr _off)
                       (pcase addr
                         (900000 0)
                         (268435624 0)
                         (268435656 0)
                         (268436168 7777)
                         (_ (error "unexpected read %S" addr)))))
                    ((symbol-function 'data-addr)
                     (lambda (sym)
                       (pcase sym
                         ('nl_gc_alloc_debt 900000)
                         (_ (error "unexpected data-addr %S" sym)))))
                    ((symbol-function 'ptr-write-u64)
                     (lambda (_addr _off _value) 0))
                    ((symbol-function 'nl_freelist_take)
                     (lambda (want)
                       (setq freelist-wants
                             (append freelist-wants (list want)))
                       (prog1 (car take-results)
                         (setq take-results (cdr take-results)))))
                    ((symbol-function 'nl_alloc_zero_fill)
                     (lambda (obj off nbytes)
                       (setq zero-fill-calls
                             (append zero-fill-calls
                                     (list (list obj off nbytes))))
                       0))
                    ((symbol-function 'nl_seq2)
                     (lambda (_a b) b))
                    ((symbol-function 'nl_chunk_try_alloc)
                     (lambda (chunk want)
                       (setq chunk-wants
                             (append chunk-wants (list (list chunk want))))
                       8484))
                    ((symbol-function 'nl_chunk_alloc_new)
                     (lambda (_want)
                       (error "unexpected chunk growth")))
                    ((symbol-function 'nl_os_alloc_fail)
                     (lambda ()
                       (error "unexpected alloc fail"))))
            (should (= (nl_alloc_bytes 984 8) 4242))
            (should (equal zero-fill-calls '((4242 0 992))))
            (should (equal freelist-wants '(1024)))
            (setq zero-fill-calls nil
                  freelist-wants nil
                  chunk-wants nil)
            (should (= (nl_alloc_bytes 984 8) 8484))
            (should (equal freelist-wants '(1024)))
            (should (equal chunk-wants '((7777 1024))))
            (should-not zero-fill-calls)
            (setq zero-fill-calls nil
                  freelist-wants nil
                  chunk-wants nil)
            (should (= (nl_alloc_bytes 464 8) 8484))
            (should (equal freelist-wants '(472)))
            (should (equal chunk-wants '((7777 472))))
            (should-not zero-fill-calls)))
      (if orig-alloc
          (fset 'nl_alloc_bytes orig-alloc)
        (fmakunbound 'nl_alloc_bytes)))))

(ert-deftest nelisp-standalone-target-arena-alloc-bytes-reuse-increments-midform-debt ()
  "Free-list reuse still bumps the monotonic mid-form allocation debt."
  (let ((orig-alloc (and (fboundp 'nl_alloc_bytes)
                         (symbol-function 'nl_alloc_bytes)))
        (nl_gc_alloc_debt 'nl_gc_alloc_debt)
        writes)
    (unwind-protect
        (progn
          (eval (nelisp-standalone-target-test--read-arena-defun
                 "nl_alloc_bytes"))
          (cl-letf (((symbol-function 'nl_block_total) (lambda (_size) 1000))
                    ((symbol-function 'nl_bt_normalize) (lambda (_raw) 1024))
                    ((symbol-function 'data-addr)
                     (lambda (sym)
                       (pcase sym
                         ('nl_gc_alloc_debt 900000)
                         (_ (error "unexpected data-addr %S" sym)))))
                    ((symbol-function 'ptr-read-u64)
                     (lambda (addr _off)
                       (pcase addr
                         (900000 4096)
                         (268435624 0)
                         (268435656 0)
                         (_ (error "unexpected read %S" addr)))))
                    ((symbol-function 'ptr-write-u64)
                     (lambda (addr off value)
                       (push (list addr off value) writes)
                       0))
                    ((symbol-function 'nl_freelist_take) (lambda (_want) 4242))
                    ((symbol-function 'nl_alloc_zero_fill) (lambda (&rest _) 0))
                    ((symbol-function 'nl_seq2) (lambda (_a b) b))
                    ((symbol-function 'nl_chunk_try_alloc)
                     (lambda (&rest _) (error "unexpected bump path")))
                    ((symbol-function 'nl_chunk_alloc_new)
                     (lambda (&rest _) (error "unexpected chunk growth")))
                    ((symbol-function 'nl_os_alloc_fail)
                     (lambda () (error "unexpected alloc fail"))))
            (should (= (nl_alloc_bytes 984 8) 4242))
            (should (member '(900000 0 5120) writes))))
      (if orig-alloc
          (fset 'nl_alloc_bytes orig-alloc)
        (fmakunbound 'nl_alloc_bytes)))))

(ert-deftest nelisp-standalone-target-arena-alloc-bytes-bump-increments-midform-debt-once ()
  "The debt counter is charged exactly once on a bump-path allocation."
  (let ((orig-alloc (and (fboundp 'nl_alloc_bytes)
                         (symbol-function 'nl_alloc_bytes)))
        (nl_gc_alloc_debt 'nl_gc_alloc_debt)
        writes)
    (unwind-protect
        (progn
          (eval (nelisp-standalone-target-test--read-arena-defun
                 "nl_alloc_bytes"))
          (cl-letf (((symbol-function 'nl_block_total) (lambda (_size) 1000))
                    ((symbol-function 'nl_bt_normalize) (lambda (_raw) 1024))
                    ((symbol-function 'data-addr)
                     (lambda (sym)
                       (pcase sym
                         ('nl_gc_alloc_debt 900000)
                         (_ (error "unexpected data-addr %S" sym)))))
                    ((symbol-function 'ptr-read-u64)
                     (lambda (addr _off)
                       (pcase addr
                         (900000 4096)
                         (268435624 0)
                         (268435656 0)
                         (268436168 7777)
                         (_ (error "unexpected read %S" addr)))))
                    ((symbol-function 'ptr-write-u64)
                     (lambda (addr off value)
                       (push (list addr off value) writes)
                       0))
                    ((symbol-function 'nl_freelist_take) (lambda (_want) 0))
                    ((symbol-function 'nl_alloc_zero_fill)
                     (lambda (&rest _) (error "unexpected zero-fill on bump path")))
                    ((symbol-function 'nl_seq2) (lambda (_a b) b))
                    ((symbol-function 'nl_chunk_try_alloc) (lambda (_chunk _want) 8484))
                    ((symbol-function 'nl_chunk_alloc_new)
                     (lambda (&rest _) (error "unexpected chunk growth")))
                    ((symbol-function 'nl_os_alloc_fail)
                     (lambda () (error "unexpected alloc fail"))))
            (should (= (nl_alloc_bytes 984 8) 8484))
            (should (= 1 (length (seq-filter
                                  (lambda (write) (= (car write) 900000))
                                  writes))))
            (should (member '(900000 0 5120) writes))))
      (if orig-alloc
          (fset 'nl_alloc_bytes orig-alloc)
        (fmakunbound 'nl_alloc_bytes)))))

(ert-deftest nelisp-standalone-target-gc-debug-switch-5-arms-from-midform-debt ()
  "Debug switch 5 arms the next trigger from alloc debt, not reserved bytes."
  (let ((source (nelisp-standalone-target-test--build-script-source)))
    (should (string-match-p
             (regexp-quote
              "(ptr-write-u64 (data-addr nl_gc_loop_ctx) 40
                                        (+ (ptr-read-u64 (data-addr nl_gc_alloc_debt) 0)
                                           16777216))")
             source))
    (should-not (string-match-p
                 (regexp-quote
                  "(ptr-write-u64 (data-addr nl_gc_loop_ctx) 40
                                        (+ (ptr-read-u64 268436184 0) 16777216))")
                 source))))

(ert-deftest nelisp-standalone-target-gc-debug-switch-manual-recycle-toggle ()
  "Debug switches 19/20 toggle only manual immediate/temp recycling."
  (let ((orig (and (fboundp 'bf_debug_switch)
                   (symbol-function 'bf_debug_switch)))
        (mode 19)
        writes)
    (unwind-protect
        (progn
          (eval
           (or (nelisp-standalone-target-test--find-defun
                nelisp-standalone--applyfn-core-helpers
                'bf_debug_switch)
               (error "bf_debug_switch not found")))
          (cl-letf (((symbol-function 'wf_argval)
                     (lambda (_args _index) mode))
                    ((symbol-function 'seq)
                     (lambda (&rest values) (car (last values))))
                    ((symbol-function 'ptr-write-u64)
                     (lambda (addr off value)
                       (push (list addr off value) writes)
                       1))
                    ((symbol-function 'ptr-read-u64)
                     (lambda (&rest _) 0))
                    ((symbol-function 'data-addr) (lambda (_sym) 700000))
                    ((symbol-function 'nl_gc_ctx_push) (lambda (&rest _) 0))
                    ((symbol-function 'nl_gc_ctx_pop) (lambda () 0))
                    ((symbol-function 'alloc-bytes) (lambda (&rest _) 800000))
                    ((symbol-function 'wf_write_nil) (lambda (&rest _) 0))
                    ((symbol-function 'wf_cons_int) (lambda (&rest _) 0)))
            (bf_debug_switch nil 900000)
            (should (member '(268435680 0 0) writes))
            (should-not (member '(268435624 0 1) writes))
            (setq mode 20
                  writes nil)
            (bf_debug_switch nil 900000)
            (should (member '(268435680 0 1) writes))
            (should-not (member '(268435624 0 0) writes))))
      (if orig
          (fset 'bf_debug_switch orig)
        (fmakunbound 'bf_debug_switch)))))

(ert-deftest nelisp-standalone-target-gc-manual-double-free-is-not-relinked ()
  "A manually recycled FREE block is rejected instead of self-linking."
  (let ((orig (and (fboundp 'nl_gc_free_block)
                   (symbol-function 'nl_gc_free_block)))
        trips)
    (unwind-protect
        (progn
          (eval
           (or (nelisp-standalone-target-test--find-defun
                nelisp-standalone--gc-source 'nl_gc_free_block)
               (error "nl_gc_free_block not found")))
          (cl-letf (((symbol-function 'nl_seq2) (lambda (_a b) b))
                    ((symbol-function 'nl_gc_in_arena) (lambda (_hdr) 1))
                    ((symbol-function 'nl_hdr_bt) (lambda (_hdr) 40))
                    ((symbol-function 'nl_gc_addr_chunk_end) (lambda (_hdr) 1000))
                    ((symbol-function 'nl_gc_bt_ok) (lambda (&rest _) 1))
                    ((symbol-function 'nl_hdr_mark) (lambda (_hdr) 2))
                    ((symbol-function 'nl_fl_record_trip)
                     (lambda (cur bt want)
                       (push (list cur bt want) trips)
                       0))
                    ((symbol-function 'nl_gc_free_block_link)
                     (lambda (&rest _) (error "double-free was relinked"))))
            (should (= (nl_gc_free_block 100) 0))
            (should (equal trips '((108 40 0))))))
      (if orig
          (fset 'nl_gc_free_block orig)
        (fmakunbound 'nl_gc_free_block)))))

(ert-deftest nelisp-standalone-target-gc-coalesce-tombstones-interior-header ()
  "Coalescing FREE A+B registers only A+B and tombstones old header B."
  (let ((orig (and (fboundp 'nl_gc_rebuild_chunk)
                   (symbol-function 'nl_gc_rebuild_chunk)))
        writes
        registrations)
    (unwind-protect
        (progn
          (eval
           (or (nelisp-standalone-target-test--find-defun
                nelisp-standalone--gc-source 'nl_gc_rebuild_chunk)
               (error "nl_gc_rebuild_chunk not found")))
          (cl-letf (((symbol-function 'seq)
                     (lambda (&rest values) (car (last values))))
                    ((symbol-function 'nl_seq2) (lambda (_a b) b))
                    ((symbol-function 'ptr-read-u64)
                     (lambda (addr _off)
                       (if (= addr 524) 100
                         (error "unexpected read %S" addr))))
                    ((symbol-function 'ptr-write-u64)
                     (lambda (addr off value)
                       (push (list addr off value) writes)
                       1))
                    ((symbol-function 'nl_gc_chunk_end) (lambda (_chunk) 172))
                    ((symbol-function 'nl_hdr_bt)
                     (lambda (hdr)
                       (if (memq hdr '(100 124 148)) 24
                         (error "unexpected header %S" hdr))))
                    ((symbol-function 'nl_gc_bt_ok) (lambda (&rest _) 1))
                    ((symbol-function 'nl_hdr_mark)
                     (lambda (hdr) (if (memq hdr '(100 124)) 2 0)))
                    ((symbol-function 'nl_gc_rebuild_free_run)
                     (lambda (hdr span)
                       (push (list hdr span) registrations)
                       0)))
            (should (= (nl_gc_rebuild_chunk 500) 0))
            (should (equal registrations '((100 48))))
            (should (member '(124 0 3) writes))))
      (if orig
          (fset 'nl_gc_rebuild_chunk orig)
        (fmakunbound 'nl_gc_rebuild_chunk)))))

(ert-deftest nelisp-standalone-target-mxcache-temporaries-use-root-stack ()
  "Macro cache eval temporaries are root slots, not arena scratch slots."
  (let ((inner (prin1-to-string nelisp-standalone--mxcache-eval-inner-cons))
        (macro (prin1-to-string nelisp-standalone--mxcache-macro-apply-eval)))
    (dolist (needle '("(nl_rootstack_init)"
                      "(marker (nl_root_mark))"
                      "(func_slot (nl_root_reserve))"
                      "(args_slot (nl_root_reserve))"
                      "(nl_root_release marker)"))
      (should (string-match-p (regexp-quote needle) inner)))
    (dolist (needle '("(nl_rootstack_init)"
                      "(marker (nl_root_mark))"
                      "(exp_slot (nl_root_reserve))"
                      "(stable_slot (alloc-bytes 32 8))"
                      "(nl_sexp_clone_into exp_slot stable_slot)"
                      "(nl_mxcache_store form_ptr stable_slot)"
                      "(nelisp_eval_call stable_slot env_ptr out)"
                      "(nl_root_release marker)"))
      (should (string-match-p (regexp-quote needle) macro)))
    (should-not (string-match-p
                 (regexp-quote "(func_slot (alloc-bytes 32 8))") inner))
    (should-not (string-match-p
                 (regexp-quote "(args_slot (alloc-bytes 32 8))") inner))
    (should-not (string-match-p
                 (regexp-quote "(exp_slot (alloc-bytes 32 8))") macro))
    (should-not (string-match-p
                 (regexp-quote "(nl_mxcache_store form_ptr exp_slot)") macro))
    (should-not (string-match-p
                 (regexp-quote "(nelisp_eval_call exp_slot env_ptr out)")
                 macro))))

(ert-deftest nelisp-standalone-target-mxcache-patch-exports-root-helpers ()
  "The cache patch splices every replacement defun into the AOT unit."
  (let* ((source
          '(seq
            (defun untouched () 0)
            (defun nl_cons_macro_apply_eval () 0)
            (defun nl_eval_inner_cons () 0)))
         (patched (nelisp-standalone--patch-combiner-cons-mxcache source))
         (forms (cdr patched))
         (names (mapcar #'cadr forms)))
    (should (equal (car patched) 'seq))
    (should (cl-every (lambda (form) (eq (car-safe form) 'defun)) forms))
    (dolist (name '(untouched
                    nl_cons_macro_apply_eval_done
                    nl_cons_macro_apply_eval_rooted
                    nl_cons_macro_apply_eval
                    nl_eval_inner_cons_done
                    nl_eval_inner_cons_rooted
                    nl_eval_inner_cons))
      (should (memq name names)))))

(ert-deftest nelisp-standalone-target-mxcache-root-frames-balance-on-return ()
  "The cache eval wrappers release their root marker on terminal paths."
  (let* ((names '(nl_eval_inner_cons_done
                  nl_eval_inner_cons_rooted
                  nl_eval_inner_cons
                  nl_cons_macro_apply_eval_done
                  nl_cons_macro_apply_eval_rooted
                  nl_cons_macro_apply_eval))
         (saved (mapcar (lambda (name)
                          (cons name (and (fboundp name)
                                          (symbol-function name))))
                        names))
         (reserves '(100 200))
         (marker 777)
         releases)
    (unwind-protect
        (progn
          (dolist (form (cdr nelisp-standalone--mxcache-eval-inner-cons))
            (eval form))
          (dolist (form (cdr nelisp-standalone--mxcache-macro-apply-eval))
            (eval form))
          (cl-letf (((symbol-function 'seq)
                     (lambda (&rest values) (car (last values))))
                    ((symbol-function 'nl_rootstack_init) (lambda () 0))
                    ((symbol-function 'nl_root_mark) (lambda () marker))
                    ((symbol-function 'nl_root_reserve)
                     (lambda () (pop reserves)))
                    ((symbol-function 'nl_root_release)
                     (lambda (value) (push value releases) 0))
                    ((symbol-function 'sexp-tag)
                     (lambda (ptr) (if (= ptr 10) 4 0)))
                    ((symbol-function 'nl_apply_special)
                     (lambda (&rest _) 0))
                    ((symbol-function 'nl_cons_cdr_ptr)
                     (lambda (_ptr) 20))
                    ((symbol-function 'nl_cons_stash_void_function)
                     (lambda (&rest _) 9)))
            (should (= (nl_eval_inner_cons 1 10 11 12 13) 0))
            (should (equal releases '(777)))
            (setq marker 888
                  reserves '(300)
                  releases nil)
            (should (= (nl_cons_macro_apply_eval 1 2 3 4 5) 9))
            (should (equal releases '(888)))))
      (dolist (entry saved)
        (if (cdr entry)
            (fset (car entry) (cdr entry))
          (fmakunbound (car entry)))))))

(ert-deftest nelisp-standalone-target-mxcache-stores-stable-arena-slot ()
  "Successful macro expansion never persists its temporary BSS root slot."
  (let* ((names '(nl_cons_macro_apply_eval_done
                  nl_cons_macro_apply_eval_rooted
                  nl_cons_macro_apply_eval))
         (saved (mapcar (lambda (name)
                          (cons name (and (fboundp name)
                                          (symbol-function name))))
                        names))
         clone-call cache-call eval-call releases)
    (unwind-protect
        (progn
          (dolist (form (cdr nelisp-standalone--mxcache-macro-apply-eval))
            (eval form))
          (cl-letf (((symbol-function 'seq)
                     (lambda (&rest values) (car (last values))))
                    ((symbol-function 'nl_rootstack_init) (lambda () 0))
                    ((symbol-function 'nl_root_mark) (lambda () 777))
                    ((symbol-function 'nl_root_reserve) (lambda () 900))
                    ((symbol-function 'nl_root_release)
                     (lambda (marker) (push marker releases) 0))
                    ((symbol-function 'nl_cons_cdr_ptr) (lambda (_ptr) 20))
                    ((symbol-function 'sexp-tag) (lambda (_ptr) 7))
                    ((symbol-function 'nl_cons_car_ptr) (lambda (_ptr) 30))
                    ((symbol-function 'nl_apply_function)
                     (lambda (_func _tail _env out)
                       (should (= out 900))
                       0))
                    ((symbol-function 'alloc-bytes)
                     (lambda (size align)
                       (should (= size 32))
                       (should (= align 8))
                       600))
                    ((symbol-function 'nl_sexp_clone_into)
                     (lambda (src dst)
                       (setq clone-call (list src dst))
                       0))
                    ((symbol-function 'nl_mxcache_store)
                     (lambda (form expansion)
                       (setq cache-call (list form expansion))
                       0))
                    ((symbol-function 'nelisp_eval_call)
                     (lambda (form env out)
                       (setq eval-call (list form env out))
                       0)))
            (should (= (nl_cons_macro_apply_eval 10 11 12 13 14) 0))
            (should (equal clone-call '(900 600)))
            (should (equal cache-call '(10 600)))
            (should (equal eval-call '(600 13 14)))
            (should (equal releases '(777)))))
      (dolist (entry saved)
        (if (cdr entry)
            (fset (car entry) (cdr entry))
          (fmakunbound (car entry)))))))

(ert-deftest nelisp-standalone-target-rootstack-reserve-enforces-region-end ()
  "The final root slot fits exactly; the first slot past it is rejected."
  (let ((orig (and (fboundp 'nl_root_reserve_slot)
                   (symbol-function 'nl_root_reserve_slot)))
        (nl_rootstack_region 'nl_rootstack_region)
        (nl_rootstack_top 'nl_rootstack_top)
        writes)
    (unwind-protect
        (progn
          (eval
           (or (nelisp-standalone-target-test--find-defun
                nelisp-cc-rootstack--source 'nl_root_reserve_slot)
               (error "nl_root_reserve_slot not found")))
          (cl-letf (((symbol-function 'seq)
                     (lambda (&rest values) (car (last values))))
                    ((symbol-function 'data-addr)
                     (lambda (sym)
                       (pcase sym
                         ('nl_rootstack_region 1000)
                         ('nl_rootstack_top 2000)
                         (_ (error "unexpected symbol %S" sym)))))
                    ((symbol-function 'ptr-write-u64)
                     (lambda (addr off value)
                       (push (list addr off value) writes)
                       1)))
            (let ((last-slot (+ 1000 1048576 -32)))
              (should (= (nl_root_reserve_slot last-slot) last-slot))
              (should (member (list 2000 0 (+ 1000 1048576)) writes)))
            (setq writes nil)
            (should (= (nl_root_reserve_slot (+ 1000 1048576)) 0))
            (should-not writes)))
      (if orig
          (fset 'nl_root_reserve_slot orig)
        (fmakunbound 'nl_root_reserve_slot)))))

(ert-deftest nelisp-standalone-target-rootstack-tracks-slots-by-address ()
  "Tracked roots preserve slot identity and are dereferenced by the marker."
  (let ((source (prin1-to-string nelisp-cc-rootstack--source)))
    (dolist (needle '("(defun nl_root_track (target)"
                      "(ptr-write-u64 slot 0 255)"
                      "(ptr-write-u64 (+ slot 8) 0 target)"
                      "(while (< cur end)"
                      "(setq cur (+ cur 32))"
                      "(extern-call nl_gc_mark_recorded_slot (ptr-read-u64 (+ cur 8) 0))"))
      (should (string-match-p (regexp-quote needle) source)))
    (should-not (string-match-p
                 (regexp-quote "(nl_gc_mark_rootstack_walk (+ p 32) end)")
                 source))))

(ert-deftest nelisp-standalone-target-flat-dump-follows-tracked-root-addresses ()
  "Flat relocation mirrors GC handling of tracked root descriptors."
  (let ((source
         (prin1-to-string nelisp-standalone--applyfn-core-helpers)))
    (dolist (needle '("(defun nl_fa_rootstack_walk"
                      "(while (< cur end)"
                      "(= (ptr-read-u64 cur 0) 255)"
                      "(nl_fa_recorded_slot (ptr-read-u64 (+ cur 8) 0)"
                      "(setq cur (+ cur 32))"))
      (should (string-match-p (regexp-quote needle) source)))
    (should-not
     (string-match-p
      (regexp-quote "(nl_fa_rootstack_walk (+ p 32) end")
      source))))

(ert-deftest nelisp-standalone-target-arglist-temporaries-use-root-stack ()
  "Arg-list rest/eval destinations are roots and are never manually recycled."
  (let ((source (prin1-to-string nelisp-standalone--arglist-source)))
    (dolist (needle '("(nl_rootstack_init)"
                      "(marker (nl_root_mark))"
                      "(rest_slot (nl_root_reserve))"
                      "(eval_slot (nl_root_reserve))"
                      "(nl_root_release marker)"))
      (should (string-match-p (regexp-quote needle) source)))
    (should-not (string-match-p
                 (regexp-quote "(rest_slot (alloc-bytes 32 8))") source))
    (should-not (string-match-p
                 (regexp-quote "(eval_slot (alloc-bytes 32 8))") source))
    (should-not (string-match-p
                 (regexp-quote "(nl_arg_slot_recycle rest_slot") source))
    (should-not (string-match-p
                 (regexp-quote "(nl_arg_slot_recycle eval_slot") source))))

(ert-deftest nelisp-standalone-target-arglist-root-frame-balances-without-recycle ()
  "A rooted arg-list step releases its marker and does not free BSS roots."
  (let* ((names '(nl_write_nil_slot
                  nl_arg_slot_recycle
                  nl_eval_arg_list_walk_done
                  nl_eval_arg_list_walk_rooted
                  nl_eval_arg_list_walk
                  nl_eval_arg_list))
         (saved (mapcar (lambda (name)
                          (cons name (and (fboundp name)
                                          (symbol-function name))))
                        names))
         (reserves '(100 200))
         releases)
    (unwind-protect
        (progn
          (dolist (form (cdr nelisp-standalone--arglist-source))
            (eval form))
          (cl-letf (((symbol-function 'seq)
                     (lambda (&rest values) (car (last values))))
                    ((symbol-function 'ptr-read-u64)
                     (lambda (ptr _off) (if (= ptr 1) 7 0)))
                    ((symbol-function 'nl_rootstack_init) (lambda () 0))
                    ((symbol-function 'nl_root_mark) (lambda () 555))
                    ((symbol-function 'nl_root_reserve)
                     (lambda () (pop reserves)))
                    ((symbol-function 'nl_root_release)
                     (lambda (marker) (push marker releases) 0))
                    ((symbol-function 'nl_cons_car_ptr) (lambda (_ptr) 10))
                    ((symbol-function 'nl_cons_cdr_ptr) (lambda (_ptr) 99))
                    ((symbol-function 'nl_val_tag) (lambda (_ptr) 2))
                    ((symbol-function 'nl_val_store_word)
                     (lambda (ptr) (if (= ptr 10) 41 3)))
                    ((symbol-function 'nl_write_nil_slot) (lambda (_slot) 0))
                    ((symbol-function 'nelisp_cons_construct)
                     (lambda (&rest _) 1))
                    ((symbol-function 'nl_arg_slot_recycle)
                     (lambda (&rest _)
                       (error "root-stack slot reached manual recycler"))))
            (should (= (nl_eval_arg_list_walk 1 2 300) 0))
            (should (equal releases '(555)))))
      (dolist (entry saved)
        (if (cdr entry)
            (fset (car entry) (cdr entry))
          (fmakunbound (car entry)))))))

(ert-deftest nelisp-standalone-target-garbage-collect-builtin-only-requests ()
  "Both apply dispatches defer Lisp `garbage-collect' to a safe boundary."
  (dolist (table (list nelisp-standalone--applyfn-dispatch-table
                       nelisp-standalone--applyfn-bf-arms))
    (let* ((arm (cdr (assoc '(:lit "garbage-collect") table)))
           (source (prin1-to-string arm)))
      (should arm)
      (should (string-match-p (regexp-quote "(nl_gc_request)") source))
      (should (string-match-p
               (regexp-quote "(bf_arena_stats out)") source))
      (should-not
       (string-match-p "nl_gc_collect_from_recorded_roots" source)))))

(ert-deftest nelisp-standalone-target-gc-request-sets-pending ()
  "A Lisp GC request sets only the driver-owned pending flag."
  (let ((orig (and (fboundp 'nl_gc_request)
                   (symbol-function 'nl_gc_request)))
        (nl_gc_pending 'nl_gc_pending)
        writes)
    (unwind-protect
        (progn
          (eval (nelisp-standalone-target-test--read-gc-defun
                 "nl_gc_request"))
          (cl-letf (((symbol-function 'data-addr)
                     (lambda (sym)
                       (should (eq sym 'nl_gc_pending))
                       5000))
                    ((symbol-function 'ptr-write-u64)
                     (lambda (addr off value)
                       (push (list addr off value) writes)
                       1))
                    ((symbol-function 'nl_seq2) (lambda (_a b) b)))
            (should (= (nl_gc_request) 0))
            (should (equal writes '((5000 0 1))))))
      (if orig
          (fset 'nl_gc_request orig)
        (fmakunbound 'nl_gc_request)))))

(ert-deftest nelisp-standalone-target-gc-boundary-due-honors-depth-and-request ()
  "Nested evaluation suppresses both requested and threshold GC."
  (let ((orig (and (fboundp 'nl_gc_form_boundary_due_p)
                   (symbol-function 'nl_gc_form_boundary_due_p)))
        (nl_gc_loop_ctx 'nl_gc_loop_ctx)
        (nl_gc_pending 'nl_gc_pending)
        (depth 1)
        (pending 1)
        (reserved 100)
        (threshold 200))
    (unwind-protect
        (progn
          (eval (nelisp-standalone-target-test--read-gc-defun
                 "nl_gc_form_boundary_due_p"))
          (cl-letf (((symbol-function 'data-addr)
                     (lambda (sym)
                       (pcase sym
                         ('nl_gc_loop_ctx 1000)
                         ('nl_gc_pending 2000)
                         (_ (error "unexpected data-addr %S" sym)))))
                    ((symbol-function 'ptr-read-u64)
                     (lambda (addr off)
                       (pcase (list addr off)
                         (`(1000 0) depth)
                         (`(2000 0) pending)
                         (`(268436184 0) reserved)
                         (`(268435560 0) threshold)
                         (_ (error "unexpected read %S/%S" addr off))))))
            (should (= (nl_gc_form_boundary_due_p) 0))
            (setq depth 0)
            (should (= (nl_gc_form_boundary_due_p) 1))
            (setq pending 0)
            (should (= (nl_gc_form_boundary_due_p) 0))
            (setq reserved 200)
            (should (= (nl_gc_form_boundary_due_p) 1))))
      (if orig
          (fset 'nl_gc_form_boundary_due_p orig)
        (fmakunbound 'nl_gc_form_boundary_due_p)))))

(ert-deftest nelisp-standalone-target-gc-loops-use-boundary-safe-point ()
  "All three multi-form loops route collection decisions through the depth gate."
  (cl-labels ((count-safe-points
               (tree)
               (cond
                ((atom tree) 0)
                ((equal tree '(nl_gc_form_boundary_due_p)) 1)
                (t (+ (count-safe-points (car tree))
                      (count-safe-points (cdr tree)))))))
    (should (= (count-safe-points nelisp-standalone--applyfn-bf-helpers) 2))
    (should (= (count-safe-points
                nelisp-standalone--reader-eval-source-source)
               1))))

(ert-deftest nelisp-standalone-target-source-failure-diagnostic-shape ()
  "The BF unit exports a cursor-first failing-form stderr diagnostic."
  (let* ((helper
          (seq-find
           (lambda (form)
             (and (eq (car-safe form) 'defun)
                  (eq (cadr form) 'bf_report_failing_source)))
           nelisp-standalone--applyfn-bf-helpers))
         (source (prin1-to-string helper))
         (cursor-pos
          (string-match
           (regexp-quote "(m5_prin1 cursor_ms cursor)") source))
         (form-pos
          (string-match
           (regexp-quote "(m5_prin1 form_ms result)") source)))
    (should helper)
    (should cursor-pos)
    (should form-pos)
    (should (< cursor-pos form-pos))
    (dolist (needle '("(nl_os_write_stderr prefix 30)"
                      "(nl_os_write_stderr separator 6)"
                      "(ptr-write-u8 newline 0 10)"
                      "(nl_os_write_stderr newline 1)"))
      (should (string-match-p (regexp-quote needle) source)))))

(ert-deftest nelisp-standalone-target-eval-stack-diagnostic-shape ()
  "The BF stack reporter walks the newest 16 recorded forms oldest-first."
  (let* ((stack
          (seq-find
           (lambda (form)
             (and (eq (car-safe form) 'defun)
                  (eq (cadr form) 'bf_report_eval_stack)))
           nelisp-standalone--applyfn-bf-helpers))
         (walk
          (nelisp-standalone-target-test--find-defun
           nelisp-standalone--applyfn-bf-helpers
           'bf_report_eval_stack_from))
         (frame
          (nelisp-standalone-target-test--find-defun
           nelisp-standalone--applyfn-bf-helpers 'bf_report_eval_frame))
         (stack-source (prin1-to-string stack))
         (walk-source (prin1-to-string walk))
         (frame-source (prin1-to-string frame)))
    (dolist (needle '("(depth (ptr-read-u64 (data-addr nl_gc_loop_ctx) 0))"
                      "(start (if (> depth 16) (- depth 16) 0))"
                      "(bf_report_eval_stack_from start depth)"))
      (should (string-match-p (regexp-quote needle) stack-source)))
    (dolist (needle '("(+ 64 (* i 56))"
                      "(form (ptr-read-u64 base 8))"
                      "(if (= form 0) 0 (bf_report_eval_frame form))"
                      "(bf_report_eval_stack_from (+ i 1) depth)"))
      (should (string-match-p (regexp-quote needle) walk-source)))
    (dolist (needle '("(m5_prin1 form_ms form)"
                      "(nl_os_write_stderr prefix 24)"
                      "(nl_os_write_stderr newline 1)"))
      (should (string-match-p (regexp-quote needle) frame-source)))))

(ert-deftest nelisp-standalone-target-env-lookup-reports-stack-only-on-failure ()
  "Simple env lookup preserves rc and diagnoses only its nonzero branch."
  (let* ((definition
          (nelisp-standalone-target-test--find-defun
           nelisp-cc-evalport-env-leaves-simple--source
           'nl_env_lookup_val))
         (source (prin1-to-string definition)))
    (should
     (string-match-p
      (regexp-quote
       "(if (= rc 0) rc (seq (bf_report_eval_stack) rc))")
      source))
    (should (= (length
                (split-string source "(bf_report_eval_stack)" t))
               2))))

(ert-deftest nelisp-standalone-target-source-loops-report-rc-without-changing-flow ()
  "Both source loops diagnose rc failures while preserving their prior control."
  (let* ((load
          (nelisp-standalone-target-test--find-defun
           nelisp-standalone--applyfn-bf-helpers 'bf_load_eval_loop))
         (eval-source
          (nelisp-standalone-target-test--find-defun
           nelisp-standalone--applyfn-bf-helpers
           'bf_eval_source_string_loop))
         (load-source (prin1-to-string load))
         (eval-source-text (prin1-to-string eval-source)))
    (should
     (string-match-p
      (regexp-quote
       "(if (= rc 0) 0 (bf_report_failing_source cursor result))")
      load-source))
    (should-not (string-match-p (regexp-quote "(setq more 2)") load-source))
    (should
     (string-match-p
      (regexp-quote
       "(seq (bf_report_failing_source cursor result) (setq more 2))")
      eval-source-text))))

(ert-deftest nelisp-standalone-target-reader-stashed-error-reports-source-once ()
  "Reader eval diagnoses stashed errors, but not flagless bare aborts."
  (let ((definition
         (nelisp-standalone-target-test--find-defun
          nelisp-standalone--reader-eval-source-source
          'nl_eval_source_all)))
    (cl-labels
        ((count-report
          (tree)
          (cond
           ((atom tree) 0)
           ((equal tree '(bf_report_failing_source cursor result)) 1)
           (t (+ (count-report (car tree))
                 (count-report (cdr tree))))))
         (find-stash-if
          (tree)
          (if (atom tree)
              nil
            (if (and (eq (car-safe tree) 'if)
                     (equal (cadr tree)
                            '(= (ptr-read-u64 268435472 0) 0)))
                tree
              (or (find-stash-if (car tree))
                  (find-stash-if (cdr tree)))))))
      (let* ((stash-if (find-stash-if definition))
             (bare-branch (nth 2 stash-if))
             (stashed-branch (nth 3 stash-if))
             (stashed-source (prin1-to-string stashed-branch)))
        (should stash-if)
        (should (= (count-report definition) 1))
        (should (= (count-report bare-branch) 0))
        (should (eq (car-safe stashed-branch) 'seq))
        (should (equal (cadr stashed-branch)
                       '(bf_report_failing_source cursor result)))
        (should (string-match-p "nl_eval_source_report_error"
                                stashed-source))
        (should (string-match-p "nl_eval_source_print_error"
                                stashed-source))))))

(ert-deftest nelisp-standalone-target-reader-string-octal-escape-shape ()
  "The pure reader lexer dispatches one-to-three-digit octal escapes."
  (let ((source (prin1-to-string nelisp-cc-reader-lexer--source)))
    (dolist (needle '("(defun nelisp_reader_string_octal_digit_p"
                      "(defun nelisp_reader_string_octal_escape"
                      "(defun nelisp_reader_string_octal_finish"
                      "(mut-str-push-byte scratch (+ 192 (/ value 64)))"
                      "(mut-str-push-byte scratch (+ 128 (logand value 63)))"
                      "(+ (* value1 8)"
                      "(nelisp_reader_string_octal_escape str-ptr cursor n scratch)"))
      (should (string-match-p (regexp-quote needle) source)))))

(ert-deftest nelisp-standalone-target-reader-string-octal-escape-runtime ()
  "Octal escapes consume at most three digits without regressing controls."
  (let* ((names '(nelisp_reader_string_octal_digit_p
                  nelisp_reader_string_octal_finish
                  nelisp_reader_string_octal_escape
                  nelisp_reader_string_escape
                  nelisp_reader_string_body))
         (saved (mapcar (lambda (name)
                          (cons name (and (fboundp name)
                                          (symbol-function name))))
                        names))
         current-source
         pushed)
    (unwind-protect
        (progn
          (dolist (name names)
            (eval
             (or (nelisp-standalone-target-test--find-defun
                  nelisp-cc-reader-lexer--source name)
                 (error "reader lexer defun not found: %S" name))))
          (cl-letf (((symbol-function 'str-byte-at)
                     (lambda (_str-ptr index)
                       (aref current-source index)))
                    ((symbol-function 'mut-str-push-byte)
                     (lambda (_scratch byte)
                       (push byte pushed)
                       1))
                    ((symbol-function 'nelisp_reader_prog2)
                     (lambda (_effect value) value)))
            (dolist (case '(("341\"" 4 (195 161))
                            ("15\"" 3 (13))
                            ("11\"" 3 (9))
                            ("3\"" 2 (3))
                            ("0\"" 2 (0))
                            ("3778\"" 5 (195 191 56))
                            ("n\"" 2 (10))))
              (setq current-source (car case)
                    pushed nil)
              (should
               (= (nelisp_reader_string_escape
                   current-source 0 (length current-source) 900)
                  (cadr case)))
              (should (equal (nreverse pushed) (caddr case))))
            ;; A raw CR inside a quoted string remains data; the lexer must
            ;; advance through it without treating it as a replay boundary.
            (setq current-source (string 13 34)
                  pushed nil)
            (should
             (= (nelisp_reader_string_body
                 current-source 0 (length current-source) 900)
                2))
            (should (equal (nreverse pushed) '(13)))
            ;; Exercise the actual host replay spelling: Emacs prints byte
            ;; 225 as `\\341' while leaving several controls raw.
            (setq current-source
                  (prin1-to-string (unibyte-string 225 3 13 9))
                  pushed nil)
            (should
             (= (nelisp_reader_string_body
                 current-source 1 (length current-source) 900)
                (length current-source)))
            (should (equal (nreverse pushed) '(195 161 3 13 9)))))
      (dolist (entry saved)
        (if (cdr entry)
            (fset (car entry) (cdr entry))
          (fmakunbound (car entry)))))))

(ert-deftest nelisp-standalone-target-gc-boundary-keeps-full-pool-visible ()
  "Boundary GC passes the parse pool without consulting a global cap."
  (let ((source
         (prin1-to-string
          (nelisp-standalone-target-test--read-gc-defun
           "nl_gc_collect_form_boundary"))))
    (should
     (string-match-p
      (regexp-quote
       "(nl_gc_collect ctx result out pool src cursor bsym)")
      source))
    (should-not (string-match-p "nl_gc_pool_cap" source))
    (should-not (string-match-p "268436448" source))))

(ert-deftest nelisp-standalone-target-reader-pool-resets-and-tracks-high-water ()
  "Reader reset clears the old used prefix before tracking the new parse."
  (let* ((names '(nelisp_reader_p_slot nelisp_reader_parse_one))
         (saved (mapcar (lambda (name)
                          (cons name (and (fboundp name)
                                          (symbol-function name))))
                        names))
         (memory (make-hash-table :test #'eql))
         (base 1016)
         parse-called)
    (unwind-protect
        (progn
          (dolist (name names)
            (eval
             (or (nelisp-standalone-target-test--find-defun
                  nelisp-cc-reader-parser--source name)
                 (error "reader parser defun not found: %S" name))))
          ;; cap=8, previous used=3, with three deliberately dirty 32B slots.
          (puthash (- base 16) 8 memory)
          (puthash (- base 8) 3 memory)
          (dotimes (i 12)
            (puthash (+ base (* i 8)) (+ i 40) memory))
          (cl-letf (((symbol-function 'ptr-read-u64)
                     (lambda (addr off)
                       (gethash (+ addr off) memory 0)))
                    ((symbol-function 'ptr-write-u64)
                     (lambda (addr off value)
                       (puthash (+ addr off) value memory)
                       value))
                    ((symbol-function 'seq)
                     (lambda (&rest values) (car (last values))))
                    ((symbol-function 'nelisp_reader_p_parse_at)
                     (lambda (_src _cursor _result pool _depth)
                       (setq parse-called t)
                       ;; Reset itself does not call p_slot or retain old used.
                       (should (= (gethash (- pool 8) memory) 0))
                       (dotimes (i 12)
                         (should (= (gethash (+ pool (* i 8)) memory) 0)))
                       ;; The real parser then raises used to max(n+1).
                       (should (= (nelisp_reader_p_slot pool 1) (+ pool 32)))
                       77)))
            (should (= (nelisp_reader_parse_one 1 2 3 base 0) 77))
            (should parse-called)
            (should (= (gethash (- base 8) memory) 2))
            ;; Accessing a lower slot is idempotent; a higher slot raises used.
            (should (= (nelisp_reader_p_slot base 0) base))
            (should (= (gethash (- base 8) memory) 2))
            (should (= (nelisp_reader_p_slot base 4) (+ base 128)))
            (should (= (gethash (- base 8) memory) 5))))
      (dolist (entry saved)
        (if (cdr entry)
            (fset (car entry) (cdr entry))
          (fmakunbound (car entry)))))))

(ert-deftest nelisp-standalone-target-gc-reader-pools-use-own-used-prefix ()
  "Nested pools mark their own owner and used slots without a shared cap."
  (let* ((names '(nl_reader_pool_owner nl_reader_pool_cap
                  nl_reader_pool_used nl_gc_mark_pool_slots nl_gc_mark_pool))
         (saved (mapcar (lambda (name)
                          (cons name (and (fboundp name)
                                          (symbol-function name))))
                        names))
         (memory (make-hash-table :test #'eql))
         marked-blocks
         marked-slots)
    (unwind-protect
        (progn
          (dolist (name names)
            (eval (nelisp-standalone-target-test--read-gc-defun name)))
          ;; Outer: owner=1000/base=1016/cap=3/used=2.
          ;; Inner: owner=2000/base=2016/cap=7/used=4.
          (dolist (cell '((1000 . 3) (1008 . 2)
                          (2000 . 7) (2008 . 4)))
            (puthash (car cell) (cdr cell) memory))
          (cl-letf (((symbol-function 'ptr-read-u64)
                     (lambda (addr off)
                       (gethash (+ addr off) memory 0)))
                    ((symbol-function 'nl_seq2)
                     (lambda (_effect value) value))
                    ((symbol-function 'nl_gc_mark_block)
                     (lambda (address)
                       (push address marked-blocks)
                       1))
                    ((symbol-function 'nl_gc_mark_slot)
                     (lambda (address)
                       (push address marked-slots)
                       1)))
            (nl_gc_mark_pool 1016)
            (nl_gc_mark_pool 2016))
          (should (equal (sort marked-blocks #'<) '(1000 2000)))
          (should
           (equal (sort marked-slots #'<)
                  '(1016 1048 2016 2048 2080 2112))))
      (dolist (entry saved)
        (if (cdr entry)
            (fset (car entry) (cdr entry))
          (fmakunbound (car entry)))))))

(ert-deftest nelisp-standalone-target-reader-pool-reset-preserves-defun-and-closure ()
  "Definitions retained by the environment survive pool reset and GC."
  (nelisp-standalone-target-test--ensure-standalone-reader)
  (let ((result
         (nelisp-standalone-target-test--run-reader-src
          (concat
           "(defun nelisp-pool-survivor (x) (+ x 1))\n"
           "(setq nelisp-pool-closure "
           "      (let ((x 40)) (lambda () (+ x 2))))\n"
           "(garbage-collect)\n"
           "(list (nelisp-pool-survivor 41) "
           "      (funcall nelisp-pool-closure))"))))
    (should (= (plist-get result :exit) 0))
    (should (equal (plist-get result :stdout) "(42 42)\n"))
    (should (equal (plist-get result :stderr) ""))))

(ert-deftest nelisp-standalone-target-gc-boundary-clears-pending ()
  "Boundary collection keeps the full pool cap and consumes the request."
  (let ((orig (and (fboundp 'nl_gc_collect_form_boundary)
                   (symbol-function 'nl_gc_collect_form_boundary)))
        (nl_gc_pending 'nl_gc_pending)
        writes collect-call)
    (unwind-protect
        (progn
          (eval (nelisp-standalone-target-test--read-gc-defun
                 "nl_gc_collect_form_boundary"))
          (cl-letf (((symbol-function 'nl_gc_collect)
                     (lambda (&rest args)
                       (setq collect-call args)
                       77))
                    ((symbol-function 'data-addr)
                     (lambda (sym)
                       (should (eq sym 'nl_gc_pending))
                       5000))
                    ((symbol-function 'ptr-write-u64)
                     (lambda (addr off value)
                       (push (list addr off value) writes)
                       1))
                    ((symbol-function 'seq)
                     (lambda (&rest values) (car (last values)))))
            (should (= (nl_gc_collect_form_boundary 1 2 3 4 5 6 7) 77))
            (should (equal collect-call '(1 2 3 4 5 6 7)))
            (should-not (seq-find (lambda (write)
                                    (= (car write) 268436448))
                                  writes))
            (should (member '(5000 0 0) writes))))
      (if orig
          (fset 'nl_gc_collect_form_boundary orig)
        (fmakunbound 'nl_gc_collect_form_boundary)))))

(ert-deftest nelisp-standalone-target-gc-midform-collect-rearms-from-midform-debt ()
  "Mid-form GC compares and re-arms against alloc debt, not reserved bytes."
  (let ((orig-midform (and (fboundp 'nl_gc_midform_collect)
                           (symbol-function 'nl_gc_midform_collect)))
        (nl_gc_loop_ctx 'nl_gc_loop_ctx)
        (nl_gc_alloc_debt 'nl_gc_alloc_debt)
        writes
        (collect-calls 0))
    (unwind-protect
        (progn
          (eval (nelisp-standalone-target-test--read-gc-defun
                 "nl_gc_midform_collect"))
          (cl-letf (((symbol-function 'data-addr)
                     (lambda (sym)
                       (pcase sym
                         ('nl_gc_loop_ctx 700000)
                         ('nl_gc_alloc_debt 900000)
                         (_ (error "unexpected data-addr %S" sym)))))
                    ((symbol-function 'ptr-read-u64)
                     (lambda (addr off)
                       (pcase (list addr off)
                         (`(700000 8) 1)
                         (`(700000 32) 4)
                         (`(700000 40) 6000)
                         (`(900000 0) 9000)
                         (`(268436184 0) 17)
                         (_ (error "unexpected read %S/%S" addr off)))))
                    ((symbol-function 'ptr-write-u64)
                     (lambda (addr off value)
                       (push (list addr off value) writes)
                       0))
                    ((symbol-function 'nl_gc_collect_from_recorded_roots)
                     (lambda (mode)
                       (setq collect-calls (1+ collect-calls))
                       (should (= mode 0))
                       77))
                    ((symbol-function 'nl_seq2) (lambda (_a b) b)))
            (should (= 0 (nl_gc_midform_collect)))
            (should (= 1 collect-calls))
            (should (member (list 700000 40 (+ 9000 16777216)) writes))
            (should-not (member (list 700000 40 (+ 17 16777216)) writes))))
      (if orig-midform
          (fset 'nl_gc_midform_collect orig-midform)
        (fmakunbound 'nl_gc_midform_collect)))))

(ert-deftest nelisp-standalone-target-eval-call-always-records-gc-context ()
  "Every nested eval records roots, independent of the mid-form GC toggle."
  (let ((source (prin1-to-string nelisp-standalone--shim-source)))
    (should (string-match-p
             (regexp-quote
              "(if (= (nl_gc_eval_ctx_push env form_ptr out 0) 1)")
             source))
    (dolist (needle '("(form_track (nl_root_track form_ptr))"
                      "(out_track (nl_root_track out))"
                      "(ptr-write-u64 out 0 0)"
                      "(ptr-write-u64 (+ out 24) 0 0)"
                      "(nl_root_release marker)"))
      (should (string-match-p (regexp-quote needle) source)))
    (should-not (string-match-p
                 (regexp-quote
                  "(if (= (ptr-read-u64 (data-addr nl_gc_loop_ctx) 8) 1)")
                 source))
    (should (string-match-p
             (regexp-quote
              "(nl_gc_ctx_overflow_push)")
             source))
    (should (string-match-p
             (regexp-quote
              "(nl_gc_ctx_overflow_pop)")
             source))))

(ert-deftest nelisp-standalone-target-eval-call-context-push-balance ()
  "Successful and overflowed eval calls pop only the ownership they acquired."
  (let ((definitions (cdr nelisp-standalone--shim-source))
        (installed nil)
        (push-result 1)
        (recorded-pops 0)
        (overflow-depth 0)
        writes)
    (unwind-protect
        (progn
          (dolist (definition definitions)
            (when (and (consp definition)
                       (eq (car definition) 'defun)
                       (memq (cadr definition)
                             '(nelisp_eval_call_done
                               nelisp_eval_call_recorded_done
                               nelisp_eval_call_overflow_done
                               nelisp_eval_call_rooted_done
                               nelisp_eval_call_stash_excessive_lisp_nesting
                               nelisp_eval_call)))
              (push (cons (cadr definition)
                          (and (fboundp (cadr definition))
                               (symbol-function (cadr definition))))
                    installed)
              (eval definition)))
          (cl-letf (((symbol-function 'ptr-read-u64)
                     (lambda (addr _off)
                       (pcase addr
                         (1096 0)
                         (1104 100)
                         (_ 0))))
                    ((symbol-function 'ptr-write-u64)
                     (lambda (addr _off value)
                       (push (list addr value) writes)
                       value))
                    ((symbol-function 'nl_gc_eval_ctx_push)
                     (lambda (&rest _) push-result))
                    ((symbol-function 'nl_gc_ctx_pop)
                     (lambda () (setq recorded-pops (1+ recorded-pops))))
                    ((symbol-function 'nl_gc_ctx_overflow_push)
                     (lambda () (setq overflow-depth (1+ overflow-depth))))
                    ((symbol-function 'nl_gc_ctx_overflow_pop)
                     (lambda () (setq overflow-depth (1- overflow-depth))))
                    ((symbol-function 'nl_rootstack_init) (lambda () 0))
                    ((symbol-function 'nl_root_mark) (lambda () 700))
                    ((symbol-function 'nl_root_track)
                     (lambda (slot) (+ slot 700)))
                    ((symbol-function 'nl_root_release)
                     (lambda (marker)
                       (should (= marker 700))
                       0))
                    ((symbol-function 'nl_sexp_clone_into)
                     (lambda (_src _dst) 0))
                    ((symbol-function 'nl_eval_inner)
                     (lambda (form _env out _pad)
                       (should (memq form '(10 11)))
                       (should (memq out '(20 21)))
                       (should (if (= push-result 1)
                                   (= recorded-pops 0)
                                 (= overflow-depth 1)))
                       7))
                    ((symbol-function 'nl_seq2) (lambda (_a b) b))
                    ((symbol-function 'seq)
                     (lambda (&rest values) (car (last values)))))
            (should (= (nelisp_eval_call 10 1000 20) 7))
            (should (= recorded-pops 1))
            (should (= overflow-depth 0))
            (setq push-result 0
                  writes nil)
            (should (= (nelisp_eval_call 11 1000 21) 7))
            (should (= recorded-pops 1))
            (should (= overflow-depth 0))
            (should (member '(1096 0) writes))))
      (dolist (entry installed)
        (if (cdr entry)
            (fset (car entry) (cdr entry))
          (fmakunbound (car entry)))))))

(ert-deftest nelisp-standalone-target-gc-recorded-overflow-suppresses-sweep ()
  "An unrecorded overflow frame makes explicit GC a safe no-op."
  (let ((orig-collect (and (fboundp 'nl_gc_collect_from_recorded_roots)
                           (symbol-function 'nl_gc_collect_from_recorded_roots)))
        (nl_gc_loop_ctx 'nl_gc_loop_ctx)
        (sweep-calls 0))
    (unwind-protect
        (progn
          (eval (nelisp-standalone-target-test--read-gc-defun
                 "nl_gc_collect_from_recorded_roots"))
          (cl-letf (((symbol-function 'data-addr) (lambda (_sym) 700000))
                    ((symbol-function 'ptr-read-u64)
                     (lambda (addr off)
                       (if (and (= addr 700000) (= off 48)) 1 0)))
                    ((symbol-function 'nl_gc_sweep)
                     (lambda ()
                       (setq sweep-calls (1+ sweep-calls))
                       77)))
            (should (= (nl_gc_collect_from_recorded_roots 0) 0))
            (should (= sweep-calls 0))))
      (if orig-collect
          (fset 'nl_gc_collect_from_recorded_roots orig-collect)
        (fmakunbound 'nl_gc_collect_from_recorded_roots)))))

(ert-deftest nelisp-standalone-target-arena-freelist-split-block-splits-8704-into-8192-plus-512 ()
  "Fallback first-fit splits a larger block and relinks the remainder."
  (let ((orig-split (and (fboundp 'nl_freelist_split_block)
                         (symbol-function 'nl_freelist_split_block)))
        writes links)
    (unwind-protect
        (progn
          (eval (nelisp-standalone-target-test--read-arena-defun
                 "nl_freelist_split_block"))
          (cl-letf (((symbol-function 'ptr-write-u64)
                     (lambda (addr off value)
                       (push (list addr off value) writes)
                       0))
                    ((symbol-function 'nl_gc_free_block_head)
                     (lambda (bt) (+ 100000 bt)))
                    ((symbol-function 'nl_gc_free_block_link)
                     (lambda (hdr head)
                       (push (list hdr head) links)
                       0))
                    ((symbol-function 'nl_hdr_set_mark)
                     (lambda (&rest _) (error "unexpected whole-block path")))
                    ((symbol-function 'seq)
                     (lambda (&rest forms) (car (last forms))))
                    ((symbol-function 'nl_seq2)
                     (lambda (_a b) b)))
            (should (= (nl_freelist_split_block 1000 8192 8704) 1008))
            (should (member (list 1000 0 8192) writes))
            (should (member (list (+ 1000 8192) 0 514) writes))
            (should (equal links
                           (list (list (+ 1000 8192) (+ 100000 512)))))))
      (if orig-split
          (fset 'nl_freelist_split_block orig-split)
        (fmakunbound 'nl_freelist_split_block)))))

(ert-deftest nelisp-standalone-target-arena-freelist-split-block-consumes-rem8-whole ()
  "A remainder of 8 bytes consumes the whole block and preserves capacity."
  (let ((orig-split (and (fboundp 'nl_freelist_split_block)
                         (symbol-function 'nl_freelist_split_block)))
        mark-calls)
    (unwind-protect
        (progn
          (eval (nelisp-standalone-target-test--read-arena-defun
                 "nl_freelist_split_block"))
          (cl-letf (((symbol-function 'ptr-write-u64)
                     (lambda (&rest _) (error "unexpected split write")))
                    ((symbol-function 'nl_gc_free_block_link)
                     (lambda (&rest _) (error "unexpected remainder link")))
                    ((symbol-function 'nl_hdr_set_mark)
                     (lambda (hdr mark)
                       (push (list hdr mark) mark-calls)
                       0))
                    ((symbol-function 'seq)
                     (lambda (&rest forms) (car (last forms))))
                    ((symbol-function 'nl_seq2)
                     (lambda (_a b) b)))
            (should (= (nl_freelist_split_block 2000 512 520) 2008))
            (should (equal mark-calls '((2000 0))))))
      (if orig-split
          (fset 'nl_freelist_split_block orig-split)
        (fmakunbound 'nl_freelist_split_block)))))

(ert-deftest nelisp-standalone-target-arena-freelist-pop-guarded-keeps-exact-canonical-behavior ()
  "An exact canonical bin member still unlinks and returns the object directly."
  (let ((orig-pop (and (fboundp 'nl_freelist_pop_guarded)
                       (symbol-function 'nl_freelist_pop_guarded)))
        writes mark-calls split-calls)
    (unwind-protect
        (progn
          (eval (nelisp-standalone-target-test--read-arena-defun
                 "nl_freelist_pop_guarded"))
          (cl-letf (((symbol-function 'ptr-read-u64)
                     (lambda (addr _off)
                       (pcase addr
                         (6000 7008)
                         (7008 8123)
                         (_ (error "unexpected read %S" addr)))))
                    ((symbol-function 'ptr-write-u64)
                     (lambda (addr off value)
                       (push (list addr off value) writes)
                       0))
                    ((symbol-function 'nl_gc_in_arena)
                     (lambda (_cur) 1))
                    ((symbol-function 'nl_gc_addr_chunk_end)
                     (lambda (_hdr) 1000000))
                    ((symbol-function 'nl_gc_bt_ok)
                     (lambda (&rest _) 1))
                    ((symbol-function 'nl_hdr_mark)
                     (lambda (_hdr) 2))
                    ((symbol-function 'nl_hdr_bt)
                     (lambda (_hdr) 512))
                    ((symbol-function 'nl_freelist_split_block)
                     (lambda (hdr want bt)
                       (push (list hdr want bt) split-calls)
                       7016))
                    ((symbol-function 'nl_fl_record_trip)
                     (lambda (&rest _) (error "unexpected trip")))
                    ((symbol-function 'nl_seq2)
                     (lambda (_a b) b)))
            (should (= (nl_freelist_pop_guarded 6000 512) 7016))
            (should (equal writes '((6000 0 8123))))
            (should (equal split-calls '((7000 512 512))))))
      (if orig-pop
          (fset 'nl_freelist_pop_guarded orig-pop)
        (fmakunbound 'nl_freelist_pop_guarded)))))

(ert-deftest nelisp-standalone-target-arena-freelist-pop-guarded-splits-oversized-bin-member ()
  "A valid oversized head member is unlinked and split at the requested class."
  (let ((orig-pop (and (fboundp 'nl_freelist_pop_guarded)
                       (symbol-function 'nl_freelist_pop_guarded)))
        writes split-calls)
    (unwind-protect
        (progn
          (eval (nelisp-standalone-target-test--read-arena-defun
                 "nl_freelist_pop_guarded"))
          (cl-letf (((symbol-function 'ptr-read-u64)
                     (lambda (addr _off)
                       (pcase addr
                         (6100 7208)
                         (7208 8333)
                         (_ (error "unexpected read %S" addr)))))
                    ((symbol-function 'ptr-write-u64)
                     (lambda (addr off value)
                       (push (list addr off value) writes)
                       0))
                    ((symbol-function 'nl_gc_in_arena)
                     (lambda (_cur) 1))
                    ((symbol-function 'nl_gc_addr_chunk_end)
                     (lambda (_hdr) 1000000))
                    ((symbol-function 'nl_gc_bt_ok)
                     (lambda (&rest _) 1))
                    ((symbol-function 'nl_hdr_mark)
                     (lambda (_hdr) 2))
                    ((symbol-function 'nl_hdr_bt)
                     (lambda (_hdr) 504))
                    ((symbol-function 'nl_freelist_split_block)
                     (lambda (hdr want bt)
                       (push (list hdr want bt) split-calls)
                       7216))
                    ((symbol-function 'nl_fl_record_trip)
                     (lambda (&rest _) (error "unexpected trip")))
                    ((symbol-function 'nl_seq2)
                     (lambda (_a b) b)))
            (should (= (nl_freelist_pop_guarded 6100 472) 7216))
            (should (equal writes '((6100 0 8333))))
            (should (equal split-calls '((7200 472 504))))))
      (if orig-pop
          (fset 'nl_freelist_pop_guarded orig-pop)
        (fmakunbound 'nl_freelist_pop_guarded)))))

(ert-deftest nelisp-standalone-target-arena-freelist-pop-guarded-rejects-undersized-bin-member ()
  "A head member smaller than WANT is treated as a misbinned corruption case."
  (let ((orig-pop (and (fboundp 'nl_freelist_pop_guarded)
                       (symbol-function 'nl_freelist_pop_guarded)))
        trips writes)
    (unwind-protect
        (progn
          (eval (nelisp-standalone-target-test--read-arena-defun
                 "nl_freelist_pop_guarded"))
          (cl-letf (((symbol-function 'ptr-read-u64)
                     (lambda (addr _off)
                       (pcase addr
                         (6200 7304)
                         (_ (error "unexpected read %S" addr)))))
                    ((symbol-function 'ptr-write-u64)
                     (lambda (addr off value)
                       (push (list addr off value) writes)
                       0))
                    ((symbol-function 'nl_gc_in_arena)
                     (lambda (_cur) 1))
                    ((symbol-function 'nl_gc_addr_chunk_end)
                     (lambda (_hdr) 1000000))
                    ((symbol-function 'nl_gc_bt_ok)
                     (lambda (&rest _) 1))
                    ((symbol-function 'nl_hdr_mark)
                     (lambda (_hdr) 2))
                    ((symbol-function 'nl_hdr_bt)
                     (lambda (_hdr) 4080))
                    ((symbol-function 'nl_freelist_split_block)
                     (lambda (&rest _) (error "unexpected split")))
                    ((symbol-function 'nl_fl_record_trip)
                     (lambda (cur bt want)
                       (push (list cur bt want) trips)
                       0))
                    ((symbol-function 'nl_seq2)
                     (lambda (_a b) b)))
            (should (= (nl_freelist_pop_guarded 6200 4096) 0))
            (should (equal trips '((7304 4080 4096))))
            (should (equal writes '((6200 0 0))))))
      (if orig-pop
          (fset 'nl_freelist_pop_guarded orig-pop)
        (fmakunbound 'nl_freelist_pop_guarded)))))

(ert-deftest nelisp-standalone-target-arena-freelist-take-prefers-exact-before-fallback ()
  "Exact small/large heads win before the fallback first-fit scan."
  (let ((orig-take (and (fboundp 'nl_freelist_take)
                        (symbol-function 'nl_freelist_take))))
    (unwind-protect
        (progn
          (eval (nelisp-standalone-target-test--read-arena-defun
                 "nl_freelist_take"))
          (cl-letf (((symbol-function 'nl_freelist_pop_guarded)
                     (lambda (head want)
                       (pcase (list head want)
                         (`(268435712 32) 4444)
                         (`(2222 8192) 5555)
                         (_ 0))))
                    ((symbol-function 'ptr-read-u64)
                     (lambda (&rest _) 0))
                    ((symbol-function 'nl_large_freelist_head_addr)
                     (lambda (want)
                       (if (= want 8192) 2222 0)))
                    ((symbol-function 'nl_large_freelist_take_upward)
                     (lambda (&rest _)
                       (error "upward search should not run after exact hit")))
                    ((symbol-function 'nl_freelist_scan)
                     (lambda (&rest _) (error "fallback scan should not run")))
                    ((symbol-function 'seq)
                     (lambda (&rest forms) (car (last forms))))
                    ((symbol-function 'nl_seq2)
                     (lambda (_a b) b)))
            (should (= (nl_freelist_take 32) 4444))
            (cl-letf (((symbol-function 'nl_freelist_pop_guarded)
                       (lambda (head want)
                         (if (and (= head 2222) (= want 8192)) 5555 0))))
              (should (= (nl_freelist_take 8192) 5555)))))
      (if orig-take
          (fset 'nl_freelist_take orig-take)
        (fmakunbound 'nl_freelist_take)))))

(ert-deftest nelisp-standalone-target-arena-freelist-small-miss-keeps-legacy-path ()
  "Small scratch requests prefer a successful legacy scan over large bins."
  (let ((orig-take (and (fboundp 'nl_freelist_take)
                        (symbol-function 'nl_freelist_take))))
    (unwind-protect
        (progn
          (eval (nelisp-standalone-target-test--read-arena-defun
                 "nl_freelist_take"))
          (cl-letf (((symbol-function 'nl_freelist_pop_guarded)
                     (lambda (head want)
                       (should (= head (+ 268435696 (- want 16))))
                       0))
                    ((symbol-function 'nl_large_freelist_take_upward)
                     (lambda (&rest _)
                       (error "small request must not search large bins")))
                    ((symbol-function 'ptr-read-u64)
                     (lambda (addr _off)
                       (should (= addr 268435552))
                       9000))
                    ((symbol-function 'nl_freelist_scan)
                     (lambda (prev cur want)
                       (should (= prev 0))
                       (should (= cur 9000))
                       (should (= want 40))
                       7777)))
            (should (= (nl_freelist_take 40) 7777))))
      (if orig-take
          (fset 'nl_freelist_take orig-take)
        (fmakunbound 'nl_freelist_take)))))

(ert-deftest nelisp-standalone-target-arena-freelist-small-legacy-miss-reuses-large-bin ()
  "A small request splits a large bin only after exact and legacy miss."
  (let ((names '("nl_large_freelist_head_addr_by_index"
                 "nl_freelist_split_block"
                 "nl_freelist_pop_guarded"
                 "nl_large_freelist_take_upward"
                 "nl_freelist_take"))
        saved
        writes links
        (nl_large_freelist_heads 'nl_large_freelist_heads))
    (dolist (name names)
      (let ((sym (intern name)))
        (push (cons sym (and (fboundp sym) (symbol-function sym))) saved)))
    (unwind-protect
        (progn
          (dolist (name names)
            (eval (nelisp-standalone-target-test--read-arena-defun name)))
          (cl-letf (((symbol-function 'data-addr)
                     (lambda (_sym) 10000))
                    ((symbol-function 'ptr-read-u64)
                     (lambda (addr _off)
                       (pcase addr
                         ;; Small exact head and legacy list are empty.  The
                         ;; first canonical large class owns a 512-byte block.
                         (268435720 0)
                         (268435552 0)
                         (10000 500008)
                         (500008 0)
                         (_ (error "unexpected read %S" addr)))))
                    ((symbol-function 'ptr-write-u64)
                     (lambda (addr off value)
                       (push (list addr off value) writes)
                       0))
                    ((symbol-function 'nl_gc_in_arena) (lambda (_cur) 1))
                    ((symbol-function 'nl_gc_addr_chunk_end)
                     (lambda (_hdr) 1000000))
                    ((symbol-function 'nl_gc_bt_ok)
                     (lambda (&rest _) 1))
                    ((symbol-function 'nl_hdr_mark) (lambda (_hdr) 2))
                    ((symbol-function 'nl_hdr_bt) (lambda (_hdr) 512))
                    ((symbol-function 'nl_hdr_set_mark)
                     (lambda (&rest _) (error "unexpected whole-block path")))
                    ((symbol-function 'nl_gc_free_block_head)
                     (lambda (bt)
                       (should (= bt 472))
                       (+ 268435696 (- bt 16))))
                    ((symbol-function 'nl_gc_free_block_link)
                     (lambda (hdr head)
                       (push (list hdr head) links)
                       0))
                    ((symbol-function 'nl_fl_record_trip)
                     (lambda (&rest _) (error "unexpected guard trip")))
                    ((symbol-function 'nl_freelist_scan)
                     (lambda (prev cur want)
                       (should (= prev 0))
                       (should (= cur 0))
                       (should (= want 40))
                       0))
                    ((symbol-function 'seq)
                     (lambda (&rest forms) (car (last forms))))
                    ((symbol-function 'nl_seq2) (lambda (_a b) b)))
            (should (= (nl_freelist_take 40) 500008))
            (should (member '(10000 0 0) writes))
            (should (member '(500000 0 40) writes))
            (should (member '(500040 0 474) writes))
            (should (equal links
                           `((500040 ,(+ 268435696 (- 472 16))))))))
      (dolist (entry saved)
        (if (cdr entry)
            (fset (car entry) (cdr entry))
          (fmakunbound (car entry)))))))

(ert-deftest nelisp-standalone-target-arena-freelist-take-reuses-next-larger-bin ()
  "A larger segregated block is split without growing reserved arena bytes."
  (let ((names '("nl_large_bt_class_index"
                 "nl_large_freelist_head_addr"
                 "nl_large_freelist_head_addr_by_index"
                 "nl_freelist_split_block"
                 "nl_freelist_pop_guarded"
                 "nl_large_freelist_take_upward"
                 "nl_freelist_take"))
        saved
        writes links
        (reserved-bytes 8388608)
        (nl_large_freelist_heads 'nl_large_freelist_heads))
    (dolist (name names)
      (let ((sym (intern name)))
        (push (cons sym (and (fboundp sym) (symbol-function sym))) saved)))
    (unwind-protect
        (progn
          (dolist (name names)
            (eval (nelisp-standalone-target-test--read-arena-defun name)))
          (cl-letf (((symbol-function 'data-addr)
                     (lambda (_sym) 10000))
                    ((symbol-function 'ptr-read-u64)
                     (lambda (addr _off)
                       (pcase addr
                         ;; WANT=262144 is class 119.  Class 120 is empty and
                         ;; class 121 owns the only reusable block.
                         (10952 0)
                         (10960 0)
                         (10968 500008)
                         (500008 0)
                         (268435552 0)
                         (_ (error "unexpected read %S" addr)))))
                    ((symbol-function 'ptr-write-u64)
                     (lambda (addr off value)
                       (push (list addr off value) writes)
                       0))
                    ((symbol-function 'nl_gc_in_arena) (lambda (_cur) 1))
                    ((symbol-function 'nl_gc_addr_chunk_end)
                     (lambda (_hdr) 1000000))
                    ((symbol-function 'nl_gc_bt_ok)
                     (lambda (&rest _) 1))
                    ((symbol-function 'nl_hdr_mark) (lambda (_hdr) 2))
                    ((symbol-function 'nl_hdr_bt) (lambda (_hdr) 393216))
                    ((symbol-function 'nl_hdr_set_mark)
                     (lambda (&rest _) (error "unexpected whole-block path")))
                    ((symbol-function 'nl_gc_free_block_head)
                     (lambda (bt)
                       (should (= bt 131072))
                       12000))
                    ((symbol-function 'nl_gc_free_block_link)
                     (lambda (hdr head)
                       (push (list hdr head) links)
                       0))
                    ((symbol-function 'nl_fl_record_trip)
                     (lambda (&rest _) (error "unexpected guard trip")))
                    ((symbol-function 'nl_freelist_scan)
                     (lambda (&rest _)
                       (setq reserved-bytes (+ reserved-bytes 8388608))
                       0))
                    ((symbol-function 'seq)
                     (lambda (&rest forms) (car (last forms))))
                    ((symbol-function 'nl_seq2) (lambda (_a b) b)))
            (should (= (nl_freelist_take 262144) 500008))
            (should (= reserved-bytes 8388608))
            (should (member '(10968 0 0) writes))
            (should (member '(500000 0 262144) writes))
            (should (member '(762144 0 131074) writes))
            (should (equal links '((762144 12000))))))
      (dolist (entry saved)
        (if (cdr entry)
            (fset (car entry) (cdr entry))
          (fmakunbound (car entry)))))))

(ert-deftest nelisp-standalone-target-arena-large-free-block-head-routing ()
  "Free routing preserves exact classes and floors noncanonical BTs into bins."
  (let ((orig-floor (and (fboundp 'nl_bt_floor_class)
                         (symbol-function 'nl_bt_floor_class)))
        (orig-idx (and (fboundp 'nl_large_bt_class_index)
                       (symbol-function 'nl_large_bt_class_index)))
        (orig-head (and (fboundp 'nl_large_freelist_head_addr)
                        (symbol-function 'nl_large_freelist_head_addr)))
        (orig-free-head (and (fboundp 'nl_gc_free_block_head)
                             (symbol-function 'nl_gc_free_block_head))))
    (unwind-protect
        (progn
          (eval (nelisp-standalone-target-test--read-arena-defun
                 "nl_bt_floor_class"))
          (eval (nelisp-standalone-target-test--read-arena-defun
                 "nl_large_bt_class_index"))
          (eval (nelisp-standalone-target-test--read-arena-defun
                 "nl_large_freelist_head_addr"))
          (eval (nelisp-standalone-target-test--read-gc-defun
                 "nl_gc_free_block_head"))
          (let ((nl_large_freelist_heads 'nl_large_freelist_heads))
            (cl-letf (((symbol-function 'data-addr)
                       (lambda (_sym) 81920)))
              (should (= (nl_gc_free_block_head 16) 268435696))
              (should (= (nl_gc_free_block_head 472) (+ 268435696 (- 472 16))))
              (should (= (nl_gc_free_block_head 504) (+ 268435696 (- 472 16))))
              (should (= (nl_gc_free_block_head 512) 81920))
              (should (= (nl_gc_free_block_head 5000) (+ 81920 (* 56 8))))
              (should (= (nl_gc_free_block_head 8192) (+ 81920 (* 57 8))))
              (should (= (nl_gc_free_block_head 300000) (+ 81920 (* 119 8))))
              (should (= (nl_gc_free_block_head 2500000) (+ 81920 (* 147 8))))
              (should (= (nl_gc_free_block_head 473) (+ 268435696 (- 472 16))))
              (should (= (nl_gc_free_block_head 4097) (+ 81920 (* 56 8))))
              (should (= (nl_gc_free_block_head 16777217) 268435552)))))
      (if orig-floor
          (fset 'nl_bt_floor_class orig-floor)
        (fmakunbound 'nl_bt_floor_class))
      (if orig-idx
          (fset 'nl_large_bt_class_index orig-idx)
        (fmakunbound 'nl_large_bt_class_index))
      (if orig-head
          (fset 'nl_large_freelist_head_addr orig-head)
        (fmakunbound 'nl_large_freelist_head_addr))
      (if orig-free-head
          (fset 'nl_gc_free_block_head orig-free-head)
        (fmakunbound 'nl_gc_free_block_head)))))

(ert-deftest nelisp-standalone-target-arena-large-freelist-clear-path ()
  "Compaction free-list clearing also zeroes all 162 large-class heads."
  (let ((orig-large (and (fboundp 'nl_compact_clear_large_fl)
                         (symbol-function 'nl_compact_clear_large_fl)))
        (orig-clear (and (fboundp 'nl_compact_clear_fl)
                         (symbol-function 'nl_compact_clear_fl)))
        writes)
    (unwind-protect
        (progn
          (eval (nelisp-standalone-target-test--read-gc-defun
                 "nl_compact_clear_large_fl"))
          (eval (nelisp-standalone-target-test--read-gc-defun
                 "nl_compact_clear_fl"))
          (let ((nl_large_freelist_heads 'nl_large_freelist_heads))
            (cl-letf (((symbol-function 'nl_seq2)
                       (lambda (_a b) b))
                      ((symbol-function 'data-addr)
                       (lambda (_sym) 122880))
                      ((symbol-function 'ptr-write-u64)
                       (lambda (addr _off value)
                         (push (list addr value) writes)
                         0)))
              (nl_compact_clear_fl 58)
              (should (member (list 268435552 0) writes))
              (should (= 163 (length writes)))
              (should (member (list 122880 0) writes))
              (should (member (list (+ 122880 (* 161 8)) 0) writes)))))
      (if orig-large
          (fset 'nl_compact_clear_large_fl orig-large)
        (fmakunbound 'nl_compact_clear_large_fl))
      (if orig-clear
          (fset 'nl_compact_clear_fl orig-clear)
        (fmakunbound 'nl_compact_clear_fl)))))

(ert-deftest nelisp-standalone-target-gc-rebuild-clear-path-clears-fallback-small-and-large-heads ()
  "Sweep rebuild clears the fallback head, 58 small heads, and 162 large heads."
  (let ((orig-clear (and (fboundp 'nl_gc_rebuild_clear_fl)
                         (symbol-function 'nl_gc_rebuild_clear_fl)))
        writes)
    (unwind-protect
        (progn
          (eval (nelisp-standalone-target-test--read-gc-defun
                 "nl_gc_rebuild_clear_fl"))
          (let ((nl_large_freelist_heads 'nl_large_freelist_heads))
            (cl-letf (((symbol-function 'data-addr)
                       (lambda (_sym) 122880))
                      ((symbol-function 'ptr-write-u64)
                       (lambda (addr _off value)
                         (push (list addr value) writes)
                         0))
                      ((symbol-function 'seq)
                       (lambda (&rest forms) (car (last forms))))
                      ((symbol-function 'nl_seq2)
                       (lambda (_a b) b)))
              (nl_gc_rebuild_clear_fl)
              (should (= 221 (length writes)))
              (should (member (list 268435552 0) writes))
              (should (member (list 268435696 0) writes))
              (should (member (list (+ 268435696 (* 57 8)) 0) writes))
              (should (member (list 122880 0) writes))
              (should (member (list (+ 122880 (* 161 8)) 0) writes)))))
      (if orig-clear
          (fset 'nl_gc_rebuild_clear_fl orig-clear)
        (fmakunbound 'nl_gc_rebuild_clear_fl)))))

(ert-deftest nelisp-standalone-target-gc-rebuild-coalesces-adjacent-free-runs ()
  "Adjacent mark-2 blocks rebuild into one coalesced free run."
  (let ((orig-rebuild (and (fboundp 'nl_gc_rebuild_chunk)
                           (symbol-function 'nl_gc_rebuild_chunk)))
        writes links)
    (unwind-protect
        (progn
          (eval (nelisp-standalone-target-test--read-gc-defun
                 "nl_gc_rebuild_free_run"))
          (eval (nelisp-standalone-target-test--read-gc-defun
                 "nl_gc_rebuild_chunk"))
          (cl-letf (((symbol-function 'ptr-read-u64)
                     (lambda (addr _off)
                       (pcase addr
                         (24 1000)
                         (_ (error "unexpected read %S" addr)))))
                    ((symbol-function 'nl_gc_chunk_end)
                     (lambda (_chunk) 1048))
                    ((symbol-function 'nl_hdr_bt)
                     (lambda (hdr)
                       (pcase hdr
                         (1000 16)
                         (1016 16)
                         (1032 16)
                         (_ (error "unexpected hdr %S" hdr)))))
                    ((symbol-function 'nl_hdr_mark)
                     (lambda (hdr)
                       (pcase hdr
                         (1000 2)
                         (1016 2)
                         (1032 0)
                         (_ 0))))
                    ((symbol-function 'nl_gc_bt_ok)
                     (lambda (hdr bt end)
                       (if (and (member hdr '(1000 1016 1032))
                                (= (+ hdr bt) (min (+ hdr bt) end)))
                           1
                         0)))
                    ((symbol-function 'ptr-write-u64)
                     (lambda (addr off value)
                       (push (list addr off value) writes)
                       0))
                    ((symbol-function 'nl_gc_free_block_head)
                     (lambda (bt) (+ 9000 bt)))
                    ((symbol-function 'nl_gc_free_block_link)
                     (lambda (hdr head)
                       (push (list hdr head) links)
                       0))
                    ((symbol-function 'seq)
                     (lambda (&rest forms) (car (last forms))))
                    ((symbol-function 'nl_seq2)
                     (lambda (_a b) b)))
            (nl_gc_rebuild_chunk 0)
            (should (equal writes '((1000 0 32))))
            (should (equal links '((1000 9032))))))
      (if orig-rebuild
          (fset 'nl_gc_rebuild_chunk orig-rebuild)
        (fmakunbound 'nl_gc_rebuild_chunk)))))

(ert-deftest nelisp-standalone-target-gc-rebuild-splits-runs-around-live-blocks ()
  "A live block breaks coalescing into separate rebuilt free runs."
  (let ((orig-rebuild (and (fboundp 'nl_gc_rebuild_chunk)
                           (symbol-function 'nl_gc_rebuild_chunk)))
        writes links)
    (unwind-protect
        (progn
          (eval (nelisp-standalone-target-test--read-gc-defun
                 "nl_gc_rebuild_free_run"))
          (eval (nelisp-standalone-target-test--read-gc-defun
                 "nl_gc_rebuild_chunk"))
          (cl-letf (((symbol-function 'ptr-read-u64)
                     (lambda (addr _off)
                       (pcase addr
                         (24 1000)
                         (_ (error "unexpected read %S" addr)))))
                    ((symbol-function 'nl_gc_chunk_end)
                     (lambda (_chunk) 1064))
                    ((symbol-function 'nl_hdr_bt)
                     (lambda (hdr)
                       (pcase hdr
                         (1000 16)
                         (1016 16)
                         (1032 16)
                         (1048 16)
                         (_ (error "unexpected hdr %S" hdr)))))
                    ((symbol-function 'nl_hdr_mark)
                     (lambda (hdr)
                       (pcase hdr
                         (1000 2)
                         (1016 0)
                         (1032 2)
                         (1048 2)
                         (_ 0))))
                    ((symbol-function 'nl_gc_bt_ok)
                     (lambda (hdr bt end)
                       (if (and (member hdr '(1000 1016 1032 1048))
                                (<= (+ hdr bt) end))
                           1
                         0)))
                    ((symbol-function 'ptr-write-u64)
                     (lambda (addr off value)
                       (push (list addr off value) writes)
                       0))
                    ((symbol-function 'nl_gc_free_block_head)
                     (lambda (bt) (+ 8000 bt)))
                    ((symbol-function 'nl_gc_free_block_link)
                     (lambda (hdr head)
                       (push (list hdr head) links)
                       0))
                    ((symbol-function 'seq)
                     (lambda (&rest forms) (car (last forms))))
                    ((symbol-function 'nl_seq2)
                     (lambda (_a b) b)))
            (nl_gc_rebuild_chunk 0)
            (should (equal (nreverse writes)
                           '((1000 0 16) (1032 0 32))))
            (should (equal (nreverse links)
                           '((1000 8016) (1032 8032))))))
      (if orig-rebuild
          (fset 'nl_gc_rebuild_chunk orig-rebuild)
        (fmakunbound 'nl_gc_rebuild_chunk)))))

(ert-deftest nelisp-standalone-target-gc-rebuild-does-not-coalesce-across-chunks ()
  "Chunk-local rebuild never merges free runs across chunk boundaries."
  (let ((orig-rebuild (and (fboundp 'nl_gc_rebuild_chunks)
                           (symbol-function 'nl_gc_rebuild_chunks)))
        calls)
    (unwind-protect
        (progn
          (eval (nelisp-standalone-target-test--read-gc-defun
                 "nl_gc_rebuild_chunks"))
          (cl-letf (((symbol-function 'ptr-read-u64)
                     (lambda (addr _off)
                       (pcase addr
                         (49 2)
                         (50 0)
                         (_ (error "unexpected read %S" addr)))))
                    ((symbol-function 'nl_gc_rebuild_chunk)
                     (lambda (chunk)
                       (push chunk calls)
                       0))
                    ((symbol-function 'seq)
                     (lambda (&rest forms) (car (last forms))))
                    ((symbol-function 'nl_seq2)
                     (lambda (_a b) b)))
            (nl_gc_rebuild_chunks 1)
            (should (equal (nreverse calls) '(1 2)))))
      (if orig-rebuild
          (fset 'nl_gc_rebuild_chunks orig-rebuild)
        (fmakunbound 'nl_gc_rebuild_chunks)))))

(ert-deftest nelisp-standalone-target-gc-collect-preserves-return-and-rebuilds-after-sweep ()
  "Form-boundary collect disarms retain scope, rebuilds free-lists, and returns sweep."
  (let ((orig-collect (and (fboundp 'nl_gc_collect)
                           (symbol-function 'nl_gc_collect)))
        writes
        (rebuild-calls 0)
        (retain-addr 1110032))
    (unwind-protect
        (progn
          (set 'nl_gc_retain_scope 'nl_gc_retain_scope)
          (unwind-protect
              (progn
            (eval (nelisp-standalone-target-test--read-gc-defun
                   "nl_gc_collect"))
            (cl-letf (((symbol-function 'data-addr)
                       (lambda (_sym) retain-addr))
                      ((symbol-function 'ptr-read-u64)
                       (lambda (addr _off)
                         (pcase addr
                           (268435616 0)
                           (268435592 0)
                           (268435608 0)
                           (_ 0))))
                      ((symbol-function 'ptr-write-u64)
                       (lambda (addr _off value)
                         (push (list addr value) writes)
                         1))
                      ((symbol-function 'nl_gc_mark_roots)
                       (lambda (&rest _) 0))
                      ((symbol-function 'nl_gc_sweep)
                       (lambda () 77))
                      ((symbol-function 'nl_gc_rebuild_free_lists)
                       (lambda ()
                         (cl-incf rebuild-calls)
                         0))
                      ((symbol-function 'seq)
                       (lambda (&rest forms) (car (last forms))))
                      ((symbol-function 'nl_seq2)
                       (lambda (_a b) b)))
              (should (= (nl_gc_collect 1 2 3 4 5 6 7) 77))
              (should (= rebuild-calls 1))
              (should (member (list retain-addr 1) writes))
                      (should (member (list retain-addr 0) writes))))
            (makunbound 'nl_gc_retain_scope))))
      (if orig-collect
          (fset 'nl_gc_collect orig-collect)
        (fmakunbound 'nl_gc_collect))))

(ert-deftest nelisp-standalone-target-gc-collect-from-recorded-roots-preserves-return-and-rebuilds ()
  "Mid-form collect clears its reentrancy flag, rebuilds free-lists, and returns sweep."
  (let ((orig-collect (and (fboundp 'nl_gc_collect_from_recorded_roots)
                           (symbol-function 'nl_gc_collect_from_recorded_roots)))
        writes
        (rebuild-calls 0))
    (unwind-protect
        (progn
          (let ((loop-ctx 333000))
            (set 'nl_gc_loop_ctx 'nl_gc_loop_ctx)
            (unwind-protect
                (progn
                  (eval (nelisp-standalone-target-test--read-gc-defun
                         "nl_gc_collect_from_recorded_roots"))
                  (cl-letf (((symbol-function 'data-addr)
                             (lambda (_sym) loop-ctx))
                            ((symbol-function 'ptr-read-u64)
                            (lambda (addr _off)
                              (pcase addr
                                (333000 0)
                                 (333024 0)
                                 (268435584 0)
                                 (_ 0))))
                            ((symbol-function 'ptr-write-u64)
                             (lambda (addr _off value)
                               (push (list addr value) writes)
                               1))
                            ((symbol-function 'nl_gc_mark_recorded_contexts)
                             (lambda () 0))
                            ((symbol-function 'nl_gc_mark_rootstack)
                             (lambda () 0))
                            ((symbol-function 'nl_gc_mark_symentry)
                             (lambda () 0))
                            ((symbol-function 'nl_gc_conserv_maybe)
                             (lambda () 0))
                            ((symbol-function 'nl_mxcache_mark_all)
                             (lambda () 0))
                            ((symbol-function 'nl_fvcache_mark_all)
                             (lambda () 0))
                            ((symbol-function 'nl_gc_sweep)
                             (lambda () 77))
                            ((symbol-function 'nl_gc_rebuild_free_lists)
                             (lambda ()
                               (cl-incf rebuild-calls)
                               0))
                            ((symbol-function 'seq)
                             (lambda (&rest forms) (car (last forms))))
                            ((symbol-function 'nl_seq2)
                             (lambda (_a b) b)))
                    (should (= (nl_gc_collect_from_recorded_roots 0) 77))
                    (should (= rebuild-calls 1))
                    (should (member (list 333000 1) writes))
                    (should (member (list 333000 0) writes))))
              (makunbound 'nl_gc_loop_ctx)))))
      (if orig-collect
          (fset 'nl_gc_collect_from_recorded_roots orig-collect)
        (fmakunbound 'nl_gc_collect_from_recorded_roots))))

(ert-deftest nelisp-standalone-target-macos-stage8-rewrites-arena-slots ()
  "macOS Stage 8 rewrites rebased arena metadata to `nl_arena_base' loads."
  (let ((nelisp-standalone--target 'macos-aarch64))
    (should (equal
             (nelisp-standalone--chunk-arena-rewrite
              (nelisp-standalone--rebase-arena-source
               '(seq (ptr-write-u64 268435472 0 1)
                     (atomic-fetch-add 268435544 1)
                     (ptr-write-u64 4096 0 268435456))))
             '(seq
               (ptr-write-u64
                (+ (ptr-read-u64 (data-addr nl_arena_base) 0) 16) 0 1)
               (atomic-fetch-add
                (+ (ptr-read-u64 (data-addr nl_arena_base) 0) 88) 1)
               (ptr-write-u64
                4096 0 (+ (ptr-read-u64 (data-addr nl_arena_base) 0) 0)))))))

(ert-deftest nelisp-standalone-target-macos-arena-init-uses-null-mmap ()
  "macOS chunk-0 init uses mmap(NULL, ...) and stores `nl_arena_base'."
  (let ((nelisp-standalone--target 'macos-aarch64))
    (cl-labels ((tree-member-p
                 (needle tree)
                 (cond
                  ((equal needle tree) t)
                  ((consp tree)
                   (or (tree-member-p needle (car tree))
                       (tree-member-p needle (cdr tree)))))))
      (let ((arena (nelisp-standalone--target-arena-source)))
        (should (tree-member-p
                 '(nl_os_alloc_chunk #x20000000)
                 arena))
        (should (tree-member-p
                 '(ptr-write-u64 (data-addr nl_arena_base) 0 base)
                 arena))
        (should-not (tree-member-p
                     '(syscall-direct 197 #x800000000 8589934592 3 4114 -1 0)
                     arena))
        (should-not (tree-member-p
                     '(syscall-direct 197 #x800000000 #x20000000 3 4114 -1 0)
                     arena))))))

(ert-deftest nelisp-standalone-target-windows-reserves-1g-stack ()
  "Windows standalone reserves a Linux-trampoline-sized native stack."
  (should (= nelisp-standalone--windows-stack-reserve #x40000000)))

(ert-deftest nelisp-standalone-target-windows-imports-virtualfree ()
  "Windows eval and reader link paths import VirtualFree for chunk release."
  (let ((nelisp-standalone--target 'windows-x86_64)
        (nelisp-standalone--manifest '(("probe.o" :helper nil)))
        captured-imports)
    (cl-letf (((symbol-function 'nelisp-standalone--unit-for)
               (lambda (_entry)
                 (nelisp-link-unit-make "probe.obj" nil nil nil)))
              ((symbol-function 'nelisp-standalone--arena-base-slot-unit)
               (lambda ()
                 (nelisp-link-unit-make "arena-base.obj"
                                        (list (cons 'bss 8)) nil nil)))
              ((symbol-function 'nelisp-standalone--output-path)
               (lambda (&optional _reader-p) "/tmp/nelisp-target-test.exe"))
              ((symbol-function 'nelisp-link-units-pe32)
               (lambda (_out _units _entry imports &optional _opts)
                 (setq captured-imports imports)))
              ((symbol-function 'message)
               (lambda (&rest _) nil)))
      (nelisp-standalone-build)
      (should (member "VirtualFree" captured-imports))))
  (should (member "VirtualFree"
                  (cdr (assoc "KERNEL32.dll"
                              (nelisp-standalone--reader-pe-imports))))))

(ert-deftest nelisp-standalone-target-macos-uses-bounded-native-stack ()
  "macOS standalone uses an explicit stack that Darwin can mmap reliably."
  (should (= nelisp-standalone--macos-native-stack-size #x20000000))
  (should (< nelisp-standalone--macos-native-stack-size
             nelisp-standalone--native-stack-size)))

(ert-deftest nelisp-standalone-target-cold-load-installs-live-roots ()
  "Cold-load boot installs globals, frames, and unbound from the image."
  (cl-labels ((tree-member-p
               (needle tree)
               (cond
                ((equal needle tree) t)
                ((consp tree)
                 (or (tree-member-p needle (car tree))
                     (tree-member-p needle (cdr tree)))))))
    (let ((forms (nelisp-standalone--reader-driver-source)))
      (should (tree-member-p '(defun nl_cold_root_ptr (off span ds ib)
                                (if (< off span)
                                    (+ ds off)
                                  (+ ib (- off span))))
                             forms))
      (should (tree-member-p
               '(if (< _cl 0)
                    0
                  (if (= (nl_cold_install_roots globals frames unbound cold_override) 1)
                      0
                    (setq _cl -1)))
               forms))
      (should (tree-member-p '(ptr-write-u8 frames 0 12) forms))
      (should (tree-member-p '(ptr-write-u8 unbound 0 4) forms)))))

(ert-deftest nelisp-standalone-target-artifact-cache-activates-manifest-load-path-before-replay ()
  "Generated cache loader registers and calls path activation before replay."
  (let* ((source
          (nelisp-standalone--artifact-command-cache-dispatch-src
           "/tmp/nelisp-artifact-runtime.el.nelc"))
         (helper-fset
          (string-search
           "(fset 'nelisp-artifact--activate-load-paths"
           source))
         (helper-puthash
          (string-search
           "(puthash 'nelisp-artifact--activate-load-paths"
           source))
         (loader-fset
          (string-search
           "(fset 'nelisp-artifact-load-file"
           source))
         (validate
          (string-search
           "(manifest (nelisp-artifact--validate artifact-path content))"
           source))
         (activate
          (string-search
           "(nelisp-artifact--activate-load-paths\n\
           (plist-get manifest :load-path))"
           source))
         (fast
          (string-search
           "(if (fboundp 'nelisp-artifact--load-private-fast)"
           source)))
    (should (integerp helper-fset))
    (should (integerp helper-puthash))
    (should (integerp loader-fset))
    (should (integerp validate))
    (should (integerp activate))
    (should (integerp fast))
    (should (< helper-fset helper-puthash))
    (should (< helper-puthash loader-fset))
    (should (< validate activate))
    (should (< activate fast))
    ;; Evaluate only the generated helper override and verify that its runtime
    ;; merge semantics stay aligned with the ordinary artifact helper.
    (let ((pos 0)
          (len (length source))
          (helper-form nil))
      (while (and (< pos len) (null helper-form))
        (condition-case nil
            (let* ((read-result (read-from-string source pos))
                   (form (car read-result)))
              (setq pos (cdr read-result))
              (when (and (consp form)
                         (eq (car form) 'fset)
                         (equal (nth 1 form)
                                '(quote nelisp-artifact--activate-load-paths)))
                (setq helper-form form)))
          (end-of-file
           (setq pos len))))
      (should helper-form)
      (let ((old-helper
             (and (fboundp 'nelisp-artifact--activate-load-paths)
                  (symbol-function 'nelisp-artifact--activate-load-paths))))
        (unwind-protect
            (progn
              (eval helper-form)
              (let ((load-path '("existing" "duplicate" "existing" nil)))
                (nelisp-artifact--activate-load-paths
                 '("manifest" nil 42 "" "duplicate" "manifest"))
                (should
                 (equal load-path
                        '("manifest" "duplicate" "existing" nil)))))
          (if old-helper
              (fset 'nelisp-artifact--activate-load-paths old-helper)
            (fmakunbound 'nelisp-artifact--activate-load-paths)))))))

(ert-deftest nelisp-standalone-target-source-cache-activates-manifest-load-path-before-replay ()
  "Compact direct/source loaders share manifest path activation before replay."
  (let* ((source (nelisp-standalone--artifact-source-command-cache-src t))
         (activate-def
          (string-search
           "(defun nelisp-standalone-source-cache--activate-load-paths"
           source))
         (manifest-def
          (string-search
           "(defun nelisp-standalone-source-cache--activate-manifest-load-paths"
           source))
         (loader-def
          (string-search
           "(defun nelisp-standalone-source-cache-load-artifact"
           source))
         (manifest-call
          (and loader-def
               (string-search
                "(nelisp-standalone-source-cache--activate-manifest-load-paths\n\
     artifact-path)"
                source loader-def)))
         (replay-call
          (and loader-def
               (string-search
                "(setq module-result (nelisp-standalone-source-cache--replay-module"
                source loader-def))))
    (should (integerp activate-def))
    (should (integerp manifest-def))
    (should (integerp loader-def))
    (should (integerp manifest-call))
    (should (integerp replay-call))
    (should (< activate-def manifest-def))
    (should (< manifest-def loader-def))
    (should (< manifest-call replay-call))
    (should
     (string-match-p
      (regexp-quote "(when (file-exists-p manifest-path)")
      (substring source manifest-def loader-def)))
    ;; Evaluate the generated common merge helper itself.  Both compact direct
    ;; and source-cache commands use the loader that calls this definition.
    (let* ((read-result (read-from-string source activate-def))
           (helper-form (car read-result))
           (old-helper
            (and (fboundp
                  'nelisp-standalone-source-cache--activate-load-paths)
                 (symbol-function
                  'nelisp-standalone-source-cache--activate-load-paths))))
      (unwind-protect
          (progn
            (eval helper-form)
            (let ((load-path '("existing" "duplicate" "existing" nil)))
              (nelisp-standalone-source-cache--activate-load-paths
               '("manifest" nil 42 "" "duplicate" "manifest"))
              (should
               (equal load-path
                      '("manifest" "duplicate" "existing" nil)))))
        (if old-helper
            (fset 'nelisp-standalone-source-cache--activate-load-paths
                  old-helper)
          (fmakunbound
           'nelisp-standalone-source-cache--activate-load-paths))))))

(ert-deftest nelisp-standalone-target-source-cache-does-not-shadow-features ()
  "Artifact feature traversal must not dynamically shadow global `features'."
  (let* ((source (nelisp-standalone--artifact-source-command-cache-src t))
         (start (string-search
                 "(defun nelisp-standalone-source-cache-load-artifact"
                 source))
         (end (and start
                   (string-search
                    "(defun nelisp-standalone-source-cache--parse-source-args"
                    source start)))
         (loader (and start end (substring source start end))))
    (should loader)
    (should (string-match-p "(provided-features nil)" loader))
    (should (string-match-p "(while provided-features" loader))
    (should-not (string-match-p "(features nil)" loader))
    (should-not (string-match-p "(while features" loader))))

(ert-deftest nelisp-standalone-target-source-cache-ignores-nested-features-key ()
  "The compact direct loader locates only outer artifact feature metadata."
  (let* ((source (nelisp-standalone--artifact-source-command-cache-src t))
         (prefix-pos
          (string-search
           "(defun nelisp-standalone-source-cache--prefix-at-p" source))
         (scanner-pos
          (string-search
           "(defun nelisp-standalone-source-cache--top-key-positions" source))
         (prefix-form (car (read-from-string source prefix-pos)))
         (scanner-form (car (read-from-string source scanner-pos)))
         (content
          "(:format x :module-init ((:eval (quote (:features (nested))))) :features (top))")
         (old-prefix
          (and (fboundp 'nelisp-standalone-source-cache--prefix-at-p)
               (symbol-function
                'nelisp-standalone-source-cache--prefix-at-p)))
         (old-scanner
          (and (fboundp 'nelisp-standalone-source-cache--top-key-positions)
               (symbol-function
                'nelisp-standalone-source-cache--top-key-positions))))
    (unwind-protect
        (progn
          (eval prefix-form)
          (eval scanner-form)
          (let ((positions
                 (nelisp-standalone-source-cache--top-key-positions
                  content 0 "nested-features.nelc")))
            (should
             (string-prefix-p ":module-init " (substring content
                                                          (car positions))))
            (should
             (string-prefix-p ":features " (substring content
                                                       (cdr positions))))
            (should
             (> (cdr positions)
                (string-search ":features (nested)" content)))))
      (if old-prefix
          (fset 'nelisp-standalone-source-cache--prefix-at-p old-prefix)
        (fmakunbound 'nelisp-standalone-source-cache--prefix-at-p))
      (if old-scanner
          (fset 'nelisp-standalone-source-cache--top-key-positions old-scanner)
        (fmakunbound
         'nelisp-standalone-source-cache--top-key-positions)))))

(ert-deftest nelisp-standalone-target-variable-alias-fast-path-has-no-resolver-allocation ()
  "The zero-alias lookup/set/bind entry points retain their direct hot path."
  (dolist (spec
           `((,nelisp-cc-env-lookup-value--source
              nelisp_env_lookup_value)
             (,nelisp-cc-env-set-value--source
              nelisp_env_set_value)
             (,nelisp-cc-env-bind-local--source
              nelisp_env_bind_local)))
    (let* ((source (car spec))
           (name (cadr spec))
           (form (cl-find-if
                  (lambda (item)
                    (and (consp item)
                         (eq (car item) 'defun)
                         (eq (cadr item) name)))
                  (cdr source)))
           (printed (prin1-to-string form)))
      (should form)
      (should-not (string-match-p "(alloc-bytes" printed))
      (should (string-match-p "nelisp_env_variable_alias_count" printed))))
  (let ((lookup (prin1-to-string nelisp-cc-env-lookup-value--source)))
    (should-not (string-match-p "nelisp_env_alias_map_symbol" lookup))
    (should-not (string-match-p "nelisp--variable-aliases" lookup))))

(ert-deftest nelisp-standalone-target-intern-canonicalizes-nil-and-t ()
  "Standalone intern must return the canonical self-evaluating constants."
  (let ((source
         (prin1-to-string nelisp-standalone--applyfn-bf-helpers)))
    (dolist (needle '("(defun bf_str_is_nil (sx)"
                      "(defun bf_str_is_t (sx)"
                      "(if (= (bf_str_is_nil sx) 1) (wf_write_nil out)"
                      "(if (= (bf_str_is_t sx) 1) (wf_write_t out)"))
      (should (string-match-p (regexp-quote needle) source)))
    ;; Both inserting and soft lookup entry points share canonicalisation.
    (should (= (length
                (string-split source
                              (regexp-quote "(bf_str_is_nil sx)")
                              t))
               3))))

(ert-deftest nelisp-standalone-target-variable-alias-uses-dedicated-relocatable-slots ()
  "Alias metadata occupies new record slots covered by normal record tracing."
  (let ((entry-source
         (prin1-to-string nelisp-cc-mirror-alloc-entry--source))
        (env-source
         (prin1-to-string nelisp-cc-env-install-empty--source))
        (prelude
         (with-temp-buffer
           (insert-file-contents nelisp-standalone--prelude-file)
           (buffer-string))))
    (should (string-match-p "(record-make tag-sym-ptr 5 result-slot)"
                            entry-source))
    (should (string-match-p "(record-slot-set result-slot 4 redirect-ptr)"
                            entry-source))
    (should (string-match-p
             "(record-make (vector-ref-ptr scratch-ptr 0) 4 globals-out)"
                            env-source))
    (should (string-match-p
             "(nelisp--set-variable-alias new-alias base-variable)"
             prelude))))

(provide 'nelisp-standalone-target-test)

;;; nelisp-standalone-target-test.el ends here
