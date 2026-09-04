;;; emacs-fileio-builtins-test.el --- ERT for emacs-fileio-builtins  -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for the Layer 2 file I/O bridges + high-level commands.
;; Under host Emacs the unprefixed names stay bound to host C
;; builtins (= the `unless (fboundp ...)' gates skip our defaliases),
;; so behavioural assertions exercise the substrate side via the
;; prefixed `nelisp-ec-*' API + the polyfill-body lambdas where
;; necessary.  Featurep / fboundp parity is checked separately.

;;; Code:

(require 'ert)
(require 'emacs-fileio-builtins)
(require 'cl-lib)

(when (boundp 'native-comp-enable-subr-trampolines)
  (setq native-comp-enable-subr-trampolines nil))

(defvar emacs-fileio-builtins-test--tmp-counter 0)

(defun emacs-fileio-builtins-test--tmp-path (suffix)
  "Return a unique tmp filename ending in SUFFIX (= each call distinct)."
  (setq emacs-fileio-builtins-test--tmp-counter
        (1+ emacs-fileio-builtins-test--tmp-counter))
  (format "/tmp/emacs-fileio-builtins-test-%d-%s-%s"
          (emacs-pid)
          emacs-fileio-builtins-test--tmp-counter
          suffix))

(defmacro emacs-fileio-builtins-test--with-fresh-world (&rest body)
  "Run BODY with a clean substrate state + fileio buffer-files alist."
  (declare (indent 0) (debug (body)))
  `(let ((nelisp-ec--buffers nil)
         (nelisp-ec--current-buffer nil)
         (nelisp-ec--match-data nil)
         (emacs-fileio--buffer-files nil))
     ,@body))

(defun emacs-fileio-builtins-test--read-defun (file marker)
  "Return the source of the form starting at MARKER in FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (when (re-search-forward marker nil t)
      (let* ((form-start (match-beginning 0))
             (form-end (save-excursion
                         (goto-char form-start)
                         (forward-sexp)
                         (point))))
        (buffer-substring form-start form-end)))))

;;;; A. Load cleanly + fboundp parity

(ert-deftest emacs-fileio-builtins-test/require-loads-cleanly ()
  (should (featurep 'emacs-fileio-builtins))
  (dolist (sym '(expand-file-name file-name-absolute-p
                 file-name-directory file-name-nondirectory
                 file-name-as-directory
                 substitute-in-file-name
                 file-name-quoted-p file-name-quote file-name-unquote
                 file-exists-p file-readable-p file-directory-p
                 file-attributes directory-files executable-find
                 delete-file rename-file
                 insert-file-contents write-region
                 buffer-file-name set-visited-file-name
                 locate-library
                 find-file-noselect find-file
                 save-buffer write-file revert-buffer))
    (should (fboundp sym)))
  (should (fboundp 'emacs-fileio-save-buffer-direct))
  (should (boundp 'emacs-fileio--buffer-files)))

(ert-deftest emacs-fileio-builtins-test/standalone-overrides-cover-expand-file-name ()
  "Standalone override inventory includes `expand-file-name' and it maps `~'."
  (should (memq 'expand-file-name emacs-fileio-builtins--standalone-overrides))
  (let ((old-home (getenv "HOME")))
    (unwind-protect
        (progn
          (setenv "HOME" "/tmp/nelisp-home")
          (should (equal "/tmp/nelisp-home" (nelisp-ec-expand-file-name "~")))
          (should (equal "/tmp/nelisp-home/notes.org"
                         (nelisp-ec-expand-file-name "~/notes.org"))))
      (setenv "HOME" old-home))))

(ert-deftest emacs-fileio-builtins-test/init-compat-shims-toggle-and-record-state ()
  (let ((auto-save-visited-mode nil)
        (global-auto-revert-mode nil)
        (current-language-environment "English")
        (language-environment-history nil)
        (buffer-file-coding-system 'utf-8-unix)
        (default-buffer-file-coding-system 'utf-8-unix)
        (default-process-coding-system '(utf-8-unix . utf-8-unix))
        (terminal-coding-system 'utf-8-unix)
        (image-file-name-extensions '("png"))
        (large-file-warning-threshold nil))
    (should-not
     (emacs-fileio-builtins--noop-toggle 'auto-save-visited-mode 1))
    (should auto-save-visited-mode)
    (should-not
     (emacs-fileio-builtins--noop-toggle 'global-auto-revert-mode -1))
    (should-not global-auto-revert-mode)
    (let ((set-language-environment-wrapper
           (lambda (environment)
             (setq current-language-environment
                   (if (and (stringp environment) (> (length environment) 0))
                       environment
                     "English"))
             (setq language-environment-history
                   (cons current-language-environment
                         (delete current-language-environment
                                 language-environment-history)))
             current-language-environment))
          (prefer-coding-system-wrapper
           (lambda (coding-system)
             (setq buffer-file-coding-system coding-system
                   default-buffer-file-coding-system coding-system
                   default-process-coding-system
                   (cons coding-system coding-system))
             coding-system))
          (set-terminal-coding-system-wrapper
           (lambda (coding-system)
             (setq terminal-coding-system coding-system)
             coding-system)))
      (should (equal "Japanese"
                     (funcall set-language-environment-wrapper "Japanese")))
      (should (equal '("Japanese") language-environment-history))
      (should (eq 'utf-8 (funcall prefer-coding-system-wrapper 'utf-8)))
      (should (eq 'utf-8-dos
                  (funcall set-terminal-coding-system-wrapper 'utf-8-dos))))
    (should (equal '("Japanese") language-environment-history))
    (should (eq 'utf-8 buffer-file-coding-system))
    (should (equal '(utf-8 . utf-8) default-process-coding-system))
    (should (eq 'utf-8-dos terminal-coding-system))
    (should (equal '("png") image-file-name-extensions))
    (should-not large-file-warning-threshold)))

(ert-deftest emacs-fileio-builtins-test/install-p-uses-function-cell ()
  "Standalone gates must not trust `fboundp' when the function cell is empty."
  (let ((original-fboundp (symbol-function 'fboundp)))
    (cl-letf (((symbol-function 'fboundp)
               (lambda (symbol)
                 (or (eq symbol 'emacs-fileio-builtins-test--missing)
                     (funcall original-fboundp symbol)))))
      (should (emacs-fileio-builtins--install-function-p
               'emacs-fileio-builtins-test--missing)))))

(ert-deftest emacs-fileio-builtins-test/file-commands-carry-interactive-forms ()
  "The standalone polyfills must be commands, not just callable functions."
  (let* ((file (locate-library "emacs-fileio-builtins"))
         (file (if (and file (string-match-p "\\.elc\\'" file))
                   (concat (substring file 0 (- (length file) 1)))
                 file)))
    (should (and file (file-exists-p file)))
    (let ((s (emacs-fileio-builtins-test--read-defun
              file "(when (emacs-fileio-builtins--install-function-p 'find-file)")))
      (should s)
      (should (string-match-p (regexp-quote "(interactive") s))
      (should (string-match-p "read-file-name" s)))
    (let ((s (emacs-fileio-builtins-test--read-defun
              file "(when (emacs-fileio-builtins--install-function-p 'save-buffer)")))
      (should s)
      (should (string-match-p (regexp-quote "(interactive \"P\")") s)))
    (let ((s (emacs-fileio-builtins-test--read-defun
              file "(when (emacs-fileio-builtins--install-function-p 'write-file)")))
      (should s)
      (should (string-match-p (regexp-quote "(interactive") s))
      (should (string-match-p "read-file-name" s)))))

(ert-deftest emacs-fileio-builtins-test/file-name-quote-roundtrip ()
  (should (file-name-quoted-p (file-name-quote "/tmp/foo")))
  (should (equal "/tmp/foo" (file-name-unquote (file-name-quote "/tmp/foo"))))
  (should (equal "/:/tmp/foo" (file-name-quote "/:/tmp/foo"))))

(ert-deftest emacs-fileio-builtins-test/expand-file-name-uses-local-splitter ()
  (should (equal "/tmp/org.el"
                 (nelisp-ec-expand-file-name "org.el" "/tmp/")))
  (should (equal '("a" "b" "c")
                 (nelisp-ec--split-string-char "/a//b/c/" ?/ t)))
  (let* ((file (locate-library "nelisp-emacs-compat-fileio"))
         ;; Read the .el source, not a compiled .elc (binary) when present.
         (file (if (and file (string-match-p "\\.elc\\'" file))
                   (substring file 0 -1)
                 file)))
    (should (and file (file-readable-p file)))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (should (search-forward "(nelisp-ec--normalize-posix-path expanded)" nil t))
      (goto-char (point-min))
      (should-not (search-forward "~user/ unsupported — leave verbatim" nil t)))))

(ert-deftest emacs-fileio-builtins-test/expand-file-name-home-and-normalization-matrix ()
  "The compat path resolver must match the standalone stdlib contract."
  (let ((old-home (getenv "HOME")))
    (unwind-protect
        (progn
          (setenv "HOME" "/tmp/nelisp-home")
          (dolist (case '(("~" "/ignored" "/tmp/nelisp-home")
                          ("~/pkg/file.el" "/ignored" "/tmp/nelisp-home/pkg/file.el")
                          ("/a//b/../c" "/ignored" "/a/c")
                          ("x/./y/../z" "/base//q/" "/base/q/x/z")
                          ("x" "~" "/tmp/nelisp-home/x")
                          ("x" "~/base/../q" "/tmp/nelisp-home/q/x")
                          ("../../x" "/a/b/" "/x")))
            (should (equal (nth 2 case)
                           (nelisp-ec-expand-file-name
                            (nth 0 case) (nth 1 case)))))
          (should-error (nelisp-ec-expand-file-name "~other/pkg" "/base")))
      (setenv "HOME" old-home))))

(ert-deftest emacs-fileio-builtins-test/substitute-in-file-name-minimal ()
  (let ((process-environment
         (cons "NELISP_EMACS_FILEIO_TEST=/tmp/nelisp-fileio"
               process-environment)))
    (should (equal "/tmp/nelisp-fileio/a"
                   (nelisp-ec-substitute-in-file-name
                    "$NELISP_EMACS_FILEIO_TEST/a")))
    (should (equal "/tmp/nelisp-fileio/b"
                   (nelisp-ec-substitute-in-file-name
                    "${NELISP_EMACS_FILEIO_TEST}/b")))
    (should (equal "$NELISP_EMACS_FILEIO_MISSING/c"
                   (nelisp-ec-substitute-in-file-name
                    "$NELISP_EMACS_FILEIO_MISSING/c")))
    (should (equal "/shadow"
                   (nelisp-ec-substitute-in-file-name
                    "/tmp//shadow")))))

(ert-deftest emacs-fileio-builtins-test/pure-file-name-helper-contracts ()
  (should (nelisp-ec-file-name-absolute-p "/tmp/a"))
  (should (nelisp-ec-file-name-absolute-p "~/a"))
  (should-not (nelisp-ec-file-name-absolute-p "relative"))
  (should (equal "/tmp/"
                 (nelisp-ec-file-name-directory "/tmp/file.txt")))
  (should-not (nelisp-ec-file-name-directory "file.txt"))
  (should (equal "file.txt"
                 (nelisp-ec-file-name-nondirectory "/tmp/file.txt")))
  (should (equal "/tmp/file"
                 (nelisp-ec-file-name-sans-extension "/tmp/file.txt")))
  (should (equal ".bashrc"
                 (nelisp-ec-file-name-sans-extension ".bashrc")))
  (should (equal "/tmp/file/"
                 (nelisp-ec-file-name-as-directory "/tmp/file")))
  (should (equal "/tmp/file/"
                 (nelisp-ec-file-name-as-directory "/tmp/file/"))))

(ert-deftest emacs-fileio-builtins-test/file-readable-p-missing-is-nil ()
  (let ((missing (emacs-fileio-builtins-test--tmp-path "missing.txt")))
    (when (file-exists-p missing)
      (delete-file missing))
    (should-not (nelisp-ec-file-exists-p missing))
    (should-not (nelisp-ec-file-readable-p missing))))

(ert-deftest emacs-fileio-builtins-test/executable-find-prefers-nelisp-sys-access ()
  (let ((seen nil)
        (had-sys (fboundp 'nelisp-sys-access))
        (before-sys (and (fboundp 'nelisp-sys-access)
                         (symbol-function 'nelisp-sys-access)))
        (had-nl (fboundp 'nl-syscall-access))
        (before-nl (and (fboundp 'nl-syscall-access)
                        (symbol-function 'nl-syscall-access))))
    (unwind-protect
        (progn
          (fset 'nelisp-sys-access
                (lambda (file mode)
                  (setq seen (cons (list file mode) seen))
                  (if (equal file "/tools/tool") 0 -1)))
          (fset 'nl-syscall-access
                (lambda (&rest _)
                  (error "nelisp-sys-access should win")))
          (cl-letf (((symbol-function 'getenv)
                     (lambda (variable)
                       (and (equal variable "PATH") "/tools"))))
            (should (equal (nelisp-ec-executable-find "tool")
                           "/tools/tool"))
            (should (member '("/tools/tool" 1) seen))))
      (if had-sys
          (fset 'nelisp-sys-access before-sys)
        (fmakunbound 'nelisp-sys-access))
      (if had-nl
          (fset 'nl-syscall-access before-nl)
        (fmakunbound 'nl-syscall-access)))))

(ert-deftest emacs-fileio-builtins-test/executable-find-uses-access-for-path-walk ()
  (let ((seen nil))
    (cl-letf (((symbol-function 'getenv)
               (lambda (variable)
                 (and (equal variable "PATH") "/nope:/tools")))
              ((symbol-function 'nl-syscall-access)
               (lambda (file mode)
                 (setq seen (cons (list file mode) seen))
                 (if (equal file "/tools/tool") 0 -1)))
              ((symbol-function 'file-exists-p)
               (lambda (&rest _)
                 (error "must use nl-syscall-access")))
              ((symbol-function 'file-executable-p)
               (lambda (&rest _)
                 (error "must use nl-syscall-access"))))
      (should (equal (nelisp-ec-executable-find "tool") "/tools/tool"))
      (should (member '("/nope/tool" 1) seen))
      (should (member '("/tools/tool" 1) seen)))))

(ert-deftest emacs-fileio-builtins-test/executable-find-uses-access-for-explicit-path ()
  (cl-letf (((symbol-function 'nl-syscall-access)
             (lambda (file mode)
               (if (and (equal file "/tools/tool")
                        (eq mode 1))
                   0 -1)))
            ((symbol-function 'file-exists-p)
             (lambda (&rest _)
               (error "must use nl-syscall-access")))
            ((symbol-function 'file-executable-p)
             (lambda (&rest _)
               (error "must use nl-syscall-access"))))
    (should (equal (nelisp-ec-executable-find "/tools/tool") "/tools/tool"))
    (should-not (nelisp-ec-executable-find "/tools/not-executable"))))

;;;; A'. Standalone reader: access(2) via `nelisp--syscall-path-int'
;; On nemacs the only working access backend is the raw syscall-by-number
;; primitive (`nelisp-sys-access' / `nl-syscall-access' are unbound and
;; `nelisp--syscall-stat' misreports).  These tests mock that primitive so
;; the access-preferring branches are exercised under host Emacs too.

(ert-deftest emacs-fileio-builtins-test/access-prefers-syscall-path-int ()
  "`nelisp-ec--access' routes through `nelisp--syscall-path-int' when present."
  (let ((seen nil))
    (cl-letf (((symbol-function 'nelisp--syscall-path-int)
               (lambda (nr file mode)
                 (setq seen (cons (list nr file mode) seen))
                 (if (and (equal file "/bin/sh") (eq mode 0)) 0 -1)))
              ;; The legacy access backends must not be consulted.
              ((symbol-function 'nelisp-sys-access)
               (lambda (&rest _) (error "path-int must win")))
              ((symbol-function 'nl-syscall-access)
               (lambda (&rest _) (error "path-int must win"))))
      (should (eq 0 (nelisp-ec--access "/bin/sh" 0)))
      (should (member (list nelisp-ec--syscall-access-number "/bin/sh" 0) seen))
      (should (eq -1 (nelisp-ec--access "/bin/sh" 1))))))

(ert-deftest emacs-fileio-builtins-test/access-public-wrapper-contract ()
  "`nelisp-ec-access' exposes the integer access(2)-style result."
  (cl-letf (((symbol-function 'nelisp--syscall-path-int)
             (lambda (_nr file mode)
               (cond
                ((and (equal file "/ok") (eq mode 4)) 0)
                ((and (equal file "/missing") (eq mode 0)) -1)
                (t 8)))))
    (should (= 0 (nelisp-ec-access "/ok" 4)))
    (should (= -1 (nelisp-ec-access "/missing" 0)))))

(ert-deftest emacs-fileio-builtins-test/file-exists-p-uses-access-f-ok ()
  "`nelisp-ec-file-exists-p' uses access(F_OK), not the misreporting stat."
  (cl-letf (((symbol-function 'nelisp--syscall-path-int)
             (lambda (_nr file mode)
               (if (and (equal file "/exists") (eq mode 0)) 0 -1)))
            ((symbol-function 'nelisp--syscall-stat)
             (lambda (&rest _) (error "access F_OK must win"))))
    (should (nelisp-ec-file-exists-p "/exists"))
    (should-not (nelisp-ec-file-exists-p "/missing"))))

(ert-deftest emacs-fileio-builtins-test/file-exists-p-standalone-access-miss-does-not-fallback-to-rdf ()
  "A standalone access(2) miss must stay nil even if `rdf' returns a string.
This protects `make-temp-name' / `make-temp-file' from looping forever on
nonexistent temp candidates under the runtime image."
  (cl-letf (((symbol-function 'nelisp--syscall-path-int)
             (lambda (&rest _) 2))
            ((symbol-function 'rdf)
             (lambda (_file) "")))
    (should-not (nelisp-ec-file-exists-p "/tmp/definitely-missing"))))

(ert-deftest emacs-fileio-builtins-test/file-exists-p-falls-back-to-rdf ()
  "`nelisp-ec-file-exists-p' falls back to `rdf' when access cannot prove existence."
  (let ((had-path-int (fboundp 'nelisp--syscall-path-int))
        (old-path-int (and (fboundp 'nelisp--syscall-path-int)
                           (symbol-function 'nelisp--syscall-path-int)))
        (had-rdf (fboundp 'rdf))
        (old-rdf (and (fboundp 'rdf) (symbol-function 'rdf))))
    (unwind-protect
        (progn
          (fset 'nelisp--syscall-path-int
                (lambda (_nr _file _mode) -1))
          (fset 'rdf
                (lambda (file)
                  (cond
                   ((equal file "/readable.svg") "<svg/>")
                   ((equal file "/empty.svg") "")
                   (t (signal 'file-missing (list "missing" file))))))
          (should (nelisp-ec-file-exists-p "/readable.svg"))
          (should (nelisp-ec-file-exists-p "/empty.svg"))
          (should-not (nelisp-ec-file-exists-p "/missing.svg")))
      (if had-path-int
          (fset 'nelisp--syscall-path-int old-path-int)
        (when (fboundp 'nelisp--syscall-path-int)
          (fmakunbound 'nelisp--syscall-path-int)))
      (if had-rdf
          (fset 'rdf old-rdf)
        (when (fboundp 'rdf)
          (fmakunbound 'rdf))))))

(ert-deftest emacs-fileio-builtins-test/rdf-file-exists-helper-contract ()
  "`emacs-fileio-rdf-file-exists-p' is nil without `rdf' and truthy for readable files."
  (let ((had-rdf (fboundp 'rdf))
        (old-rdf (and (fboundp 'rdf) (symbol-function 'rdf))))
    (unwind-protect
        (progn
          (when (fboundp 'rdf)
            (fmakunbound 'rdf))
          (should-not (emacs-fileio-rdf-file-exists-p "/any"))
          (fset 'rdf
                (lambda (file)
                  (if (equal file "/ok.svg")
                      "<svg/>"
                    (error "missing"))))
          (should (emacs-fileio-rdf-file-exists-p "/ok.svg"))
          (should-not (emacs-fileio-rdf-file-exists-p "/missing.svg")))
      (if had-rdf
          (fset 'rdf old-rdf)
        (when (fboundp 'rdf)
          (fmakunbound 'rdf))))))

(ert-deftest emacs-fileio-builtins-test/file-executable-p-uses-access-x-ok ()
  "`nelisp-ec-file-executable-p' wraps access(X_OK) through the primitive."
  (cl-letf (((symbol-function 'nelisp--syscall-path-int)
             (lambda (_nr file mode)
               (if (and (equal file "/bin/tool") (eq mode 1)) 0 -1))))
    (should (nelisp-ec-file-executable-p "/bin/tool"))
    (should-not (nelisp-ec-file-executable-p "/bin/data"))
    (should-error (nelisp-ec-file-executable-p 42))))

(ert-deftest emacs-fileio-builtins-test/executable-find-uses-syscall-path-int ()
  "`nelisp-ec-executable-find' walks PATH with X_OK via the syscall primitive."
  (let ((seen nil))
    (cl-letf (((symbol-function 'getenv)
               (lambda (v) (and (equal v "PATH") "/nope:/tools")))
              ((symbol-function 'nelisp--syscall-path-int)
               (lambda (_nr file mode)
                 (setq seen (cons (list file mode) seen))
                 (if (equal file "/tools/tool") 0 -1))))
      (should (equal "/tools/tool" (nelisp-ec-executable-find "tool")))
      (should (member '("/nope/tool" 1) seen))
      (should (member '("/tools/tool" 1) seen)))))

(ert-deftest emacs-fileio-builtins-test/directory-and-mutation-substrate-contracts ()
  (let* ((root (make-temp-file "nelisp-io-" t))
         (sub (expand-file-name "sub" root))
         (src (expand-file-name "a.txt" sub))
         (dst (expand-file-name "b.txt" sub)))
    (unwind-protect
        (progn
          (should (equal sub (nelisp-ec-make-directory sub t)))
          (should (nelisp-ec-file-directory-p sub))
          (with-temp-file src
            (insert "alpha"))
          (should (equal '("a.txt")
                         (nelisp-ec-directory-files
                          sub nil "\\.txt\\'" nil 1)))
          (should (nelisp-ec-rename-file src dst))
          (should (nelisp-ec-file-exists-p dst))
          (should (nelisp-ec-delete-file dst)))
      (when (file-directory-p root)
        (delete-directory root t)))))

(ert-deftest emacs-fileio-builtins-test/make-temp-file-ignores-unbound-marker-temp-vars ()
  (let ((path nil))
    (cl-letf (((symbol-value 'temp-directory) 'nelisp--unbound-marker)
              ((symbol-value 'temporary-file-directory) 'nelisp--unbound-marker))
      (setq path (make-temp-file "nelisp-ec-temp-")))
    (unwind-protect
        (should (string-prefix-p "/tmp/nelisp-ec-temp-" path))
      (when (and (stringp path) (file-exists-p path))
        (delete-file path)))))

(ert-deftest emacs-fileio-builtins-test/locate-library-finds-el-file ()
  (let* ((dir (emacs-fileio-builtins-test--tmp-path "load-path"))
         (file (expand-file-name "sample-lib.el" dir))
         (subdir (expand-file-name "nested" dir))
         (subfile (expand-file-name "sub-lib.el" subdir))
         (shadow-parent (emacs-fileio-builtins-test--tmp-path "shadow-parent"))
         (shadow-dir (expand-file-name "sample-lib" shadow-parent))
         (shadow-file (expand-file-name "sample-lib.el" shadow-dir)))
    (unwind-protect
        (progn
          (make-directory dir t)
          (make-directory subdir t)
          (make-directory shadow-dir t)
          (with-temp-file file
            (insert "(provide 'sample-lib)\n"))
          (with-temp-file subfile
            (insert "(provide 'sub-lib)\n"))
          (with-temp-file shadow-file
            (insert "(provide 'sample-lib)\n"))
          (should (equal file
                         (emacs-fileio-locate-library
                          "sample-lib" nil (list dir))))
          (should (equal file
                         (emacs-fileio-locate-library
                          "sample-lib.el" nil (list dir))))
          (should (equal subfile
                         (emacs-fileio-locate-library
                          "nested/sub-lib" nil (list dir))))
          (should (equal shadow-file
                         (emacs-fileio-locate-library
                          "sample-lib"
                          nil
                          (list shadow-parent shadow-dir))))
          (cl-letf (((symbol-function 'directory-files)
                     (lambda (&rest _)
                       (error "locate-library must not list directories"))))
            (should (equal file
                           (emacs-fileio-locate-library
                            "sample-lib" nil (list dir))))))
      (when (file-exists-p shadow-file)
        (delete-file shadow-file))
      (when (file-directory-p shadow-dir)
        (delete-directory shadow-dir))
      (when (file-directory-p shadow-parent)
        (delete-directory shadow-parent))
      (when (file-exists-p subfile)
        (delete-file subfile))
      (when (file-directory-p subdir)
        (delete-directory subdir))
      (when (file-exists-p file)
        (delete-file file))
      (when (file-directory-p dir)
        (delete-directory dir)))))

(ert-deftest emacs-fileio-builtins-test/locate-library-standalone-needs-no-opendir ()
  (let* ((dir (emacs-fileio-builtins-test--tmp-path "load-path"))
         (file (expand-file-name "sample-lib.el" dir)))
    (unwind-protect
        (progn
          (make-directory dir t)
          (with-temp-file file
            (insert "(provide 'sample-lib)\n"))
          (cl-letf (((symbol-function 'nl-write-file)
                     (lambda (&rest _) t))
                    ((symbol-function 'directory-files)
                     (lambda (&rest _)
                       (error "standalone fallback must not call directory-files")))
                    ((symbol-function 'nelisp-ec-file-exists-p)
                     (lambda (candidate) (equal candidate file))))
            (should (equal file
                           (emacs-fileio-locate-library
                            "sample-lib" nil (list dir))))))
      (when (file-exists-p file)
        (delete-file file))
      (when (file-directory-p dir)
        (delete-directory dir)))))

(ert-deftest emacs-fileio-builtins-test/safe-directory-probe-blocks-runtime-image-shape ()
  "Standalone/runtime-image locate-library must stay nil without opendir.
The runtime image can look standalone through `files-standalone-runtime-p'
even when `nl-write-file' is absent.  In that shape we still must not
touch `directory-files'."
  (cl-letf (((symbol-function 'files-standalone-runtime-p)
             (lambda () t))
            ((symbol-function 'directory-files)
             (lambda (&rest _)
               (error "runtime-image fallback must not call directory-files"))))
    (should-not (emacs-fileio--safe-directory-probe-p))
    (should-not (emacs-fileio-locate-library
                 "sample-lib"
                 nil
                 (list "/tmp/does-not-matter")))))

;;;; B. Substrate-direct: write + read roundtrip

(ert-deftest emacs-fileio-builtins-test/load-file-is-exact-and-propagates-errors ()
  "The shared fallback must load the exact absolute file without fake success."
  (let ((file (emacs-fileio-builtins-test--tmp-path "load-exact"))
        called)
    (unwind-protect
        (progn
          (with-temp-file file (insert "(setq exact-load-ran t)\n"))
          (cl-letf (((symbol-function 'load)
                     (lambda (&rest args) (setq called args) t)))
            (should (emacs-fileio-builtins-load-file file))
            (should (equal (nelisp-ec-expand-file-name file) (car called)))
            (should (eq t (nth 3 called)))
            (should-not (nth 4 called)))
          (cl-letf (((symbol-function 'load)
                     (lambda (&rest _) (error "exact load failure"))))
            (should-error (emacs-fileio-builtins-load-file file))))
      (when (file-exists-p file) (delete-file file)))))

(ert-deftest emacs-fileio-builtins-test/load-path-tree-bfs-filters-cycles-and-is-idempotent ()
  "Recursive discovery is stable, read-only, cycle-safe, and duplicate-free."
  (let* ((root (make-temp-file "nelisp-load-path-tree-" t))
         (one (expand-file-name "1num" root))
         (alpha (expand-file-name "Alpha" root))
         (beta (expand-file-name "beta" root))
         (child (expand-file-name "Child" beta))
         (skip (expand-file-name "Skip" root))
         (hidden (expand-file-name ".hidden" root))
         (rcs (expand-file-name "RCS" root))
         (cvs (expand-file-name "CVS" root))
         (loop (expand-file-name "loop" beta)))
    (unwind-protect
        (progn
          (dolist (directory (list one alpha beta child skip hidden rcs cvs))
            (make-directory directory t))
          (with-temp-file (expand-file-name ".nosearch" skip))
          (make-symbolic-link root loop)
          (let* ((root-time (file-attribute-modification-time
                             (file-attributes root)))
                 (default-directory root)
                 (load-path (list "/before" root "/after" beta)))
            (emacs-fileio-builtins-normal-top-level-add-subdirs-to-load-path)
            (should (equal load-path
                           (list "/before" root one alpha beta child "/after")))
            (let ((once (copy-sequence load-path)))
              (emacs-fileio-builtins-normal-top-level-add-subdirs-to-load-path)
              (should (equal once load-path)))
            (should (equal root-time
                           (file-attribute-modification-time
                            (file-attributes root))))))
      (delete-directory root t))))

(ert-deftest emacs-fileio-builtins-test/write-region-selects-atomic-append-primitive ()
  "Any non-nil APPEND marker must use `nl-append-file' without a read."
  (let (calls)
    (cl-letf (((symbol-function 'emacs-fileio-builtins--standalone-p)
               (lambda () t))
              ((symbol-function 'nl-write-file)
               (lambda (file bytes)
                 (push (list 'overwrite file bytes) calls)
                 t))
              ((symbol-function 'nl-append-file)
               (lambda (file bytes)
                 (push (list 'append file bytes) calls)
                 t))
              ((symbol-function 'rdf)
               (lambda (&rest _)
                 (error "write-region must not read the destination"))))
      (should (= 4 (emacs-fileio-builtins--local-write-region
                    "base" nil "/tmp/atomic-append" nil nil)))
      (dolist (marker '(t append "marker"))
        (should (= 4 (emacs-fileio-builtins--local-write-region
                      "tail" nil "/tmp/atomic-append" marker nil))))
      (should (equal (nreverse calls)
                     '((overwrite "/tmp/atomic-append" "base")
                       (append "/tmp/atomic-append" "tail")
                       (append "/tmp/atomic-append" "tail")
                       (append "/tmp/atomic-append" "tail")))))))

(ert-deftest emacs-fileio-builtins-test/compat-raw-writer-selects-atomic-append ()
  "The encoded-byte adapter must pass bytes directly to the append primitive."
  (let (calls)
    (cl-letf (((symbol-function 'nl-write-file)
               (lambda (file bytes)
                 (push (list 'overwrite file bytes) calls)
                 t))
              ((symbol-function 'nl-append-file)
               (lambda (file bytes)
                 (push (list 'append file bytes) calls)
                 t)))
      (should (= 2 (nelisp-ec--write-raw-bytes "/tmp/raw" "\351\377" nil)))
      (should (= 2 (nelisp-ec--write-raw-bytes "/tmp/raw" "\351\377" 'append)))
      (should (equal (nreverse calls)
                     '((overwrite "/tmp/raw" "\351\377")
                       (append "/tmp/raw" "\351\377")))))))

(ert-deftest emacs-fileio-builtins-test/write-region-then-insert-file-contents-roundtrip ()
  (emacs-fileio-builtins-test--with-fresh-world
    (let ((path (emacs-fileio-builtins-test--tmp-path "roundtrip.txt"))
          (buf (nelisp-ec-generate-new-buffer "writer")))
      (unwind-protect
          (progn
            (nelisp-ec-with-current-buffer buf
              (nelisp-ec-insert "hello\nworld\n"))
            (nelisp-ec-with-current-buffer buf
              (nelisp-ec-write-region (nelisp-ec-point-min)
                                      (nelisp-ec-point-max)
                                      path))
            (should (nelisp-ec-file-exists-p path))
            (let ((reader (nelisp-ec-generate-new-buffer "reader")))
              (unwind-protect
                  (nelisp-ec-with-current-buffer reader
                    (nelisp-ec-insert-file-contents path)
                    (should (equal "hello\nworld\n"
                                   (nelisp-ec-buffer-string))))
                (nelisp-ec-kill-buffer reader))))
        (when (nelisp-ec-file-exists-p path)
          (nelisp-ec-delete-file path))
        (nelisp-ec-kill-buffer buf)))))

;;;; C. buffer-file-name / set-visited-file-name polyfill body

(defun emacs-fileio-builtins-test--buffer-file-name (&optional buffer)
  (let ((b (or buffer (nelisp-ec-current-buffer))))
    (when (and b (nelisp-ec-buffer-p b)
               (not (nelisp-ec-buffer-killed-p b)))
      (cdr (assq b emacs-fileio--buffer-files)))))

(defun emacs-fileio-builtins-test--set-visited (filename)
  (let ((b (nelisp-ec-current-buffer)))
    (when (and b (nelisp-ec-buffer-p b))
      (setq emacs-fileio--buffer-files
            (cons (cons b filename)
                  (assq-delete-all b emacs-fileio--buffer-files)))
      filename)))

(ert-deftest emacs-fileio-builtins-test/buffer-file-name-roundtrip ()
  (emacs-fileio-builtins-test--with-fresh-world
    (let ((b (nelisp-ec-generate-new-buffer "visited")))
      (unwind-protect
          (nelisp-ec-with-current-buffer b
            (should (null (emacs-fileio-builtins-test--buffer-file-name)))
            (emacs-fileio-builtins-test--set-visited "/tmp/foo.txt")
            (should (equal "/tmp/foo.txt"
                           (emacs-fileio-builtins-test--buffer-file-name)))
            ;; Overwrite same buffer's filename.
            (emacs-fileio-builtins-test--set-visited "/tmp/bar.txt")
            (should (equal "/tmp/bar.txt"
                           (emacs-fileio-builtins-test--buffer-file-name))))
        (nelisp-ec-kill-buffer b)))))

(ert-deftest emacs-fileio-builtins-test/direct-path-expansion-prefers-owned-substrate ()
  (cl-letf (((symbol-function 'nelisp-ec-expand-file-name)
             (lambda (path &optional _default-directory)
               (concat "/owned/" (substring path 2))))
            ((symbol-function 'expand-file-name)
             (lambda (&rest _args) "~/left-literal")))
    (should (equal "/owned/note.org"
                   (emacs-fileio--expand-direct-path "~/note.org")))))

(ert-deftest emacs-fileio-builtins-test/direct-path-expansion-keeps-absolute-and-relative-contracts ()
  (should (equal "/tmp/direct-note.org"
                 (emacs-fileio--expand-direct-path
                  "/tmp/direct-note.org" "/ignored/")))
  (should (equal "/tmp/direct-base/direct-note.org"
                 (emacs-fileio--expand-direct-path
                  "direct-note.org" "/tmp/direct-base/"))))

(ert-deftest emacs-fileio-builtins-test/direct-visit-expands-home-and-loads-exact-content ()
  (emacs-fileio-builtins-test--with-fresh-world
    (let* ((home (make-temp-file "nelisp-fileio-home-" t))
           (path (expand-file-name "fixture.org" home))
           (content "* Fixture\nJapanese UTF-8 content\n")
           (old-home (getenv "HOME"))
           buffer)
      (unwind-protect
          (progn
            (write-region content nil path nil 'silent)
            (setenv "HOME" home)
            (cl-letf (((symbol-function 'expand-file-name)
                       (lambda (name &optional _default-directory)
                         (if (string-prefix-p "~/" name)
                             name
                           path))))
              (setq buffer (emacs-fileio-visit-file-direct "~/fixture.org")))
            (should (nelisp-ec-buffer-p buffer))
            (should (equal path (emacs-fileio-buffer-file-direct buffer)))
            (nelisp-ec-with-current-buffer buffer
              (should (equal content (nelisp-ec-buffer-string)))
              (should-not (nelisp-ec-buffer-modified-p buffer))))
        (setenv "HOME" old-home)
        (when (and buffer (nelisp-ec-buffer-p buffer))
          (nelisp-ec-kill-buffer buffer))
        (when (file-directory-p home)
          (delete-directory home t))))))

;;;; D. find-file-noselect polyfill body

(defun emacs-fileio-builtins-test--find-file-noselect (filename)
  (let ((live nil))
    (dolist (cell emacs-fileio--buffer-files)
      (when (and (nelisp-ec-buffer-p (car cell))
                 (not (nelisp-ec-buffer-killed-p (car cell))))
        (setq live (cons cell live))))
    (setq emacs-fileio--buffer-files (nreverse live)))
  (let* ((abs (nelisp-ec-expand-file-name filename))
         (existing
          (catch 'found
            (dolist (cell emacs-fileio--buffer-files)
              (when (equal abs (cdr cell))
                (throw 'found (car cell))))
            nil)))
    (cond
     (existing existing)
     (t
      (let* ((bname (nelisp-ec-file-name-nondirectory abs))
             (buf (nelisp-ec-generate-new-buffer
                   (if (and bname (> (length bname) 0))
                       bname " *find-file*"))))
        (nelisp-ec-with-current-buffer buf
          (when (nelisp-ec-file-exists-p abs)
            (nelisp-ec-insert-file-contents abs))
          (setq emacs-fileio--buffer-files
                (cons (cons buf abs)
                      (assq-delete-all buf emacs-fileio--buffer-files))))
        buf)))))

(ert-deftest emacs-fileio-builtins-test/find-file-noselect-creates-and-loads ()
  (emacs-fileio-builtins-test--with-fresh-world
    (let ((path (emacs-fileio-builtins-test--tmp-path "load.txt"))
          (writer (nelisp-ec-generate-new-buffer "writer")))
      (unwind-protect
          (progn
            ;; Seed file
            (nelisp-ec-with-current-buffer writer
              (nelisp-ec-insert "fileio test content")
              (nelisp-ec-write-region (nelisp-ec-point-min)
                                      (nelisp-ec-point-max)
                                      path))
            ;; find-file-noselect loads it
            (let ((b (emacs-fileio-builtins-test--find-file-noselect path)))
              (should (nelisp-ec-buffer-p b))
              (nelisp-ec-with-current-buffer b
                (should (equal "fileio test content"
                               (nelisp-ec-buffer-string))))
              ;; Second call returns same buffer (= dedup by filename).
              (should (eq b (emacs-fileio-builtins-test--find-file-noselect path)))))
        (when (nelisp-ec-file-exists-p path)
          (nelisp-ec-delete-file path))
        (nelisp-ec-kill-buffer writer)))))

;;;; E. find-file-noselect for nonexistent file creates empty buffer

(ert-deftest emacs-fileio-builtins-test/find-file-noselect-nonexistent-empty ()
  (emacs-fileio-builtins-test--with-fresh-world
    (let* ((path (emacs-fileio-builtins-test--tmp-path "nope.txt"))
           (b (emacs-fileio-builtins-test--find-file-noselect path)))
      (unwind-protect
          (progn
            (should (nelisp-ec-buffer-p b))
            (nelisp-ec-with-current-buffer b
              (should (equal "" (nelisp-ec-buffer-string)))))
        (nelisp-ec-kill-buffer b)))))

;;;; F. save-buffer polyfill body — write + reload yields same content

(defun emacs-fileio-builtins-test--save-buffer ()
  (let* ((b (nelisp-ec-current-buffer))
         (f (and b (emacs-fileio-builtins-test--buffer-file-name b))))
    (cond
     ((null f) (signal 'error '("save-buffer: not visiting a file")))
     (t
      (nelisp-ec-write-region (nelisp-ec-point-min) (nelisp-ec-point-max) f)
      f))))

(ert-deftest emacs-fileio-builtins-test/save-buffer-flushes-to-visited-file ()
  (emacs-fileio-builtins-test--with-fresh-world
    (let ((path (emacs-fileio-builtins-test--tmp-path "save.txt"))
          (b (nelisp-ec-generate-new-buffer "saver")))
      (unwind-protect
          (nelisp-ec-with-current-buffer b
            (emacs-fileio-builtins-test--set-visited path)
            (nelisp-ec-insert "saved content")
            (emacs-fileio-builtins-test--save-buffer)
            (should (nelisp-ec-file-exists-p path))
            (let ((verify (nelisp-ec-generate-new-buffer "verify")))
              (unwind-protect
                  (nelisp-ec-with-current-buffer verify
                    (nelisp-ec-insert-file-contents path)
                    (should (equal "saved content"
                                   (nelisp-ec-buffer-string))))
                (nelisp-ec-kill-buffer verify))))
        (when (nelisp-ec-file-exists-p path)
          (nelisp-ec-delete-file path))
        (nelisp-ec-kill-buffer b)))))

(ert-deftest emacs-fileio-builtins-test/save-buffer-direct-uses-pluggable-accessors ()
  (let (written modified)
    (cl-letf (((symbol-function 'emacs-buffer-set-buffer-modified-p)
               (lambda (flag buffer)
                 (setq modified (list flag buffer)))))
      (should (equal "/tmp/shared.txt"
                     (emacs-fileio-save-buffer-direct
                      :buffer :buffer
                      :file-function (lambda (buffer)
                                       (and (eq buffer :buffer)
                                            "/tmp/shared.txt"))
                      :string-function (lambda (buffer)
                                         (and (eq buffer :buffer)
                                              "body"))
                      :write-function (lambda (path text)
                                        (setq written (list path text))))))
      (should (equal '("/tmp/shared.txt" "body") written))
      (should (equal '(nil :buffer) modified)))))

(ert-deftest emacs-fileio-builtins-test/save-buffer-direct-signals-when-no-file ()
  (should-error
   (emacs-fileio-save-buffer-direct
    :buffer :buffer
    :file-function (lambda (_buffer) nil)
    :string-function (lambda (_buffer) "body")
    :write-function (lambda (&rest _args) nil))))

(ert-deftest emacs-fileio-builtins-test/run-find-file-command-uses-frontend-hooks ()
  (let (prompts visited synced)
    (should
     (eq :buffer
         (emacs-fileio-run-find-file-command
          :read-string (lambda (prompt)
                         (push prompt prompts)
                         "note.el")
          :visit-function (lambda (path)
                            (setq visited path)
                            :buffer)
          :sync-window (lambda (buffer)
                         (setq synced buffer)))))
    (should (equal "note.el" visited))
    (should (eq synced :buffer))
    (should (equal '("Find file: ") prompts))))

(ert-deftest emacs-fileio-builtins-test/run-find-file-command-reports-cancel ()
  (let (cancelled)
    (should-not
     (emacs-fileio-run-find-file-command
      :read-string (lambda (_prompt) nil)
      :cancel-function (lambda ()
                         (setq cancelled t))))
    (should cancelled)))

(ert-deftest emacs-fileio-builtins-test/run-find-file-command-reports-missing-buffer ()
  (let (missing)
    (should-not
     (emacs-fileio-run-find-file-command
      :read-string (lambda (_prompt) "/tmp/missing")
      :visit-function (lambda (_path) nil)
      :missing-function (lambda (path)
                          (setq missing path))))
    (should (equal "/tmp/missing" missing))))

(ert-deftest emacs-fileio-builtins-test/run-find-file-command-runs-after-success ()
  (let (after)
    (should
     (eq :buffer
         (emacs-fileio-run-find-file-command
          :read-string (lambda (_prompt) "/tmp/note.el")
          :visit-function (lambda (_path) :buffer)
          :after-success (lambda (buffer path)
                           (setq after (list buffer path))))))
    (should (equal '(:buffer "/tmp/note.el") after))))

(ert-deftest emacs-fileio-builtins-test/run-save-buffer-command-saves-visited-file ()
  (let (written)
    (cl-letf (((symbol-function 'emacs-buffer-set-buffer-modified-p)
               (lambda (&rest _args) nil)))
      (should
       (equal
        "/tmp/note.el"
        (emacs-fileio-run-save-buffer-command
         :current-buffer (lambda () :buffer)
         :file-function (lambda (buffer)
                          (and (eq buffer :buffer) "/tmp/note.el"))
         :direct-save-p (lambda () t)
         :message-function (lambda (&rest _args) :unexpected)
         :read-string (lambda (&rest _args) :unexpected)
         :write-function (lambda (path text)
                           (setq written (list path text)))
         :string-function (lambda (buffer)
                            (and (eq buffer :buffer) "body"))))))
    (should (equal '("/tmp/note.el" "body") written))))

(ert-deftest emacs-fileio-builtins-test/run-save-buffer-command-prompts-write-file ()
  (let (prompts wrote)
    (cl-letf (((symbol-function 'write-file)
               (lambda (path)
                 (setq wrote path)
                 :wrote)))
      (should
       (eq :wrote
           (emacs-fileio-run-save-buffer-command
            :current-buffer (lambda () :buffer)
            :file-function (lambda (_buffer) nil)
            :read-string (lambda (prompt)
                           (push prompt prompts)
                           "/tmp/new.el"))))
      (should (equal "/tmp/new.el" wrote))
      (should (equal '("Write file: ") prompts)))))

(ert-deftest emacs-fileio-builtins-test/run-write-file-command-uses-frontend-hooks ()
  (let (prompts wrote after status)
    (should
     (equal "/tmp/written.el"
            (emacs-fileio-run-write-file-command
             :read-string (lambda (prompt)
                            (push prompt prompts)
                            "/tmp/new.el")
             :write-file-function (lambda (path)
                                    (setq wrote path)
                                    "/tmp/written.el")
             :after-success (lambda (written path)
                              (setq after (list written path)))
             :status-function (lambda (message)
                                (setq status message)))))
    (should (equal "/tmp/new.el" wrote))
    (should (equal '("/tmp/written.el" "/tmp/new.el") after))
    (should (equal "Wrote: /tmp/written.el" status))
    (should (equal '("Write file: ") prompts))))

(ert-deftest emacs-fileio-builtins-test/run-write-file-command-reports-empty ()
  (let (status)
    (should-not
     (emacs-fileio-run-write-file-command
      :read-string (lambda (_prompt) "")
      :status-function (lambda (message)
                         (setq status message))))
    (should (equal "write-file: empty path" status))))

(ert-deftest emacs-fileio-builtins-test/run-write-file-command-reports-error ()
  (let (message)
    (should-not
     (emacs-fileio-run-write-file-command
      :read-string (lambda (_prompt) "/tmp/new.el")
      :write-file-function (lambda (_path)
                             (error "denied"))
      :message-function (lambda (format-string &rest args)
                          (setq message
                                (apply #'format format-string args)))))
    (should (equal "write-file: denied" message))))

(ert-deftest emacs-fileio-builtins-test/run-save-buffers-quit-command-no-dirty ()
  (let (quit status)
    (should
     (equal
      "C-x C-c → quit"
      (emacs-fileio-run-save-buffers-quit-command
       :dirty-buffers nil
       :quit-function (lambda () (setq quit t))
       :status-function (lambda (message)
                          (setq status message)))))
    (should quit)
    (should (equal "C-x C-c → quit" status))))

(ert-deftest emacs-fileio-builtins-test/run-save-buffers-quit-command-saves-dirty ()
  (let (prompt callback saved quit status)
    (emacs-fileio-run-save-buffers-quit-command
     :dirty-buffers '(:a :b)
     :begin-prompt (lambda (p cb)
                     (setq prompt p
                           callback cb))
     :save-buffer-function (lambda (buffer)
                             (push buffer saved))
     :quit-function (lambda () (setq quit t))
     :status-function (lambda (message)
                        (setq status message)))
    (should (equal "2 modified buffer(s).  Save? (y/n/c): " prompt))
    (funcall callback "y")
    (should quit)
    (should (equal '(:b :a) saved))
    (should (equal "Saved 2 buffer(s) — quit" status))))

(ert-deftest emacs-fileio-builtins-test/run-save-buffers-quit-command-unsaved ()
  (let (callback quit status saved)
    (emacs-fileio-run-save-buffers-quit-command
     :dirty-buffers '(:a)
     :begin-prompt (lambda (_prompt cb) (setq callback cb))
     :save-buffer-function (lambda (buffer) (push buffer saved))
     :quit-function (lambda () (setq quit t))
     :status-function (lambda (message)
                        (setq status message)))
    (funcall callback "n")
    (should quit)
    (should-not saved)
    (should (equal "Quit (unsaved)" status))))

(ert-deftest emacs-fileio-builtins-test/run-save-buffers-quit-command-cancel ()
  (let (callback quit status saved)
    (emacs-fileio-run-save-buffers-quit-command
     :dirty-buffers '(:a)
     :begin-prompt (lambda (_prompt cb) (setq callback cb))
     :save-buffer-function (lambda (buffer) (push buffer saved))
     :quit-function (lambda () (setq quit t))
     :status-function (lambda (message)
                        (setq status message)))
    (funcall callback "c")
    (should-not quit)
    (should-not saved)
    (should (equal "Quit cancelled" status))))

;;;; G. save-buffer signals when not visiting

(ert-deftest emacs-fileio-builtins-test/save-buffer-signals-when-no-file ()
  (emacs-fileio-builtins-test--with-fresh-world
    (let ((b (nelisp-ec-generate-new-buffer "no-file")))
      (unwind-protect
          (nelisp-ec-with-current-buffer b
            (should-error (emacs-fileio-builtins-test--save-buffer)))
        (nelisp-ec-kill-buffer b)))))

;;;; H. revert-buffer reloads file

(defun emacs-fileio-builtins-test--revert-buffer ()
  (let* ((b (nelisp-ec-current-buffer))
         (f (and b (emacs-fileio-builtins-test--buffer-file-name b))))
    (when f
      (nelisp-ec-erase-buffer)
      (nelisp-ec-insert-file-contents f)
      f)))

(ert-deftest emacs-fileio-builtins-test/revert-buffer-reloads-from-disk ()
  (emacs-fileio-builtins-test--with-fresh-world
    (let ((path (emacs-fileio-builtins-test--tmp-path "revert.txt"))
          (writer (nelisp-ec-generate-new-buffer "writer"))
          (visitor (nelisp-ec-generate-new-buffer "visitor")))
      (unwind-protect
          (progn
            ;; Seed disk
            (nelisp-ec-with-current-buffer writer
              (nelisp-ec-insert "original")
              (nelisp-ec-write-region (nelisp-ec-point-min)
                                      (nelisp-ec-point-max)
                                      path))
            ;; Visit + dirty
            (nelisp-ec-with-current-buffer visitor
              (nelisp-ec-insert "dirty in-memory")
              (emacs-fileio-builtins-test--set-visited path)
              (emacs-fileio-builtins-test--revert-buffer)
              (should (equal "original" (nelisp-ec-buffer-string)))))
        (when (nelisp-ec-file-exists-p path)
          (nelisp-ec-delete-file path))
        (nelisp-ec-kill-buffer writer)
        (nelisp-ec-kill-buffer visitor)))))

;;;; I. emacs-fileio--clean-killed drops dead entries

(ert-deftest emacs-fileio-builtins-test/clean-killed-drops-dead-entries ()
  (emacs-fileio-builtins-test--with-fresh-world
    (let ((alive (nelisp-ec-generate-new-buffer "alive"))
          (dead  (nelisp-ec-generate-new-buffer "dead")))
      (setq emacs-fileio--buffer-files
            (list (cons alive "/tmp/a.txt") (cons dead "/tmp/d.txt")))
      (nelisp-ec-kill-buffer dead)
      (emacs-fileio--clean-killed)
      (should (assq alive emacs-fileio--buffer-files))
      (should (null (assq dead emacs-fileio--buffer-files)))
      (nelisp-ec-kill-buffer alive))))

;;;; J. Idempotence

(ert-deftest emacs-fileio-builtins-test/require-is-idempotent ()
  (let ((before-find-file  (symbol-function 'find-file))
        (before-save-buf   (symbol-function 'save-buffer))
        (before-buf-file   (symbol-function 'buffer-file-name)))
    (require 'emacs-fileio-builtins)
    (should (eq before-find-file (symbol-function 'find-file)))
    (should (eq before-save-buf  (symbol-function 'save-buffer)))
    (should (eq before-buf-file  (symbol-function 'buffer-file-name)))))

;;;; make-temp-file polyfill (standalone reader; host keeps its C builtin)

(ert-deftest emacs-fileio-builtins-test/make-temp-file-creates-unique ()
  (let ((f1 (emacs-fileio-make-temp-file "ap-ert-" nil ".json"))
        (f2 (emacs-fileio-make-temp-file "ap-ert-" nil ".json")))
    (unwind-protect
        (progn
          (should (stringp f1))
          (should (file-exists-p f1))
          (should (string-suffix-p ".json" f1))
          (should-not (equal f1 f2)))
      (when (file-exists-p f1) (delete-file f1))
      (when (file-exists-p f2) (delete-file f2)))))

(ert-deftest emacs-fileio-builtins-test/make-temp-file-writes-text ()
  (let ((f (emacs-fileio-make-temp-file "ap-ert-" nil ".txt" "hello-temp")))
    (unwind-protect
        (should (equal "hello-temp"
                       (with-temp-buffer (insert-file-contents f) (buffer-string))))
      (when (file-exists-p f) (delete-file f)))))

(ert-deftest emacs-fileio-builtins-test/reinstall-standalone-overrides-restores-write-region ()
  ;; The reinstall entry point force-installs THREE overrides
  ;; (`write-region' plus the temp shims via
  ;; `emacs-fileio-builtins-install-standalone-temp-shims'); restore all
  ;; of them, or the host `make-temp-file' stays shadowed for every
  ;; later suite (measured: broke the tier3 non-VC temp-dir helper).
  (let ((original-write-region (symbol-function 'write-region))
        (original-make-temp-file (symbol-function 'make-temp-file))
        (original-make-temp-name (symbol-function 'make-temp-name)))
    (unwind-protect
        (progn
          (fset 'write-region (lambda (&rest _args) 'shadowed))
          (cl-letf (((symbol-function 'emacs-fileio-builtins--standalone-p)
                     (lambda () t)))
            (emacs-fileio-builtins-reinstall-standalone-overrides))
          (should (eq (symbol-function 'write-region)
                      #'emacs-fileio-builtins-write-region)))
      (fset 'write-region original-write-region)
      (fset 'make-temp-file original-make-temp-file)
      (fset 'make-temp-name original-make-temp-name))))

(ert-deftest emacs-fileio-builtins-test/reinstall-standalone-overrides-restores-temp-shims ()
  (let ((original-make-temp-file (symbol-function 'make-temp-file))
        (original-make-temp-name (symbol-function 'make-temp-name)))
    (unwind-protect
        (progn
          (fset 'make-temp-file (lambda (&rest _args) 'shadowed-file))
          (fset 'make-temp-name (lambda (&rest _args) 'shadowed-name))
          (cl-letf (((symbol-function 'emacs-fileio-builtins--standalone-p)
                     (lambda () t)))
            (emacs-fileio-builtins-reinstall-standalone-overrides))
          (should (eq (symbol-function 'make-temp-file)
                      #'emacs-fileio-make-temp-file))
          (should (eq (symbol-function 'make-temp-name)
                      #'emacs-fileio-make-temp-name)))
      (fset 'make-temp-file original-make-temp-file)
      (fset 'make-temp-name original-make-temp-name))))

(provide 'emacs-fileio-builtins-test)

;;; emacs-fileio-builtins-test.el ends here

(ert-deftest emacs-fileio-builtins-test/copy-file-content-and-overwrite ()
  "copy-file MVP copies byte content and refuses to overwrite by default.
Host shadows `copy-file', so the body's control flow is exercised via a pinned
lambda over the same fileio substrate (parity pattern)."
  (let* ((src (make-temp-file "ecf-src"))
         (dst (concat src "-dst"))
         (cp (lambda (file newname &optional ok)
               (when (and (not ok) (file-exists-p newname)) (error "exists"))
               (with-temp-buffer
                 (insert-file-contents-literally file)
                 (write-region (point-min) (point-max) newname))
               nil)))
    (unwind-protect
        (progn
          (with-temp-buffer
            (insert "hello copy")
            (write-region (point-min) (point-max) src))
          (when (file-exists-p dst) (delete-file dst))
          (funcall cp src dst)
          (should (string= (with-temp-buffer
                             (insert-file-contents-literally src) (buffer-string))
                           (with-temp-buffer
                             (insert-file-contents-literally dst) (buffer-string))))
          (should-error (funcall cp src dst))
          (should (progn (funcall cp src dst t) t)))
      (ignore-errors (delete-file src))
      (ignore-errors (delete-file dst)))))
