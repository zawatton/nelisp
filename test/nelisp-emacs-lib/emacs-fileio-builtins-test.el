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
  (should (boundp 'emacs-fileio--buffer-files)))

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
      (should (search-forward "(nelisp-ec--split-string-char seed ?/ t)" nil t))
      (goto-char (point-min))
      (should-not (search-forward "(split-string seed" nil t)))))

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

(ert-deftest emacs-fileio-builtins-test/locate-library-finds-el-file ()
  (let* ((dir (emacs-fileio-builtins-test--tmp-path "load-path"))
         (file (expand-file-name "sample-lib.el" dir))
         (subdir (expand-file-name "nested" dir))
         (subfile (expand-file-name "sub-lib.el" subdir)))
    (unwind-protect
        (progn
          (make-directory dir t)
          (make-directory subdir t)
          (with-temp-file file
            (insert "(provide 'sample-lib)\n"))
          (with-temp-file subfile
            (insert "(provide 'sub-lib)\n"))
          (should (equal file
                         (emacs-fileio-locate-library
                          "sample-lib" nil (list dir))))
          (should (equal file
                         (emacs-fileio-locate-library
                          "sample-lib.el" nil (list dir))))
          (should (equal subfile
                         (emacs-fileio-locate-library
                          "nested/sub-lib" nil (list dir))))
          (cl-letf (((symbol-function 'file-readable-p)
                     (lambda (&rest _)
                       (error "locate-library must not call file-readable-p")))
                    ((symbol-function 'file-exists-p)
                     (lambda (&rest _)
                       (error "locate-library must not call file-exists-p"))))
            (should (equal file
                           (emacs-fileio-locate-library
                            "sample-lib" nil (list dir))))))
      (when (file-exists-p subfile)
        (delete-file subfile))
      (when (file-directory-p subdir)
        (delete-directory subdir))
      (when (file-exists-p file)
        (delete-file file))
      (when (file-directory-p dir)
        (delete-directory dir)))))

(ert-deftest emacs-fileio-builtins-test/locate-library-standalone-without-opendir-is-nil ()
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
                    ((symbol-function 'file-readable-p)
                     (lambda (&rest _)
                       (error "standalone fallback must not call file-readable-p")))
                    ((symbol-function 'file-exists-p)
                     (lambda (&rest _)
                       (error "standalone fallback must not call file-exists-p"))))
            (should-not (emacs-fileio-locate-library
                         "sample-lib" nil (list dir)))))
      (when (file-exists-p file)
        (delete-file file))
      (when (file-directory-p dir)
        (delete-directory dir)))))

;;;; B. Substrate-direct: write + read roundtrip

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

(provide 'emacs-fileio-builtins-test)

;;; emacs-fileio-builtins-test.el ends here
