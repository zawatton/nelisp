;;; files-test.el --- ERT for lightweight files.el shim  -*- lexical-binding: t; -*-

;;; Commentary:

;; Host Emacs normally loads GNU files.el early.  These tests load the
;; repository shim source directly and temporarily open only the command
;; gates owned by the shim.

;;; Code:

(require 'ert)
(require 'cl-lib)
;; Ensure the standalone buffer shim is loaded before any fixture captures
;; `files-test--shim-functions' originals.  Several fixtures capture the current
;; function cells and restore them in cleanup; if `files-standalone-buffer' is
;; only lazily loaded *inside* a fixture body, the captured originals are nil
;; and cleanup `fmakunbound's shim functions (e.g. `files--expand-file-name'),
;; leaving every later find-file/save-buffer/org/project test with a void
;; `files--expand-file-name'.  Loading it here keeps the captured originals bound.
(require 'files-standalone-buffer)

(defconst files-test--root
  (file-name-directory
   (directory-file-name
    (file-name-directory (or load-file-name buffer-file-name default-directory))))
  "Repository root used to load the shim source directly.")

(defconst files-test--shim-functions
  '(make-sparse-keymap
    keymapp
    define-key
    lookup-key
    buffer-file-name
    set-visited-file-name
    find-file
    find-file-noselect
    find-file-read-only
    find-alternate-file
    find-file-other-window
    find-file-other-frame
    save-buffer
    save-some-buffers
    write-file
    insert-file
    list-directory
    file-exists-p
    file-readable-p
    file-writable-p
    file-executable-p
    file-directory-p
    make-directory
    delete-file
    delete-directory
    rename-file
    add-name-to-file
    make-symbolic-link
    set-file-modes
    files--standalone-runtime-p
    files--ensure-buffer-substrate
    files--call
    files--wrap
    files--install
    files--lazy-wrapper
    files--install-nullary
    files--lazy-nullary-wrapper
    files--install-lazy
    files--install-lazy-nullary
    files--buffer-key
    files--buffer-file-cell
    files--buffer-state-cell
    files--host-buffer-available-p
    files--with-host-buffer
    files--buffer-live-or-unknown-p
    files--live-buffer-cells
    files--prune-dead-buffer-state
    files--set-buffer-state-cell
    files--buffer-string-value
    files--set-buffer-string-value
    files--buffer-point-value
    files--set-buffer-point-value
    files--buffer-modified-value
    files--set-buffer-modified-value
    files--buffer-file-name
    files--set-buffer-file-name
    files--set-visited-file-name
    files--file-name-equal-p
    files--visited-buffer-for-file
    files--file-buffer-name
    files--create-buffer-for-file
    files--expand-file-name
    files--buffer-for-file
    files--string-length
    files--concat-strings
    files--clip-point
    files--buffer-substring
    files--install-fallback-function-p
    files--fallback-insert-strings
    files--read-file-text
    files--region-text
    files--write-file-text
    files--current-buffer-if-available
    files--set-buffer-if-available
    files--file-readable-or-unknown-p
    files--insert-file-if-readable
    files--load-file-into-buffer
    files--native-access-ok-p
    point-min
    point-max
    point
    goto-char
    buffer-string
    erase-buffer
    insert
    buffer-modified-p
    set-buffer-modified-p
    insert-file-contents
    write-region
    directory-files
    files-standalone-find-file
    files-standalone-find-file-noselect
    files-standalone-find-file-read-only
    files-standalone-find-alternate-file
    files-standalone-save-buffer
    files--save-current-buffer-if-needed
    files--buffer-modified-for-save-p
    files--save-buffer-entry-if-needed
    files--save-buffer-entries-if-needed
    files-standalone-write-file
    files-standalone-save-some-buffers
    files-standalone-insert-file
    files-standalone-list-directory)
  "Functions supplied by the lightweight files shim.")

(defconst files-test--preload-unbind-functions
  '(make-sparse-keymap
    keymapp
    define-key
    lookup-key
    buffer-file-name
    set-visited-file-name
    find-file
    find-file-noselect
    find-file-read-only
    find-alternate-file
    find-file-other-window
    find-file-other-frame
    save-buffer
    save-some-buffers
    write-file
    insert-file
    list-directory
    files--ensure-buffer-substrate)
  "Functions safe to unbind before loading src/files.el.")

(defconst files-test--shim-variables
  '(ctl-x-map
    ctl-x-4-map
    ctl-x-5-map
    files--standalone-p
    files--current-file-name
    files--buffer-file-names
    files--buffer-string
    files--buffer-strings
    files--point
    files--buffer-points
    files--buffer-modified-p
    files--buffer-modified-flags
    files--standalone-runtime-p
    files--native-write-region
    files--native-insert-file-contents
    files--native-file-exists-p
    files--native-file-readable-p
    files--native-file-writable-p
    files--native-file-executable-p
    files--native-delete-file
    files--native-buffer-string
    files--native-erase-buffer
    files--native-insert
    files--native-point-min
    files--native-point-max
    files--native-point
    files--native-goto-char
    files--native-buffer-modified-p
    files--native-set-buffer-modified-p
    buffer-read-only)
  "Variables supplied by the lightweight files shim.")

(defmacro files-test--with-shim-functions (&rest body)
  "Load the lightweight files shim with its command gates open."
  (declare (indent 0) (debug (body)))
  `(let ((originals
          (mapcar (lambda (symbol)
                    (cons symbol
                          (and (fboundp symbol)
                               (symbol-function symbol))))
                  files-test--shim-functions))
         (original-values
          (mapcar (lambda (symbol)
                    (cons symbol
                          (and (boundp symbol)
                               (symbol-value symbol))))
                  files-test--shim-variables))
         (original-features features)
         (original-boundp (symbol-function 'boundp))
         (native-comp-enable-subr-trampolines nil))
     (unwind-protect
         (cl-letf (((symbol-function 'boundp)
                    (lambda (symbol)
                      (if (eq symbol 'emacs-version)
                          nil
                        (funcall original-boundp symbol)))))
           (dolist (symbol files-test--preload-unbind-functions)
             (fmakunbound symbol))
           (dolist (symbol files-test--shim-variables)
             (makunbound symbol))
           (setq features (remove 'files (remove 'files-standalone-buffer
                                                 features)))
           (load (expand-file-name "src/files.el" files-test--root)
                 nil t)
           ,@body)
       (setq features original-features)
       (dolist (cell originals)
         (if (cdr cell)
             (fset (car cell) (cdr cell))
           (fmakunbound (car cell))))
       (dolist (cell original-values)
         (if (cdr cell)
             (set (car cell) (cdr cell))
           (makunbound (car cell)))))))

(ert-deftest files-test/provides-daily-driver-surface ()
  (files-test--with-shim-functions
    (should (featurep 'files))
    (dolist (symbol '(find-file find-file-read-only find-alternate-file
                                save-buffer save-some-buffers write-file
                                insert-file list-directory
                                find-file-other-window find-file-other-frame))
      (should (fboundp symbol)))))

(ert-deftest files-test/standalone-detected-when-nelisp-write-primitive-exists ()
  (let ((originals
         (mapcar (lambda (symbol)
                   (cons symbol
                         (and (fboundp symbol)
                              (symbol-function symbol))))
                 files-test--shim-functions))
        (original-values
         (mapcar (lambda (symbol)
                   (cons symbol
                         (and (boundp symbol)
                              (symbol-value symbol))))
                 files-test--shim-variables))
        (original-features features)
        (original-nl-write-file (and (fboundp 'nl-write-file)
                                     (symbol-function 'nl-write-file)))
        (native-comp-enable-subr-trampolines nil))
    (unwind-protect
        (progn
          (dolist (symbol files-test--preload-unbind-functions)
            (fmakunbound symbol))
          (dolist (symbol files-test--shim-variables)
            (makunbound symbol))
          (fset 'nl-write-file (lambda (&rest _args) nil))
          (let ((emacs-version "30.0"))
            (setq features (remove 'files (remove 'files-standalone-buffer
                                                  features)))
            (load (expand-file-name "src/files.el" files-test--root)
                  nil t))
          (should files--standalone-p)
          (should (fboundp 'find-file))
          (should (fboundp 'save-buffer))
          (should (fboundp 'write-file)))
      (setq features original-features)
      (dolist (cell originals)
        (if (cdr cell)
            (fset (car cell) (cdr cell))
          (fmakunbound (car cell))))
      (dolist (cell original-values)
        (if (cdr cell)
            (set (car cell) (cdr cell))
          (makunbound (car cell))))
      (if original-nl-write-file
          (fset 'nl-write-file original-nl-write-file)
        (when (fboundp 'nl-write-file)
          (fmakunbound 'nl-write-file))))))

(ert-deftest files-test/wrappers-avoid-lexical-closure-capture ()
  (files-test--with-shim-functions
    (should (equal (symbol-function 'find-file)
                   '(lambda (&rest args)
                      (require 'files-standalone-buffer)
                      (apply 'files-standalone-find-file args))))
    (should (equal (symbol-function 'save-buffer)
                   '(lambda (&rest _args)
                      (require 'files-standalone-buffer)
                      (funcall 'files-standalone-save-buffer))))))

(ert-deftest files-test/installs-standard-c-x-file-bindings ()
  (files-test--with-shim-functions
    (should (eq (lookup-key ctl-x-map "\C-f") 'find-file))
    (should (eq (lookup-key ctl-x-map "\C-r") 'find-file-read-only))
    (should (eq (lookup-key ctl-x-map "\C-v") 'find-alternate-file))
    (should (eq (lookup-key ctl-x-map "\C-s") 'save-buffer))
    (should (eq (lookup-key ctl-x-map "\C-w") 'write-file))
    (should (eq (lookup-key ctl-x-map "i") 'insert-file))
    (should (eq (lookup-key ctl-x-4-map "f") 'find-file-other-window))
    (should (eq (lookup-key ctl-x-5-map "f") 'find-file-other-frame))))

(ert-deftest files-test/insert-file-inserts-file-contents ()
  (files-test--with-shim-functions
    (let ((file (make-temp-file "nelisp-emacs-files-test-")))
      (unwind-protect
          (progn
            (with-temp-file file
              (insert "alpha"))
            (with-temp-buffer
              (cl-letf (((symbol-function 'nelisp--syscall-read-file)
                         (lambda (_filename) "alpha")))
                (insert-file file))
              (should (equal (buffer-string) "alpha"))))
        (delete-file file)))))

(ert-deftest files-test/find-file-read-only-uses-standalone-command ()
  (files-test--with-shim-functions
    (let ((file (make-temp-file "nelisp-emacs-files-test-"))
          (host-delete-file (symbol-function 'delete-file)))
      (unwind-protect
          (progn
            (with-temp-file file
              (insert "alpha"))
            (setq buffer-read-only nil)
            (cl-letf (((symbol-function 'nelisp--syscall-path-int)
                       (lambda (&rest _args) 0))
                      ((symbol-function 'nelisp--syscall-read-file)
                       (lambda (_filename) "alpha")))
              (find-file-read-only file))
            (should buffer-read-only)
            (should (equal (buffer-string) "alpha")))
        (funcall host-delete-file file)))))

(ert-deftest files-test/find-file-reuses-existing-buffer-without-reload ()
  (files-test--with-shim-functions
    (let ((file (make-temp-file "nelisp-emacs-files-test-"))
          buffer)
      (unwind-protect
          (progn
            (with-temp-file file
              (insert "alpha"))
            (setq buffer (find-file file))
            (should (equal (buffer-string) "alpha"))
            (should (eq (find-file file) buffer))
            (should (equal (buffer-string) "alpha"))
            (erase-buffer)
            (insert "local edit")
            (should (buffer-modified-p))
            (should (eq (find-file file) buffer))
            (should (equal (buffer-string) "local edit"))
            (should (buffer-modified-p)))
        (let ((buffer (get-buffer (file-name-nondirectory file))))
          (when buffer
            (kill-buffer buffer)))
        (delete-file file)))))

(ert-deftest files-test/find-file-then-save-buffer-writes-visited-file ()
  (files-test--with-shim-functions
    (let ((file (make-temp-file "nelisp-emacs-files-test-"))
          written)
      (unwind-protect
          (progn
            (with-temp-file file
              (insert "alpha"))
            (find-file file)
            (should (equal (buffer-string) "alpha"))
            (insert " beta")
            (cl-letf (((symbol-function 'nl-write-file)
                       (lambda (filename text)
                         (setq written (cons filename text))
                         t)))
              (should (equal (save-buffer) file)))
            (should (equal written (cons file "alpha beta")))
            (should-not (buffer-modified-p)))
        (let ((buffer (get-buffer (file-name-nondirectory file))))
          (when buffer
            (kill-buffer buffer)))
        (delete-file file)))))

(ert-deftest files-test/save-buffer-tracks-visited-file-per-buffer ()
  (files-test--with-shim-functions
    (let ((file-a (make-temp-file "nelisp-emacs-files-a-"))
          (file-b (make-temp-file "nelisp-emacs-files-b-"))
          buffer-a
          buffer-b
          writes)
      (unwind-protect
          (progn
            (with-temp-file file-a (insert "alpha"))
            (with-temp-file file-b (insert "bravo"))
            (setq buffer-a (find-file file-a))
            (insert " edited-a")
            (setq buffer-b (find-file file-b))
            (insert " edited-b")
            (cl-letf (((symbol-function 'nl-write-file)
                       (lambda (filename text)
                         (push (cons filename text) writes)
                         t)))
              (set-buffer buffer-a)
              (should (equal (buffer-file-name) file-a))
              (should (equal (save-buffer) file-a))
              (set-buffer buffer-b)
              (should (equal (buffer-file-name) file-b))
              (should (equal (save-buffer) file-b)))
            (should (member (cons file-a "alpha edited-a") writes))
            (should (member (cons file-b "bravo edited-b") writes)))
        (dolist (buffer (list buffer-a buffer-b))
          (when (and buffer (bufferp buffer) (buffer-live-p buffer))
            (kill-buffer buffer)))
        (dolist (file (list file-a file-b))
          (when (file-exists-p file)
            (delete-file file)))))))

(ert-deftest files-test/find-file-separates-same-basename-different-dirs ()
  (files-test--with-shim-functions
    (let* ((dir-a (make-temp-file "nelisp-emacs-files-a-" t))
           (dir-b (make-temp-file "nelisp-emacs-files-b-" t))
           (file-a (expand-file-name "same.el" dir-a))
           (file-b (expand-file-name "same.el" dir-b))
           buffer-a
           buffer-b
           writes)
      (unwind-protect
          (progn
            (write-region "alpha" nil file-a)
            (write-region "bravo" nil file-b)
            (setq buffer-a (find-file file-a))
            (insert " edited-a")
            (setq buffer-b (find-file file-b))
            (should (buffer-live-p buffer-a))
            (should (buffer-live-p buffer-b))
            (should-not (eq buffer-a buffer-b))
            (should (equal (buffer-file-name buffer-a) file-a))
            (should (equal (buffer-file-name buffer-b) file-b))
            (should (equal (buffer-string) "bravo"))
            (insert " edited-b")
            (cl-letf (((symbol-function 'nl-write-file)
                       (lambda (filename text)
                         (push (cons filename text) writes)
                         t)))
              (set-buffer buffer-a)
              (should (equal (save-buffer) file-a))
              (set-buffer buffer-b)
              (should (equal (save-buffer) file-b)))
            (should (member (cons file-a "alpha edited-a") writes))
            (should (member (cons file-b "bravo edited-b") writes)))
        (dolist (buffer (list buffer-a buffer-b))
          (when (and buffer (bufferp buffer) (buffer-live-p buffer))
            (kill-buffer buffer)))
        (dolist (dir (list dir-a dir-b))
          (when (file-directory-p dir)
            (delete-directory dir t)))))))

(ert-deftest files-test/buffer-file-name-is-nil-for-non-file-buffer ()
  (files-test--with-shim-functions
    (let ((file (make-temp-file "nelisp-emacs-files-test-"))
          file-buffer
          scratch-buffer)
      (unwind-protect
          (progn
            (write-region "alpha" nil file)
            (setq file-buffer (find-file file))
            (should (equal (buffer-file-name) file))
            (setq scratch-buffer (generate-new-buffer "scratch"))
            (set-buffer scratch-buffer)
            (should-not (buffer-file-name))
            (should-not (buffer-file-name scratch-buffer))
            (should (equal (buffer-file-name file-buffer) file)))
        (dolist (buffer (list file-buffer scratch-buffer))
          (when (and buffer (bufferp buffer) (buffer-live-p buffer))
            (kill-buffer buffer)))
        (when (file-exists-p file)
          (delete-file file))))))

(ert-deftest files-test/save-some-buffers-saves-only-when-modified ()
  (files-test--with-shim-functions
    ;; Exercise the standalone fallback modified-tracking (what this shim
    ;; simulates): force the captured native `buffer-modified-p' off so the
    ;; directly-set fallback flag `files--buffer-modified-p' is authoritative.
    (let (written
          (files--native-buffer-string nil)
          (files--native-buffer-modified-p nil)
          (files--native-set-buffer-modified-p nil))
      (set-visited-file-name "/tmp/nelisp-emacs-files-test-save.txt")
      (setq files--point 6)
      ;; Set the per-buffer fallback content + modified flag through the
      ;; proper setters, so the registered-buffer save path (which reads
      ;; per-buffer state) sees them under both interpreted and byte-compiled
      ;; loads -- not just the single-buffer global `files--buffer-string'.
      (files--set-buffer-string-value "alpha")
      (files--set-buffer-modified-value nil)
      (cl-letf (((symbol-function 'nl-write-file)
                 (lambda (filename text)
                   (setq written (cons filename text))
                   t)))
        (should-not (save-some-buffers))
        (should-not written)
        (files--set-buffer-modified-value t)
        (should (save-some-buffers))
        (should (equal written
                       '("/tmp/nelisp-emacs-files-test-save.txt" . "alpha")))
        (should-not (buffer-modified-p))))))

(ert-deftest files-test/save-some-buffers-saves-all-modified-file-buffers ()
  (files-test--with-shim-functions
    (let ((file-a (make-temp-file "nelisp-emacs-files-a-"))
          (file-b (make-temp-file "nelisp-emacs-files-b-"))
          (file-c (make-temp-file "nelisp-emacs-files-c-"))
          buffer-a
          buffer-b
          buffer-c
          writes)
      (unwind-protect
          (progn
            (with-temp-file file-a (insert "alpha"))
            (with-temp-file file-b (insert "bravo"))
            (with-temp-file file-c (insert "charlie"))
            (setq buffer-a (find-file file-a))
            (insert " edited-a")
            (setq buffer-b (find-file file-b))
            (setq buffer-c (find-file file-c))
            (insert " edited-c")
            (cl-letf (((symbol-function 'nl-write-file)
                       (lambda (filename text)
                         (push (cons filename text) writes)
                         t)))
              (set-buffer buffer-b)
              (should (save-some-buffers))
              (should (eq (current-buffer) buffer-b)))
            (should (member (cons file-a "alpha edited-a") writes))
            (should (member (cons file-c "charlie edited-c") writes))
            (should-not (assoc file-b writes))
            (set-buffer buffer-a)
            (should-not (buffer-modified-p))
            (set-buffer buffer-c)
            (should-not (buffer-modified-p)))
        (dolist (buffer (list buffer-a buffer-b buffer-c))
          (when (and buffer (bufferp buffer) (buffer-live-p buffer))
            (kill-buffer buffer)))
        (dolist (file (list file-a file-b file-c))
          (when (file-exists-p file)
            (delete-file file)))))))

(ert-deftest files-test/save-some-buffers-prunes-killed-file-buffers ()
  (files-test--with-shim-functions
    (let ((file-a (make-temp-file "nelisp-emacs-files-a-"))
          (file-b (make-temp-file "nelisp-emacs-files-b-"))
          buffer-a
          buffer-b
          writes)
      (unwind-protect
          (progn
            (with-temp-file file-a (insert "alpha"))
            (with-temp-file file-b (insert "bravo"))
            (setq buffer-a (find-file file-a))
            (insert " edited-a")
            (setq buffer-b (find-file file-b))
            (insert " edited-b")
            (kill-buffer buffer-a)
            (cl-letf (((symbol-function 'nl-write-file)
                       (lambda (filename text)
                         (push (cons filename text) writes)
                         t)))
              (set-buffer buffer-b)
              (should (save-some-buffers))
              (should (eq (current-buffer) buffer-b)))
            (should-not (assoc file-a writes))
            (should (member (cons file-b "bravo edited-b") writes))
            (should-not (assq buffer-a files--buffer-file-names))
            (should-not (assq buffer-a files--buffer-strings))
            (should-not (assq buffer-a files--buffer-points))
            (should-not (assq buffer-a files--buffer-modified-flags)))
        (dolist (buffer (list buffer-a buffer-b))
          (when (and buffer (bufferp buffer) (buffer-live-p buffer))
            (kill-buffer buffer)))
        (dolist (file (list file-a file-b))
          (when (file-exists-p file)
            (delete-file file)))))))

(ert-deftest files-test/reopen-after-kill-uses-new-buffer-state ()
  (files-test--with-shim-functions
    (let ((file (make-temp-file "nelisp-emacs-files-reopen-"))
          first-buffer
          second-buffer
          written)
      (unwind-protect
          (progn
            (write-region "alpha" nil file)
            (setq first-buffer (find-file file))
            (insert " stale")
            (kill-buffer first-buffer)
            (should-not (buffer-file-name first-buffer))
            (write-region "fresh" nil file)
            (setq second-buffer (find-file file))
            (should (buffer-live-p second-buffer))
            (should-not (eq first-buffer second-buffer))
            (should (equal (buffer-string) "fresh"))
            (insert " edit")
            (cl-letf (((symbol-function 'nl-write-file)
                       (lambda (filename text)
                         (setq written (cons filename text))
                         t)))
              (should (equal (save-buffer) file)))
            (should (equal written (cons file "fresh edit")))
            (should-not (assq first-buffer files--buffer-file-names)))
        (dolist (buffer (list first-buffer second-buffer))
          (when (and buffer (bufferp buffer) (buffer-live-p buffer))
            (kill-buffer buffer)))
        (when (file-exists-p file)
          (delete-file file))))))

(ert-deftest files-test/list-directory-returns-directory-names ()
  (files-test--with-shim-functions
    (let ((dir (make-temp-file "nelisp-emacs-files-test-" t)))
      (unwind-protect
          (progn
            (with-temp-file (expand-file-name "alpha.txt" dir)
              (insert "alpha"))
            (should (member "alpha.txt" (list-directory dir))))
        (delete-directory dir t)))))

(ert-deftest files-test/list-directory-can-use-standalone-readdir ()
  (files-test--with-shim-functions
    (fmakunbound 'directory-files)
    (cl-letf (((symbol-function 'nelisp--syscall-readdir)
               (lambda (_dirname) (cons t '("." ".." "alpha.txt")))))
      (should (equal (list-directory "/tmp")
                     '("." ".." "alpha.txt"))))))

(ert-deftest files-test/source-does-not-force-full-bootstrap ()
  (with-temp-buffer
    (insert-file-contents (expand-file-name "src/files.el" files-test--root))
    (should-not (search-forward "(require 'emacs-init)" nil t))))

(ert-deftest files-test/standalone-buffer-source-preserves-host-primitives ()
  "Loading the fallback substrate in host Emacs must not replace subrs."
  (let* ((primitive-symbols '(point-min
                              point-max
                              point
                              goto-char
                              buffer-string
                              erase-buffer
                              insert
                              buffer-modified-p
                              set-buffer-modified-p
                              insert-file-contents
                              write-region))
         (primitive-cells
          (mapcar (lambda (symbol)
                    (cons symbol (symbol-function symbol)))
                  primitive-symbols))
         (function-cells
          (mapcar (lambda (symbol)
                    (cons symbol
                          (and (fboundp symbol)
                               (symbol-function symbol))))
                  files-test--shim-functions))
         (value-cells
          (mapcar (lambda (symbol)
                    (cons symbol
                          (and (boundp symbol)
                               (list (symbol-value symbol)))))
                  files-test--shim-variables))
         (original-features features)
         (native-comp-enable-subr-trampolines nil))
    (unwind-protect
        (progn
          (setq features (remove 'files-standalone-buffer features))
          (load (expand-file-name "src/files-standalone-buffer.el"
                                  files-test--root)
                nil t)
          (dolist (cell primitive-cells)
            (should (eq (symbol-function (car cell)) (cdr cell)))))
      (setq features original-features)
      (dolist (cell function-cells)
        (if (cdr cell)
            (fset (car cell) (cdr cell))
          (fmakunbound (car cell))))
      (dolist (cell value-cells)
        (if (cdr cell)
            (set (car cell) (cadr cell))
          (makunbound (car cell)))))))

(defun files-test--oversized-top-level-forms (file max-span)
  "Return top-level forms in FILE whose source span exceeds MAX-SPAN."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (let ((index 0)
          oversized)
      (condition-case err
          (while t
            (let ((start (point)))
              (read (current-buffer))
              (setq index (1+ index))
              (let ((span (- (point) start)))
                (when (> span max-span)
                  (push (list index span) oversized)))))
        (end-of-file nil)
        (error (signal (car err) (cdr err))))
      (nreverse oversized))))

(ert-deftest files-test/lightweight-sources-keep-small-top-level-forms ()
  "Daily-driver shims should stay friendly to NeLisp cold source loading.
The editing shims (files.el, simple.el) stay tight at 650 chars/form.
files-standalone-buffer.el carries the OS syscall layer (stat/statx/lstat
buffer parsing) whose individual forms are inherently larger; they already
cold-load in the standalone runtime image (which `require's the file at image
build), so the syscall file gets a looser ceiling that still guards against
runaway forms rather than the editing-shim limit."
  (dolist (spec '(("src/files.el" . 650)
                  ("src/simple.el" . 650)
                  ("src/files-standalone-buffer.el" . 2000)))
    (let* ((file (expand-file-name (car spec) files-test--root))
           (oversized (files-test--oversized-top-level-forms file (cdr spec))))
      (should (equal oversized nil)))))

(provide 'files-test)

;;; files-test.el ends here
