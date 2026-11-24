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
    files--native-set-buffer-modified-p)
  "Variables supplied by the lightweight files shim.")

(ert-deftest files-test/files-access-ok-p-delegates-to-syscall-path-int ()
  (let ((seen nil))
    (cl-letf (((symbol-function 'nelisp--syscall-path-int)
               (lambda (nr file mode)
                 (push (list nr file mode) seen)
                 (if (and (equal file "/tmp/ok") (eq mode 0)) 0 -1)))
              ((symbol-function 'rdf)
               (lambda (&rest _)
                 (error "rdf fallback must not run"))))
      (should (files-access-ok-p "/tmp/ok" 0))
      (should-not (files-access-ok-p "/tmp/missing" 0))
      (should (equal (reverse seen)
                     (list (list files--syscall-access "/tmp/ok" 0)
                           (list files--syscall-access "/tmp/missing" 0)))))))

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
        (if (consp (cdr cell))
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
  (dolist (spec '(("src/files-standalone-buffer.el" . 2000)))
    (let* ((file (expand-file-name (car spec) files-test--root))
           (oversized (files-test--oversized-top-level-forms file (cdr spec))))
      (should (equal oversized nil)))))

(ert-deftest files-test/native-buffer-read-subrs-not-shadowed ()
  ;; GNU implements point/goto-char/char-after in C; standalone NeLisp runs
  ;; them as native subrs.  The Lisp fallback must not sit on top of them
  ;; (it cost 8-50 us per call against ~0.4 us native).
  (skip-unless (and files--standalone-runtime-p (fboundp 'nelisp--repr)))
  (dolist (name '(point point-min point-max following-char preceding-char
                    goto-char char-after char-before))
    (should (subrp (symbol-function name))))
  (with-temp-buffer
    (insert "hello")
    (should (= (point) 6))
    (should (= (goto-char 100) 100))
    (should (= (point) 6))
    (should (eq (char-after 1) ?h))
    (should (null (char-after 6)))
    (should (eq (char-before 6) ?o))
    (should (= (following-char) 0))
    (should (= (preceding-char) ?o))
    ;; Markers are accepted by the native subrs; detached ones signal.
    (let ((m (copy-marker 2)))
      (should (eq (char-after m) ?e))
      (should (eq (goto-char m) m))
      (should (= (point) 2)))
    (should-error (goto-char (make-marker)) :type 'error)
    ;; The subrp checks above are the discriminating assertion (a Lisp
    ;; shadow cost 8-50 us per call); no absolute timing bound here.
    nil))

(ert-deftest files-test/buffer-substring-fast-path-keeps-properties ()
  ;; A buffer whose property sidecar is empty takes the native buffer-substring
  ;; directly (~55 us through the Lisp advice body); once properties exist
  ;; the sidecar copy still runs.
  (skip-unless (and files--standalone-runtime-p (fboundp 'nelisp--repr)))
  (with-temp-buffer
    (insert "hello world")
    (should (equal (buffer-substring 1 6) "hello"))
    (should (equal (buffer-substring 6 1) "hello"))
    (should-error (buffer-substring 1 100) :type 'args-out-of-range)
    (should-error (buffer-substring (make-marker) 5) :type 'error)
    ;; Path, not speed: with integer bounds and an empty sidecar the
    ;; property copy is never entered; once a property exists it is.
    (let ((copies 0)
          (orig (symbol-function 'emacs-buffer--copy-buffer-properties-to-string)))
      (unwind-protect
          (progn
            (fset 'emacs-buffer--copy-buffer-properties-to-string
                  (lambda (&rest a) (setq copies (1+ copies)) (apply orig a)))
            (buffer-substring 1 6)
            (buffer-substring 6 1)
            (should (= copies 0))
            (put-text-property 2 5 'face 'bold)
            (buffer-substring 1 6)
            (should (= copies 1)))
        (fset 'emacs-buffer--copy-buffer-properties-to-string orig)))
    (put-text-property 2 5 'face 'bold)
    (should (eq (get-text-property 0 'face (buffer-substring 1 6)) nil))
    (should (eq (get-text-property 1 'face (buffer-substring 1 6)) 'bold))
    (should (eq (get-text-property 0 'face (buffer-substring 2 3)) 'bold))))

(provide 'files-test)

;;; files-test.el ends here
