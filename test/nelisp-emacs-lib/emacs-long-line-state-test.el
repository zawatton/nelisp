;;; emacs-long-line-state-test.el --- redisplay-owned long-line state -*- lexical-binding: t; -*-

;;; Code:

(defconst emacs-long-line-state-test--root
  (expand-file-name "../.." (file-name-directory (or load-file-name buffer-file-name))))

(dolist (directory '("packages/nelisp-emacs-foundation/src"
                     "packages/nelisp-emacs-buffer-core/src"
                     "packages/nelisp-emacs-text-core/src"
                     "packages/nelisp-emacs-core/src"
                     "packages/nelisp-regex/src"))
  (add-to-list 'load-path
               (expand-file-name directory emacs-long-line-state-test--root)))

(require 'ert)
(require 'emacs-buffer)
(require 'emacs-window)
(require 'nelisp-emacs-compat)
(require 'emacs-tui-backend)

(defvar long-line-threshold 50000)
(defvar emacs-long-line-state-test--buffer-counter 0)

(defun emacs-long-line-state-test--new-buffer (prefix)
  "Allocate a uniquely named test buffer using PREFIX."
  (setq emacs-long-line-state-test--buffer-counter
        (1+ emacs-long-line-state-test--buffer-counter))
  (nelisp-ec-generate-new-buffer
   (format "%s-%d" prefix emacs-long-line-state-test--buffer-counter)))

(defun emacs-long-line-state-test--load-xdisp-fallback ()
  "Load the xdisp fallback without a host implementation."
  (load (expand-file-name "packages/nelisp-emacs-foundation/src/emacs-cc-xdisp-1.el"
                          emacs-long-line-state-test--root) nil t))

(defun emacs-long-line-state-test--load-renderer (kind)
  "Load the redisplay implementation named KIND."
  (load (expand-file-name
         (if (eq kind 'core)
             "packages/nelisp-emacs-core/src/emacs-redisplay-core.el"
           "packages/nelisp-emacs-core/src/emacs-redisplay.el")
         emacs-long-line-state-test--root) nil t))

(defmacro emacs-long-line-state-test--with-world (&rest body)
  "Run BODY with clean custom buffer and window state."
  (declare (indent 0) (debug (body)))
  `(let ((nelisp-ec--buffers nil)
         (nelisp-ec--current-buffer nil))
     (unwind-protect
         (progn (emacs-window-reset) ,@body)
       (emacs-window-reset))))

(defun emacs-long-line-state-test--render (kind buffer)
  "Render BUFFER once through renderer KIND."
  (emacs-long-line-state-test--load-renderer kind)
  (let ((window (emacs-window-selected-window))
        (handle (emacs-redisplay-init)))
    (emacs-window-set-window-buffer window buffer)
    (emacs-redisplay-redisplay-window handle window)))

(defun emacs-long-line-state-test--value (buffer)
  "Read the state stored for BUFFER, independent of current-buffer ownership."
  (and (nth 2 (cdr (assq 'emacs-cc-xdisp-1--long-line-state
                         (emacs-buffer-buffer-local-variables buffer))))
       t))

(ert-deftest emacs-long-line-state-getter-uses-explicit-owner-fixture ()
  (let ((host-function (and (fboundp 'long-line-optimizations-p)
                            (symbol-function 'long-line-optimizations-p)))
        (owner-function (and (fboundp 'emacs-buffer--property-current-buffer)
                             (symbol-function 'emacs-buffer--property-current-buffer)))
        (owner-was-bound (fboundp 'emacs-buffer--property-current-buffer)))
    (unwind-protect
        (progn
          (when (fboundp 'long-line-optimizations-p)
            (fmakunbound 'long-line-optimizations-p))
          (emacs-long-line-state-test--load-xdisp-fallback)
          (emacs-long-line-state-test--with-world
            (let ((owner (emacs-long-line-state-test--new-buffer "ll-owner"))
                  (compat (emacs-long-line-state-test--new-buffer "ll-compat")))
              (emacs-buffer-set-buffer-local-value
               'emacs-cc-xdisp-1--long-line-state owner '(0 nil t))
              (emacs-buffer-set-buffer-local-value
               'emacs-cc-xdisp-1--long-line-state compat '(0 nil nil))
              ;; This fixture controls owner resolution explicitly.  It does
              ;; not claim that native buffers are wired to the renderer.
              (fset 'emacs-buffer--property-current-buffer (lambda () owner))
              (let ((nelisp-ec--current-buffer compat))
                (should (long-line-optimizations-p)))
              (fset 'emacs-buffer--property-current-buffer (lambda () compat))
              (let ((nelisp-ec--current-buffer owner))
                (should-not (long-line-optimizations-p))))))
      (if host-function (fset 'long-line-optimizations-p host-function)
        (fmakunbound 'long-line-optimizations-p))
      (if owner-was-bound
          (fset 'emacs-buffer--property-current-buffer owner-function)
        (fmakunbound 'emacs-buffer--property-current-buffer)))))

(ert-deftest emacs-long-line-state-core-boundary-and-undisplayed-buffer ()
  (let ((host-function (and (fboundp 'long-line-optimizations-p)
                            (symbol-function 'long-line-optimizations-p))))
    (unwind-protect
        (progn
          (when (fboundp 'long-line-optimizations-p)
            (fmakunbound 'long-line-optimizations-p))
          (emacs-long-line-state-test--load-xdisp-fallback)
          (emacs-long-line-state-test--with-world
            (let ((long-line-threshold 3)
                  (boundary (emacs-long-line-state-test--new-buffer "ll-boundary"))
                  (over (emacs-long-line-state-test--new-buffer "ll-over")))
              (dolist (pair `((,boundary . "abc") (,over . "abcd")))
                (let ((nelisp-ec--current-buffer (car pair)))
                  (nelisp-ec-insert (cdr pair))))
              (should-not (emacs-long-line-state-test--value over))
              (emacs-long-line-state-test--render 'core boundary)
              (should-not (emacs-long-line-state-test--value boundary))
              (should-not (emacs-long-line-state-test--value over))
              (emacs-long-line-state-test--render 'core over)
              (should (emacs-long-line-state-test--value over)))))
      (if host-function (fset 'long-line-optimizations-p host-function)
        (fmakunbound 'long-line-optimizations-p)))))

(ert-deftest emacs-long-line-state-threshold-change-trigger-and-latch ()
  (let ((host-function (and (fboundp 'long-line-optimizations-p)
                            (symbol-function 'long-line-optimizations-p))))
    (unwind-protect
        (progn
          (when (fboundp 'long-line-optimizations-p)
            (fmakunbound 'long-line-optimizations-p))
          (emacs-long-line-state-test--load-xdisp-fallback)
          (emacs-long-line-state-test--with-world
            (let* ((long-line-threshold 3)
                   (buffer (emacs-long-line-state-test--new-buffer "ll-change")))
              (let ((nelisp-ec--current-buffer buffer))
                (nelisp-ec-insert "a"))
              (emacs-long-line-state-test--render 'core buffer)
              (dotimes (_ 8)
                (let ((nelisp-ec--current-buffer buffer))
                  (nelisp-ec-goto-char (1+ (nelisp-ec-buffer-size buffer)))
                  (nelisp-ec-insert "x")))
              (emacs-long-line-state-test--render 'core buffer)
              (should-not (emacs-long-line-state-test--value buffer))
              ;; The GNU trigger measures changes since the prior display
              ;; pass, not the accumulated edits since the initial scan.
              (dotimes (_ 4)
                (let ((nelisp-ec--current-buffer buffer))
                  (nelisp-ec-goto-char (1+ (nelisp-ec-buffer-size buffer)))
                  (nelisp-ec-insert "x")))
              (emacs-long-line-state-test--render 'core buffer)
              (should-not (emacs-long-line-state-test--value buffer))
              (dotimes (_ 5)
                (let ((nelisp-ec--current-buffer buffer))
                  (nelisp-ec-goto-char (1+ (nelisp-ec-buffer-size buffer)))
                  (nelisp-ec-insert "x")))
              (emacs-long-line-state-test--render 'core buffer)
              (should-not (emacs-long-line-state-test--value buffer))
              (dotimes (_ 9)
                (let ((nelisp-ec--current-buffer buffer))
                  (nelisp-ec-goto-char (1+ (nelisp-ec-buffer-size buffer)))
                  (nelisp-ec-insert "x")))
              (emacs-long-line-state-test--render 'core buffer)
              (should (emacs-long-line-state-test--value buffer))
              (let ((nelisp-ec--current-buffer buffer))
                (nelisp-ec-delete-region 1 (1+ (nelisp-ec-buffer-size buffer))))
              (emacs-long-line-state-test--render 'core buffer)
              (should (emacs-long-line-state-test--value buffer)))))
      (if host-function (fset 'long-line-optimizations-p host-function)
        (fmakunbound 'long-line-optimizations-p)))))

(ert-deftest emacs-long-line-state-z-nil-threshold-and-full-renderer ()
  (let ((host-function (and (fboundp 'long-line-optimizations-p)
                            (symbol-function 'long-line-optimizations-p))))
    (unwind-protect
        (progn
          (when (fboundp 'long-line-optimizations-p)
            (fmakunbound 'long-line-optimizations-p))
          (emacs-long-line-state-test--load-xdisp-fallback)
          (emacs-long-line-state-test--with-world
            (let* ((long-line-threshold nil)
                   (buffer (emacs-long-line-state-test--new-buffer "ll-disabled")))
              (let ((nelisp-ec--current-buffer buffer))
                (nelisp-ec-insert (make-string 40 ?x)))
              (emacs-long-line-state-test--render 'full buffer)
              (should-not (emacs-long-line-state-test--value buffer))
              (setq long-line-threshold 3)
              (dotimes (_ 9)
                (let ((nelisp-ec--current-buffer buffer))
                  (nelisp-ec-goto-char (1+ (nelisp-ec-buffer-size buffer)))
                  (nelisp-ec-insert "y")))
              (emacs-long-line-state-test--render 'full buffer)
              (should (emacs-long-line-state-test--value buffer)))))
      (if host-function (fset 'long-line-optimizations-p host-function)
        (fmakunbound 'long-line-optimizations-p)))))

(provide 'emacs-long-line-state-test)
;;; emacs-long-line-state-test.el ends here
