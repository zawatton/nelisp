;;; emacs-easy-mmode-test.el --- tests for standalone easy-mmode fallback  -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)

(defconst emacs-easy-mmode-test--source
  (expand-file-name
   "../src/emacs-easy-mmode.el"
   (file-name-directory (or load-file-name buffer-file-name))))

(defvar easy-test-count 0)
(defvar easy-global-turn-on-count 0)
(defvar easy-global-body-count 0)
(defvar easy-variable-setter-arg nil)

(defmacro emacs-easy-mmode-test--with-standalone-fallback (&rest body)
  "Load `emacs-easy-mmode' as if running under standalone NeLisp."
  (declare (indent 0) (debug (body)))
  `(let ((--emacs-version-bound-- (boundp 'emacs-version))
         (--emacs-version-value-- (and (boundp 'emacs-version)
                                       emacs-version))
         (--standalone-bound-- (boundp 'emacs-easy-mmode--standalone-p))
         (--standalone-value-- (and (boundp 'emacs-easy-mmode--standalone-p)
                                    emacs-easy-mmode--standalone-p))
         (--define-minor-mode-- (and (fboundp 'define-minor-mode)
                                     (symbol-function 'define-minor-mode)))
         (--define-globalized-minor-mode--
          (and (fboundp 'define-globalized-minor-mode)
               (symbol-function 'define-globalized-minor-mode)))
         (--define-global-minor-mode--
          (and (fboundp 'define-global-minor-mode)
               (symbol-function 'define-global-minor-mode))))
     (unwind-protect
         (progn
           (when --emacs-version-bound--
             (makunbound 'emacs-version))
           (when --standalone-bound--
             (makunbound 'emacs-easy-mmode--standalone-p))
           (load emacs-easy-mmode-test--source nil t)
           ,@body)
       (if --emacs-version-bound--
           (setq emacs-version --emacs-version-value--)
         (makunbound 'emacs-version))
       (if --standalone-bound--
           (setq emacs-easy-mmode--standalone-p --standalone-value--)
         (makunbound 'emacs-easy-mmode--standalone-p))
       (if --define-minor-mode--
           (fset 'define-minor-mode --define-minor-mode--)
         (fmakunbound 'define-minor-mode))
       (if --define-globalized-minor-mode--
           (fset 'define-globalized-minor-mode
                 --define-globalized-minor-mode--)
         (fmakunbound 'define-globalized-minor-mode))
       (if --define-global-minor-mode--
           (fset 'define-global-minor-mode --define-global-minor-mode--)
         (fmakunbound 'define-global-minor-mode)))))

(ert-deftest emacs-easy-mmode-test/define-minor-mode-materializes-surfaces ()
  (emacs-easy-mmode-test--with-standalone-fallback
    (let ((minor-mode-alist nil)
          (minor-mode-map-alist nil)
          (global-minor-modes nil)
          (easy-test-count 0))
      (unwind-protect
          (progn
            (should
             (eq (eval
                  '(define-minor-mode easy-test-mode
                     "Standalone minor mode test."
                     :init-value nil
                     :lighter " Ez"
                     :keymap 'easy-test-map
                     (setq easy-test-count (1+ easy-test-count))))
                 'easy-test-mode))
            (should (boundp 'easy-test-mode))
            (should (fboundp 'easy-test-mode))
            (should (commandp 'easy-test-mode))
            (should (equal (interactive-form 'easy-test-mode)
                           '(interactive "P")))
            (should (equal (get 'easy-test-mode 'interactive-form)
                           '(interactive "P")))
            (should (equal (assq 'easy-test-mode minor-mode-alist)
                           '(easy-test-mode " Ez")))
            (should (equal (assq 'easy-test-mode minor-mode-map-alist)
                           '(easy-test-mode . easy-test-map)))
            (should-not (memq 'easy-test-mode global-minor-modes))
            (should (eq (easy-test-mode 1) t))
            (should (= easy-test-count 1))
            (should (eq (easy-test-mode 1) t))
            (should (= easy-test-count 2))
            (should-not (easy-test-mode -1)))
        (when (boundp 'easy-test-mode)
          (makunbound 'easy-test-mode))
        (when (fboundp 'easy-test-mode)
          (fmakunbound 'easy-test-mode))))))

(ert-deftest emacs-easy-mmode-test/define-globalized-minor-mode-materializes-surfaces ()
  (emacs-easy-mmode-test--with-standalone-fallback
    (let ((minor-mode-alist nil)
          (minor-mode-map-alist nil)
          (global-minor-modes nil)
          (easy-global-turn-on-count 0)
          (easy-global-body-count 0))
      (unwind-protect
          (progn
            (defun easy-global-turn-on ()
              (setq easy-global-turn-on-count
                    (1+ easy-global-turn-on-count)))
            (should
             (eq (eval
                  '(define-globalized-minor-mode easy-global-mode
                     easy-test-mode easy-global-turn-on
                     :init-value nil
                     :lighter " GE"
                     (setq easy-global-body-count
                           (1+ easy-global-body-count))))
                 'easy-global-mode))
            (should (boundp 'easy-global-mode))
            (should (fboundp 'easy-global-mode))
            (should (commandp 'easy-global-mode))
            (should (equal (interactive-form 'easy-global-mode)
                           '(interactive "P")))
            (should (equal (get 'easy-global-mode 'interactive-form)
                           '(interactive "P")))
            (should (equal (assq 'easy-global-mode minor-mode-alist)
                           '(easy-global-mode " GE")))
            (should (eq (easy-global-mode 1) t))
            (should (memq 'easy-global-mode global-minor-modes))
            (should (= easy-global-turn-on-count 1))
            (should (= easy-global-body-count 1))
            (should (eq (easy-global-mode 1) t))
            (should (= easy-global-turn-on-count 2))
            (should-not (easy-global-mode -1))
            (should-not (memq 'easy-global-mode global-minor-modes)))
        (when (boundp 'easy-global-mode)
          (makunbound 'easy-global-mode))
        (when (fboundp 'easy-global-mode)
          (fmakunbound 'easy-global-mode))
        (when (fboundp 'easy-global-turn-on)
          (fmakunbound 'easy-global-turn-on))))))

(ert-deftest emacs-easy-mmode-test/define-minor-mode-honours-variable-setter ()
  (emacs-easy-mmode-test--with-standalone-fallback
    (let ((minor-mode-alist nil)
          (minor-mode-map-alist nil)
          (global-minor-modes nil)
          (easy-variable-setter-arg nil))
      (unwind-protect
          (progn
            (defun easy-variable-setter (mode)
              (setq easy-variable-setter-arg mode)
              (setq easy-variable-mode
                    (if mode 'custom-on nil)))
            (should
             (eq (eval
                  '(define-minor-mode easy-variable-mode
                     "Standalone :variable setter test."
                     :init-value nil
                     :variable (easy-variable-mode . easy-variable-setter)))
                 'easy-variable-mode))
            (should (boundp 'easy-variable-mode))
            (should (fboundp 'easy-variable-mode))
            (should (eq (easy-variable-mode 1) 'custom-on))
            (should (eq easy-variable-setter-arg t))
            (should-not (easy-variable-mode -1))
            (should-not easy-variable-mode)
            (should-not easy-variable-setter-arg))
        (when (boundp 'easy-variable-mode)
          (makunbound 'easy-variable-mode))
        (when (fboundp 'easy-variable-mode)
          (fmakunbound 'easy-variable-mode))
        (when (fboundp 'easy-variable-setter)
          (fmakunbound 'easy-variable-setter))))))

(provide 'emacs-easy-mmode-test)

;;; emacs-easy-mmode-test.el ends here
