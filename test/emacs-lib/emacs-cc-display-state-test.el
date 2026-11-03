;;; emacs-cc-display-state-test.el --- minibuffer window state -*- lexical-binding: t; -*-

;;; Code:

(defconst emacs-cc-display-state-test--root
  (expand-file-name "../.." (file-name-directory (or load-file-name buffer-file-name))))

(dolist (directory '("packages/nelisp-emacs-foundation/src"
                     "packages/nelisp-emacs-buffer-core/src"
                     "packages/nelisp-emacs-text-core/src"
                     "packages/nelisp-emacs-core/src"
                     "packages/nelisp-regex/src"))
  (add-to-list 'load-path
               (expand-file-name directory emacs-cc-display-state-test--root)))

(require 'ert)
(require 'nelisp-emacs-compat)
(require 'emacs-window)
(require 'emacs-minibuffer)

(defun emacs-cc-display-state-test--load-window-fallback ()
  "Load the C-core window unit with its minibuffer function unbound."
  (load (expand-file-name "packages/nelisp-emacs-foundation/src/emacs-cc-window-1.el"
                          emacs-cc-display-state-test--root) nil t))

(defmacro emacs-cc-display-state-test--with-world (&rest body)
  "Run BODY with fresh custom buffer and window state."
  (declare (indent 0) (debug (body)))
  `(let ((nelisp-ec--buffers nil)
         (nelisp-ec--current-buffer nil))
     (unwind-protect
         (progn
           (emacs-window-reset)
           (emacs-minibuffer-reset)
           ,@body)
       (emacs-minibuffer-reset)
       (emacs-window-reset))))

(ert-deftest emacs-cc-display-state-minibuffer-selection-is-restored ()
  (let ((saved-function (and (fboundp 'minibuffer-selected-window)
                             (symbol-function 'minibuffer-selected-window))))
    (unwind-protect
        (progn
          (fmakunbound 'minibuffer-selected-window)
          (emacs-cc-display-state-test--load-window-fallback)
          (emacs-cc-display-state-test--with-world
            (let ((original (emacs-window-selected-window))
                  observed)
              (should-not (minibuffer-selected-window))
              (setq emacs-minibuffer--read-fn
                    (lambda (&rest _args)
                      (setq observed
                            (list (emacs-window-selected-window)
                                  (minibuffer-selected-window)
                                  emacs-minibuffer--depth))
                      "answer"))
              (should (equal "answer"
                             (emacs-minibuffer-read-from-minibuffer "Prompt: ")))
              (should (eq (car observed) emacs-minibuffer--window))
              (should (eq (cadr observed) original))
              (should (= (caddr observed) 1))
              (should (eq (emacs-window-selected-window) original))
              (should-not (minibuffer-selected-window))
              (should-not emacs-minibuffer--saved-window))))
      (if saved-function (fset 'minibuffer-selected-window saved-function)
        (fmakunbound 'minibuffer-selected-window)))))

(ert-deftest emacs-cc-display-state-nested-minibuffers-restore-each-window ()
  (let ((saved-function (and (fboundp 'minibuffer-selected-window)
                             (symbol-function 'minibuffer-selected-window))))
    (unwind-protect
        (progn
          (fmakunbound 'minibuffer-selected-window)
          (emacs-cc-display-state-test--load-window-fallback)
          (emacs-cc-display-state-test--with-world
            (let ((original (emacs-window-selected-window))
                  inner-seen outer-seen)
              (setq emacs-minibuffer--read-fn
                    (lambda (&rest _args)
                      (if (= emacs-minibuffer--depth 1)
                          (progn
                            (setq outer-seen (minibuffer-selected-window))
                            (emacs-minibuffer-read-from-minibuffer "Nested: ")
                            (setq outer-seen (list outer-seen
                                                   (minibuffer-selected-window)))
                            "outer")
                        (setq inner-seen (minibuffer-selected-window))
                        "inner")))
              (should (equal "outer"
                             (emacs-minibuffer-read-from-minibuffer "Outer: ")))
              (should (eq inner-seen original))
              (should (eq (car outer-seen) original))
              (should (eq (cadr outer-seen) original))
              (should (eq (emacs-window-selected-window) original))
              (should-not emacs-minibuffer--saved-window))))
      (if saved-function (fset 'minibuffer-selected-window saved-function)
        (fmakunbound 'minibuffer-selected-window)))))

(ert-deftest emacs-cc-display-state-nested-entry-from-another-window-restores-state ()
  (let ((saved-function (and (fboundp 'minibuffer-selected-window)
                             (symbol-function 'minibuffer-selected-window))))
    (unwind-protect
        (progn
          (fmakunbound 'minibuffer-selected-window)
          (emacs-cc-display-state-test--load-window-fallback)
          (emacs-cc-display-state-test--with-world
            (let* ((original (emacs-window-selected-window))
                   (other (emacs-window-split-window original nil 'right))
                   outer-before outer-after saved-after inner-seen)
              (setq emacs-minibuffer--read-fn
                    (lambda (&rest _args)
                      (if (= emacs-minibuffer--depth 1)
                          (progn
                            (setq outer-before (minibuffer-selected-window))
                            (emacs-window-select-window other)
                            (emacs-minibuffer-read-from-minibuffer "Nested: ")
                            (setq outer-after (minibuffer-selected-window)
                                  saved-after emacs-minibuffer--saved-window)
                            "outer")
                        (setq inner-seen (minibuffer-selected-window))
                        "inner")))
              (should (equal "outer"
                             (emacs-minibuffer-read-from-minibuffer "Outer: ")))
              (should (eq outer-before original))
              (should (eq inner-seen other))
              (should-not outer-after)
              (should (eq saved-after original))
              (should-not emacs-minibuffer--saved-window)
              (should-not emacs-minibuffer--window-selection-stack))))
      (if saved-function (fset 'minibuffer-selected-window saved-function)
        (fmakunbound 'minibuffer-selected-window)))))

(ert-deftest emacs-cc-display-state-error-exit-restores-selection ()
  (let ((saved-function (and (fboundp 'minibuffer-selected-window)
                             (symbol-function 'minibuffer-selected-window))))
    (unwind-protect
        (progn
          (fmakunbound 'minibuffer-selected-window)
          (emacs-cc-display-state-test--load-window-fallback)
          (emacs-cc-display-state-test--with-world
            (let ((original (emacs-window-selected-window)))
              (setq emacs-minibuffer--read-fn
                    (lambda (&rest _) (signal 'error '("injected reader error"))))
              (should-error (emacs-minibuffer-read-from-minibuffer "Prompt: "))
              (should (eq (emacs-window-selected-window) original))
              (should-not (minibuffer-selected-window))
              (should-not emacs-minibuffer--saved-window)
              (should (= emacs-minibuffer--depth 0)))))
      (if saved-function (fset 'minibuffer-selected-window saved-function)
        (fmakunbound 'minibuffer-selected-window)))))

(ert-deftest emacs-cc-display-state-dead-saved-window-restores-live-sibling ()
  (let ((saved-function (and (fboundp 'minibuffer-selected-window)
                             (symbol-function 'minibuffer-selected-window))))
    (unwind-protect
        (progn
          (fmakunbound 'minibuffer-selected-window)
          (emacs-cc-display-state-test--load-window-fallback)
          (emacs-cc-display-state-test--with-world
            (let* ((original (emacs-window-selected-window))
                   (sibling (emacs-window-split-window original nil 'right)))
              (emacs-window-select-window original)
              (setq emacs-minibuffer--read-fn
                    (lambda (&rest _args)
                      (emacs-window-delete-window original)
                      (should-not (minibuffer-selected-window))
                      "answer"))
              (should (equal "answer"
                             (emacs-minibuffer-read-from-minibuffer "Prompt: ")))
              (should-not (emacs-window-window-live-p original))
              (should (eq (emacs-window-selected-window) sibling))
              (should-not emacs-minibuffer--saved-window))))
      (if saved-function (fset 'minibuffer-selected-window saved-function)
        (fmakunbound 'minibuffer-selected-window)))))

(provide 'emacs-cc-display-state-test)
