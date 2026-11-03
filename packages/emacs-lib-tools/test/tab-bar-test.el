;;; tab-bar-test.el --- Tests for the minimal tab-bar -*- lexical-binding: t; -*-

;; Doc 11 M3: workspace tab-bar with per-tab window configuration.

;;; Code:

(require 'ert)
(require 'emacs-window)
(require 'tab-bar)

(defmacro tab-bar-test--with-fresh-world (&rest body)
  "Run BODY with clean nelisp-ec / emacs-window / tab-bar state."
  (declare (indent 0) (debug (body)))
  `(let ((nelisp-ec--buffers nil)
         (nelisp-ec--current-buffer nil)
         (emacs-window--id-counter 0)
         (emacs-window--root nil)
         (emacs-window--selected nil)
         (tab-bar--tabs nil)
         (tab-bar--selected-index 0))
     ,@body))

(ert-deftest tab-bar-new-and-switch-tabs ()
  (tab-bar-test--with-fresh-world
    (tab-bar--ensure-tabs)
    (should (= 1 (length (tab-bar-tabs))))
    (tab-bar-new-tab)
    (should (= 2 (length (tab-bar-tabs))))
    (should (= 1 (tab-bar-current-tab-index)))
    ;; next wraps around back to the first tab
    (tab-bar-switch-to-next-tab)
    (should (= 0 (tab-bar-current-tab-index)))
    (tab-bar-switch-to-prev-tab)
    (should (= 1 (tab-bar-current-tab-index)))))

(ert-deftest tab-bar-close-keeps-last-tab ()
  (tab-bar-test--with-fresh-world
    (tab-bar--ensure-tabs)
    (tab-bar-new-tab)
    (should (= 2 (length (tab-bar-tabs))))
    (tab-bar-close-tab)
    (should (= 1 (length (tab-bar-tabs))))
    ;; the last remaining tab is kept (daily-driver guardrail)
    (tab-bar-close-tab)
    (should (= 1 (length (tab-bar-tabs))))))

(ert-deftest tab-bar-tabs-carry-window-layout ()
  (tab-bar-test--with-fresh-world
    (let ((b1 (nelisp-ec-generate-new-buffer "t-1"))
          (b2 (nelisp-ec-generate-new-buffer "t-2")))
      ;; tab 1: a single window showing b1
      (emacs-window-set-window-buffer (emacs-window-selected-window) b1)
      (tab-bar--ensure-tabs)
      ;; tab 2: split into two windows
      (tab-bar-new-tab)
      (emacs-window-set-window-buffer (emacs-window-split-window) b2)
      (should (= 2 (length (emacs-window-window-list))))
      ;; switching back to tab 1 restores its single-window layout
      (tab-bar-select-tab 1)
      (should (= 1 (length (emacs-window-window-list))))
      ;; switching to tab 2 restores the two-window layout
      (tab-bar-select-tab 2)
      (should (= 2 (length (emacs-window-window-list)))))))

(provide 'tab-bar-test)

;;; tab-bar-test.el ends here
