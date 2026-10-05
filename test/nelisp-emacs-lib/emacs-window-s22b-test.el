;;; emacs-window-s22b-test.el --- Preserve split bridge ownership -*- lexical-binding: t; -*-
(require 'ert)
(require 'cl-lib)
(require 'emacs-window)

(ert-deftest emacs-window-s22b-reload-keeps-builtin-split-commands ()
  ;; GNU's real split commands stand in for the already installed bridge.
  ;; Reload the source in standalone mode, as the late bootstrap does.
  (let ((original-fboundp (symbol-function 'fboundp))
        (original-featurep (symbol-function 'featurep))
        (source (locate-library "emacs-window"))
        (frame (selected-frame))
        (width (frame-width)) (height (frame-height))
        (emacs-window--root nil) (emacs-window--selected nil))
    (unwind-protect
        (save-window-excursion
          (cl-letf (((symbol-function 'fboundp)
                     (lambda (symbol)
                       (or (eq symbol 'nl-write-file)
                           (funcall original-fboundp symbol))))
                    ((symbol-function 'featurep)
                     (lambda (feature &optional subfeature)
                       (or (eq feature 'emacs-window-builtins)
                           (funcall original-featurep feature subfeature))))
                    ((symbol-function 'split-window-right) (symbol-function 'split-window-right))
                    ((symbol-function 'split-window-below) (symbol-function 'split-window-below))
                    ((symbol-function 'other-window) (symbol-function 'other-window))
                    ((symbol-function 'delete-window) (symbol-function 'delete-window))
                    ((symbol-function 'delete-other-windows) (symbol-function 'delete-other-windows)))
            (load source nil t t)
            (set-frame-size frame 10 10)
            (should (string-match-p
                     "too small for splitting"
                     (cadr (should-error (split-window-right) :type 'error))))
            (set-frame-size frame 10 4)
            (should (string-match-p
                     "too small for splitting"
                     (cadr (should-error (split-window-below) :type 'error))))))
      (set-frame-size frame width height))))
