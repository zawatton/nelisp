;;; emacs-redisplay-header-line-test.el --- ERT for header-line + cursor-type (Doc 06 E6)  -*- lexical-binding: t; -*-

;;; Commentary:

;; Doc 06 E6: `header-line-format' (reusing the mode-line %-spec machinery) and
;; `cursor-type' accessors.  Separate file (the main emacs-redisplay-test.el is
;; pre-broken by unrelated dirty edits).

;;; Code:

(require 'ert)
(require 'emacs-redisplay)

(ert-deftest emacs-redisplay-header-line-test/default-off ()
  "By default a buffer has no header line and a box cursor (Doc 06 E6)."
  (let ((b (nelisp-ec-generate-new-buffer "hl-default")))
    (unwind-protect
        (progn
          (should-not (emacs-redisplay--header-line-format b))
          (should-not (emacs-redisplay--header-line-enabled-p b))
          (should (eq 'box (emacs-redisplay--cursor-type b))))
      (when (fboundp 'nelisp-ec-kill-buffer) (nelisp-ec-kill-buffer b)))))

(ert-deftest emacs-redisplay-header-line-test/format-reuses-mode-line-specs ()
  "Header-line rendering uses the same %-spec vocabulary as the mode line."
  (let ((b (nelisp-ec-generate-new-buffer "hl-fmt")))
    (unwind-protect
        (progn
          (should (equal "hl-fmt"
                         (emacs-redisplay--header-line-format-to-string "%b" b)))
          (should (equal "100%"
                         (emacs-redisplay--header-line-format-to-string
                          "100%%" b))))
      (when (fboundp 'nelisp-ec-kill-buffer) (nelisp-ec-kill-buffer b)))))

(ert-deftest emacs-redisplay-header-line-test/buffer-local-values ()
  "Buffer-local `header-line-format' / `cursor-type' are honored (Doc 06 E6)."
  (let ((b (nelisp-ec-generate-new-buffer "hl-local")))
    (unwind-protect
        (progn
          (emacs-buffer-set-buffer-local-value 'header-line-format b "H:%b")
          (emacs-buffer-set-buffer-local-value 'cursor-type b 'bar)
          (should (equal "H:%b" (emacs-redisplay--header-line-format b)))
          (should (emacs-redisplay--header-line-enabled-p b))
          (should (equal "H:hl-local"
                         (emacs-redisplay--header-line-format-to-string
                          (emacs-redisplay--header-line-format b) b)))
          (should (eq 'bar (emacs-redisplay--cursor-type b))))
      (when (fboundp 'nelisp-ec-kill-buffer) (nelisp-ec-kill-buffer b)))))

(provide 'emacs-redisplay-header-line-test)
;;; emacs-redisplay-header-line-test.el ends here
