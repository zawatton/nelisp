;;; emacs-redisplay-mode-line-test.el --- ERT for mode-line format (Doc 06 E2)  -*- lexical-binding: t; -*-

;;; Commentary:

;; Separate from emacs-redisplay-test.el so the expanded mode-line %-spec
;; coverage runs independently.

;;; Code:

(require 'ert)
(require 'emacs-redisplay)

(ert-deftest emacs-redisplay-mode-line-test/format-specs ()
  "mode-line %l %c %p %n %% %b specs expand correctly (Doc 06 E2)."
  (let ((b (nelisp-ec-generate-new-buffer "*ml*")))
    (unwind-protect
        (progn
          (let ((nelisp-ec--current-buffer b))
            (nelisp-ec-insert "ab\ncd")
            (nelisp-ec-goto-char 5))
          (should (equal "L2 C1"
                         (emacs-redisplay--mode-line-format-to-string "L%l C%c" b)))
          (should (equal "*ml*"
                         (emacs-redisplay--mode-line-format-to-string "%b" b)))
          (should (equal "100%"
                         (emacs-redisplay--mode-line-format-to-string "100%%" b)))
          (should (equal ""
                         (emacs-redisplay--mode-line-format-to-string "%n" b)))
          (should (stringp
                   (emacs-redisplay--mode-line-format-to-string "%p" b))))
      (when (fboundp 'nelisp-ec-kill-buffer) (nelisp-ec-kill-buffer b)))))

(provide 'emacs-redisplay-mode-line-test)
;;; emacs-redisplay-mode-line-test.el ends here
