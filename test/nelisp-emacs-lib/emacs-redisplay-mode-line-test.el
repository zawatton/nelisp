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

(ert-deftest emacs-redisplay-mode-line-test/s22-single-digit-percent ()
  (with-temp-buffer
    (insert (make-string 100 ?x))
    (let ((emacs-window--root nil) (emacs-window--selected nil)
          (emacs-window--id-counter 0))
      (let* ((win (emacs-window-selected-window))
             (emacs-redisplay--mode-line-window win)
             (emacs-redisplay--mode-line-end 50))
        (setf (emacs-window-buffer win) (current-buffer))
        (emacs-window-set-window-start win 2)
        (should (equal (emacs-redisplay--mode-line-format-to-string "%p" (current-buffer)) " 1%"))))))

(ert-deftest emacs-redisplay-mode-line-test/s22-simple-format-keeps-face ()
  (with-temp-buffer
    (setq-local mode-line-format " %b ")
    (let ((emacs-window--root nil) (emacs-window--selected nil)
          (emacs-window--id-counter 0))
      (let ((win (emacs-window-selected-window)))
        (setf (emacs-window-buffer win) (current-buffer))
        (let ((spans (emacs-redisplay-mode-line-spans win 80 (point-max))))
          (should (equal (mapconcat #'car spans "")
                         (concat " " (buffer-name) " ")))
          (should (equal (cdar spans) (emacs-redisplay-realize-face 'mode-line))))))))

(provide 'emacs-redisplay-mode-line-test)
;;; emacs-redisplay-mode-line-test.el ends here
