;;; emacs-redisplay-mode-line-test.el --- ERT for mode-line format (Doc 06 E2)  -*- lexical-binding: t; -*-

;;; Commentary:

;; Separate from emacs-redisplay-test.el so the expanded mode-line %-spec
;; coverage runs independently.

;;; Code:

(require 'ert)
(require 'emacs-redisplay)

(ert-deftest emacs-redisplay-mode-line-test/shared-defaults-and-local-nil ()
  (let ((buffer (nelisp-ec-generate-new-buffer "*plain-defaults*"))
        (sym (make-symbol "g2-default")))
    (unwind-protect
        (progn
          (set-default sym '("Shared default"))
          (should (equal (emacs-redisplay--mode-line-format-to-string sym buffer)
                         "Shared default"))
          (emacs-buffer-set-buffer-local-value sym buffer nil)
          (should (equal (emacs-redisplay--mode-line-format-to-string sym buffer) "")))
      (nelisp-ec-kill-buffer buffer))))

(ert-deftest emacs-redisplay-mode-line-test/newline-boundary-cursor ()
  (let* ((matrix (emacs-redisplay--make-glyph-matrix
                  :height 3 :width 80
                  :rows (vector
                         (emacs-redisplay--make-glyph-row :start-pos 1 :end-pos 3 :glyphs [] :used 0)
                         (emacs-redisplay--make-glyph-row :start-pos 3 :end-pos 4 :glyphs [] :used 0)
                         (emacs-redisplay--make-glyph-row :start-pos 4 :end-pos 4 :glyphs [] :used 0)))))
    (should (equal (emacs-redisplay--cursor-for-point matrix 3) '(1 . 0)))
    (should (equal (emacs-redisplay--cursor-for-point matrix 4) '(2 . 0)))))

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

(ert-deftest emacs-redisplay-mode-line-test/paint-runs-preserve-face-boundaries ()
  (with-temp-buffer
    (setq-local mode-line-format
                '("ab" "cd" (:propertize "EF" face bold) "gh"))
    (let ((emacs-window--root nil) (emacs-window--selected nil)
          (emacs-window--id-counter 0))
      (let ((win (emacs-window-selected-window)))
        (setf (emacs-window-buffer win) (current-buffer))
        (cl-letf (((symbol-function 'emacs-redisplay-realize-face) #'identity))
          (let ((spans (emacs-redisplay-mode-line-spans win 80 (point-max))))
            (should (equal (mapcar #'car spans) '("abcd" "EF" "gh")))
            (should (equal (cdar spans) (cdr (nth 2 spans))))
            (should-not (equal (cdar spans) (cdr (nth 1 spans))))))))))

(provide 'emacs-redisplay-mode-line-test)
;;; emacs-redisplay-mode-line-test.el ends here
