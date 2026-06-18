;;; emacs-calc-test.el --- ERT for emacs-calc  -*- lexical-binding: t; -*-

;;; Commentary:

;; RPN calculator tests: the pure string evaluator, the buffer-backed stack
;; operators, rendering, underflow, and install.

;;; Code:

(require 'ert)
(require 'emacs-calc)

(ert-deftest emacs-calc-test/eval-rpn ()
  (should (= 5 (emacs-calc-eval "2 3 +")))
  (should (= 8 (emacs-calc-eval "10 2 -")))
  (should (= 20 (emacs-calc-eval "2 3 + 4 *")))
  (should (= 5 (emacs-calc-eval "20 4 /")))
  (should (= 14 (emacs-calc-eval "2 3 4 * +")))     ; 2 + (3 * 4)
  (should (null (emacs-calc-eval ""))))

(ert-deftest emacs-calc-test/stack-operators ()
  (emacs-calc-reset)
  (emacs-calc-push 2)
  (emacs-calc-push 3)
  (emacs-calc-plus)
  (should (= 5 (emacs-calc-top)))
  (emacs-calc-push 4)
  (emacs-calc-times)
  (should (= 20 (emacs-calc-top)))
  (emacs-calc-push 5)
  (emacs-calc-minus)
  (should (= 15 (emacs-calc-top))))               ; 20 - 5

(ert-deftest emacs-calc-test/underflow-signals ()
  (emacs-calc-reset)
  (emacs-calc-push 1)
  (should-error (emacs-calc-plus)))

(ert-deftest emacs-calc-test/buffer-and-render ()
  (emacs-calc-reset)
  (let ((buf (emacs-calc)))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (should (eq major-mode 'calc-mode)))
          (emacs-calc-push 7)
          (emacs-calc-push 11)
          (with-current-buffer buf
            (should (string-match-p "1:  11" (buffer-string)))   ; top of stack
            (should (string-match-p "2:  7" (buffer-string)))))
      (kill-buffer buf))))

(ert-deftest emacs-calc-test/install ()
  (emacs-calc-install)
  (should (fboundp 'calc))
  (should (fboundp 'calc-eval))
  (should (= 5 (calc-eval "2 3 +"))))

(provide 'emacs-calc-test)

;;; emacs-calc-test.el ends here
