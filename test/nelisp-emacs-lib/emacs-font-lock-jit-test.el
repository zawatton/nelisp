;;; emacs-font-lock-jit-test.el --- ERT for jit-lock fontifier dispatch (Doc 06 E5)  -*- lexical-binding: t; -*-

;;; Commentary:

;; Doc 06 E5: registered jit-lock functions must actually RUN over the
;; refontified region on the incremental flush path, not merely be remembered.
;; Separate from emacs-font-lock-builtins-test.el so this coverage runs
;; independently of that file's pre-existing failures.

;;; Code:

(require 'ert)
(require 'emacs-font-lock)

(ert-deftest emacs-font-lock-jit-test/register-unregister ()
  "Registration adds/removes a function from the jit-lock function list."
  (let ((f (lambda (_s _e) nil)))
    (unwind-protect
        (progn
          (emacs-font-lock-jit-lock-register f)
          (should (memq f (emacs-font-lock-jit-lock-functions)))
          (emacs-font-lock-jit-lock-unregister f)
          (should-not (memq f (emacs-font-lock-jit-lock-functions))))
      (emacs-font-lock-jit-lock-unregister f))))

(ert-deftest emacs-font-lock-jit-test/run-jit-functions-count-and-guard ()
  "`--run-jit-functions' runs each registered fontifier with (START END),
counts the ones that ran cleanly, and guards errors so one bad fontifier
cannot abort the rest (Doc 06 E5)."
  (let* ((seen nil)
         (good (lambda (s e) (push (cons s e) seen)))
         (bad  (lambda (_s _e) (error "boom"))))
    (unwind-protect
        (progn
          (emacs-font-lock-jit-lock-register good)
          (emacs-font-lock-jit-lock-register bad)
          ;; good runs (counted); bad errors (guarded, not counted) → 1.
          (should (= 1 (emacs-font-lock--run-jit-functions 1 3)))
          (should (member '(1 . 3) seen)))
      (emacs-font-lock-jit-lock-unregister good)
      (emacs-font-lock-jit-lock-unregister bad))))

(ert-deftest emacs-font-lock-jit-test/flush-drives-registered-functions ()
  "The incremental flush invokes registered jit-lock functions over the
dirty interval — the contract `jit-lock-register' promises (Doc 06 E5)."
  (let* ((calls nil)
         (probe (lambda (s e) (push (cons s e) calls))))
    (unwind-protect
        (let ((b (nelisp-ec-generate-new-buffer "fl-jit")))
          (let ((nelisp-ec--current-buffer b)) (nelisp-ec-insert "abcdef"))
          (emacs-font-lock-jit-lock-register probe)
          (emacs-font-lock-mark-dirty-region 2 4 b)
          (emacs-font-lock-flush-pending b)
          (should (member '(2 . 4) calls))
          ;; The flush also cleared the dirty marker (existing contract).
          (should-not (emacs-font-lock-pending-dirty-region b)))
      (emacs-font-lock-jit-lock-unregister probe))))

(provide 'emacs-font-lock-jit-test)
;;; emacs-font-lock-jit-test.el ends here
