;;; standalone-boot-fboundp-check.el --- Optional provider visibility -*- lexical-binding: t; -*-
;; Run before requiring any optional provider.
(let ((failures 0) (checks 0))
  (dolist (name '(make-pipe-process process-mark set-process-buffer get-buffer-process))
    (setq checks (1+ checks))
    ;; Source boot already supplies these two prelude fallbacks. Keep their
    ;; function cells, while never replacing them with process-buffer autoloads.
    (when (if (memq name '(make-pipe-process set-process-buffer))
              (and (fboundp name) (autoloadp (symbol-function name)))
            (fboundp name))
      (setq failures (1+ failures))
      (princ (format "BOOT-FBOUNDP unexpectedly bound %S\n" name))))
  (dolist (entry nelisp-boot-library-autoloads)
    (when (nth 3 entry)
      (setq checks (1+ checks))
      (let ((function (and (fboundp (car entry)) (symbol-function (car entry)))))
        (when (and (autoloadp function) (equal (nth 1 function) (nth 1 entry)))
          (setq failures (1+ failures))
          (princ (format "BOOT-FBOUNDP guarded autoload %S\n" (car entry)))))))
  (unless (eq (require 'nelisp-process-buffer) 'nelisp-process-buffer)
    (setq failures (1+ failures)))
  (setq checks (1+ checks))
  (when (fboundp 'nl-ffi-call)
    (dolist (name '(make-pipe-process process-mark set-process-buffer get-buffer-process))
      (setq checks (1+ checks))
      (unless (fboundp name) (setq failures (1+ failures)))))
  (princ (format "BOOT-FBOUNDP checks=%d failures=%d %s\n"
                 checks failures (if (= failures 0) "PASS" "FAIL")))
  (kill-emacs (if (= failures 0) 0 1)))
