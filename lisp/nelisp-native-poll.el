;;; nelisp-native-poll.el --- Frozen runtime poll -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
(let* ((gc (symbol-function 'garbage-collect))
       (remaining 0)
       (poll (lambda ()
               ;; Return a request to the native status edge. Raising inside
               ;; the evaluator callback can expose the caller's handler early.
               (let ((pending (and (boundp 'quit-flag) (symbol-value 'quit-flag)
                                   (not (and (boundp 'inhibit-quit) (symbol-value 'inhibit-quit))))))
                 (when pending (setq quit-flag nil))
                 ;; Quit is tested on every poll. Collect at the first poll
                 ;; and every 64 polls; allocator safepoints remain active.
                 ;; Unconditional full collection walked the retained compiler
                 ;; heap on each edge and exceeded the native 300-second cap.
                 (setq remaining (1- remaining))
                 (when (or pending (<= remaining 0))
                   (setq remaining 64)
                   (funcall gc))
                 (and pending t)))))
  (defun nelisp-bytecode-native-rooted-cfg-poll-function ()
    "Return the frozen evaluator callable for rooted cyclic edge polls."
    poll))

(provide 'nelisp-native-poll)
